{-# LANGUAGE NamedFieldPuns #-}

{-
This file contains all things related to parsing STG according to the following grammar:

prog       ::= <def>*  -- is an empty program still a valid program? I think so.

<def>      ::= (<objDef> | <dataDef>) ";" -- currently expecting ";" always.  Can modify this easily

<objDef>   ::= "FUN" "(" <var>+ -> <expr> ")"
             |  "PAP" "(" <var> <atom>+ ")"
             |  "CON" "(" <con> <atom>* ")"
             |  "THUNK" <expr>
             |  "ERROR"  (aka BLACKHOLE)

<dataDef>  ::= "data" ["unboxed"] <typeCon> "=" <dataCons>

<typeCon>  ::= <conName> (<varName>)*

<dataCons> ::= <dataCon> ("|" <dataCon>)*

<dataCon>  ::= <conName> (<monoType>)*

<monoType> ::= <varName>
             | <funType>
             | <conType>

<funType>  ::= "(" <monoType> ("->" <monoType>)+ ")"

<conType>  ::= <conName>
             | <qualCon>

<qualCon>  ::= "(" <conName> (<monoType>)* ")" -- qualified Constructor (e.g. Maybe a)

<expr>     ::= <atom>
             | <funCall>
             | <primop> <atom>+
             | "let" "{" <letDefs> "}" "in" <expr>
             | "case" <expr> "of" "{" <alts> "}"

<funCall>  ::= <var> <atom>+

<primop>   ::= "iplus#"  -- subject to change?
             | "isub#"
             | "imul#"
             | "idiv#"
             | "ieq#"

<letDefs>  ::= <objDef> (";" <objDef>)*

<alts>     ::= <alt> (";" <alt>)*

<alt>      ::= <conName> <var>* "->" <expr>
             | <var> "->" <expr>

<atom>     ::= <literal>
             | <varName>

<literal>  ::= <int>
             | <float> -- subject to change? boolean vals?

<int>      :: <num>+

<float>    :: <num>+ "." <num>+  -- haskell follows similar rule: no implicit digits

<conName>  ::= <upperChar> (<idChar>)*

<varName>  ::= <lowerChar> (<idChar>)*

<idChar>   ::= <alphaNum>
             | "#"
          -- | [others?]

-}


module Parser
(
 Parsed(..),
 Comment,
 parse,
 fromParsed,
 parseWithComments,
) where

import Tokenizer
import ParserComb
import AST
import ADT
import Data.List (groupBy)
import PPrint
import Data.Char (isNumber)
import Options (reWriteSTG) -- controls grammar of case/alts expressions
    
type Comment = String

parse :: [Token] -> ([TyCon], [Obj ()]) -- (ObjDefs, DataDefs)
parse = splitDefs . fst . head . prog 

parseWithComments :: [Token] -> [Either Comment (Def ())]
parseWithComments = fst . head . progWithComments

progWithComments :: Parser Token [Either Comment (Def ())]
progWithComments = 
   (many' $ orEx commentP (defP `thenx` optP semiP))
   `thenx` tokcutP "Expected semicolon or EOF after object definition" eofP

prog :: Parser Token [Def ()]
prog = sepByP' defP semiP `thenx`
       tokcutP "Expected semicolon or EOF after object definition" eofP

defP :: Parser Token (Def ())
defP = orExList [objDefP `using` ObjDef, tyConP `using` DataDef]


splitDefs :: [Def a] -> ([TyCon], [Obj a])
splitDefs =
  let f x (ts, os) = case x of
        (DataDef t) -> (t:ts, os)
        (ObjDef o)  -> (ts, o:os)
  in foldr f ([],[])

parseDBG :: String -> [Parsed [Def()] Token]
parseDBG = fst . head . progDBG . tokenize

progDBG :: Parser Token [Parsed [Def()] Token]
progDBG = many' (orExList [sepByP' defP semiP `using` Parsed,
                           notP eofP `using` Unparsed])
          `thenx` (tokcutP "EOF not found" eofP)

groupParsed :: [Parsed a b] -> [[Parsed a b]]
groupParsed =
  let f (Parsed _) (Parsed _)     = True
      f (Unparsed _) (Unparsed _) = True
      f _ _                       = False
  in groupBy f

    

-- uncurried cons is used on several occasions to combine the results of
-- ordered parsers
cons = uncurry (:)
      
----------------------------- Parsers for Tokens ---------------------------


tokP :: Token -> Parser Token Token
tokP _ [] = reject []
tokP t1 (t2:inp) =
  case (t1,t2) of
   (TokNum _ _   , TokNum _ _  ) -> accept t2 inp
   (TokId _ _    , TokId _ _   ) -> accept t2 inp
   (TokCon _ _   , TokCon _ _  ) -> accept t2 inp
   (TokPrim _ _  , TokPrim _ _ ) -> accept t2 inp
   (TokRsv x _   , TokRsv y _  ) -> if x == y then accept t2 inp else reject inp
   (TokEOF _ _   , TokEOF _ _  ) -> accept t2 inp
   (TokWht _ _ _ , TokWht _ _ _) -> accept t2 inp
   _                             -> reject inp

-- hacky way of ignoring warnings when simply trying to match equality in Token
-- data constructors (don't want to make instance of Eq for this)
tokP1 :: (a -> Token) -> Parser Token Token
tokP1 t = tokP $ t undefined
tokP2 :: (a -> b -> Token) -> Parser Token Token
tokP2 t = tokP1 $ t undefined
tokP3 :: (a -> b -> c -> Token) -> Parser Token Token
tokP3 t = tokP2 $ t undefined

-- Match a TokRsv with string s
rsvP :: String -> Parser Token Token
rsvP s = tokP1 $ TokRsv s

subHash [] = []
subHash ('#':xs) = "_h" ++ subHash xs
subHash (x:xs) = x : subHash xs

-- Match constructor token, accept its String
conNameP :: Parser Token String
conNameP = tokP2 (TokCon) `using` (subHash.tks)

-- Match variable token, accept its String
varNameP :: Parser Token String
varNameP = tokP2 (TokId) `using` (subHash . tks)

-- Match numeric literals
intP :: Parser Token Int
intP = satisfy f `using` (read . tks)
  where f (TokNum s _) = all isNumber s
        f _ = False

fltP :: Parser Token Float
fltP = satisfy f `using` (read . tks)
  where f (TokNum s _) | '.' `elem` s =
                         let (w, d:ds) = break (== '.') s
                         in all isNumber w && all isNumber ds
        f _ = False


-- Match Primop Token, accept Primop
primP :: Parser Token Primop
primP = tokP2 (TokPrim) `using` getPrimop
  where getPrimop (TokPrim s _) =
          snd . head $ filter ((== s).fst) primopTable
        getPrimop _ = error "Parser.primP"

-- match common reserved symbols/words
dataP = rsvP "data"
lparenP = rsvP "("
rparenP = rsvP ")"
lbraceP = rsvP "{"
rbraceP = rsvP "}"
arrowP = rsvP "->"
eqP = rsvP "="
barP = rsvP "|"
semiP = rsvP ";"

-- match EOF Token
eofP = tokP2 (TokEOF)


-- match comment token, pull out comment
commentP :: Parser Token Comment
commentP = satisfy f `using` tks
    where f TokWht{cmnt} = cmnt
          f _ = False
                           
    
-- Given a parser, match parens surrounding what it would match
inparensP :: Parser Token v -> Parser Token v
inparensP p = xthenx lparenP p rparenP

-- similar, for braces,
inbracesP :: Parser Token v -> Parser Token v
inbracesP p = xthenx lbraceP p rbraceP

-- specialized cutp
tokcutP msg p inp = cutP (show $
                  text "Parse error:" <+>
                  (if not $ null inp then pprint $ head inp else text "end of input:") $+$
                  text msg)
                  p inp

        
---------------------------- Object Parsing -------------------------------

-- Parse an Object definition (no semicolon) as Obj ()
objDefP :: Parser Token (Obj ())
objDefP =
  varNameP >>> \name ->
  eqP >>> \_ ->
  tokcutP "Expected object definition"
  (orExList
   [funP, papP, conP, thunkP, errorP]) >>> \obj -> -- partially applied Obj constructor
                                          accept $ obj name -- apply name to constructor


-- Parse an Object definition (with semicolon) as Obj ()
objDefSC :: Parser Token (Obj ())
objDefSC = objDefP `thenx` semiP -- semicolon at end

-- helper abstracts the TokRsv "CAPS" lparenP [definition] rparenP pattern
objPat name p = rsvP name `xthen`
                (inparensP $ tokcutP "Expected valid object definition in parentheses" p)


-- parse a function definition,
-- accept partially applied FUN constructor
funP :: Parser Token (String -> Obj ())
funP = objPat "FUN" $
       tokcutP "Expected 1 or more valid function variables" $
       some' varNameP >>> \v1 ->
       tokcutP "Expected '->' to initiate function body" $
       arrowP >>> \_ ->
       tokcutP "Expected valid expression in function body"
       exprP >>> \v2 ->
                  accept $ FUN () v1 v2


-- parse a partially applied function application,
-- accept partially applied PAP constructor
papP :: Parser Token (String -> Obj ())
papP = objPat "PAP" $
       tokcutP "Expected a variable followed by one or more atoms to form a PAP"
       varNameP >>> \f ->
       many' atomP >>> \atoms ->
                        accept $ PAP () f (map (\a->EAtom () a) atoms)


-- parse a constructor application,
-- accept partially applied CON constructor
conP :: Parser Token (String -> Obj ())
conP = objPat "CON" $
       tokcutP "Expected a valid constructor name"
       conNameP >>> \nm ->
       many' atomP >>> \atoms ->
                        accept $ CON () nm (map (\a->EAtom () a) atoms)


-- parse a thunk object,
-- accept a partially applied THUNK constructor
thunkP :: Parser Token (String -> Obj ())
thunkP = objPat "THUNK" $
         tokcutP "Expected valid expression in thunk body"
         exprP >>> \e ->
                    accept $ THUNK () e


-- parse an Error/Blackhole object,
-- accept partially applied BLACKHOLE constructor
errorP :: Parser Token (String -> Obj ())
errorP = rsvP "ERROR" >>> \_ -> accept $ BLACKHOLE ()


-- parse an expression, accept an Expr () object
exprP :: Parser Token (Expr ())
exprP = orExList
        [eFCallP, eAtomP, ePrimopP, eLetP, eCaseP, inparensP exprP]


-- parse an expression argument to a function call or primop
-- the preference in this list is different from exprP to
-- accept EAtoms before EFCalls/EPrimops. This makes function
-- application tightly bound as in Haskell.
-- Not in use currently. EFCalls and EPrimops still look for
-- atoms but map EAtom across
exprArgP :: Parser Token (Expr ())
exprArgP = orExList
           [eAtomP, eLetP, eCaseP, eFCallP, ePrimopP, inparensP exprP]

-- parse an atom expression
eAtomP :: Parser Token (Expr ())
eAtomP = atomP `using` (EAtom ())


-- parse a function call expression
eFCallP :: Parser Token (Expr ())
eFCallP =
  varNameP >>> \fn ->
  some' eAtomP >>> \args ->
                   accept $ EFCall () fn args

-- parse a primitive operation expression (e.g. isub# 1# 2#)
ePrimopP :: Parser Token (Expr ())
ePrimopP =
  primP >>> \op ->
  tokcutP "Expected one or more atoms as arguments to a primop" $
  some' atomP >>> \args ->
                   accept $ EPrimop () op $ map (EAtom ()) args

-- parse a let expression
eLetP :: Parser Token (Expr ())
eLetP =
  rsvP "let" >>> \_ ->
  tokcutP "Expected a left brace to open the declaration block of a let expr"
  lbraceP >>> \_ ->
  tokcutP "Expected one or more declarations for a let expr" $
  sepByP' objDefP semiP >>> \defs ->
  tokcutP "Expected a right brace to close the declaration block of a let expr"
  rbraceP >>> \_ ->
  tokcutP "Expected 'in' Token to open a let expr's sub expression" $
  rsvP "in" >>> \_ ->
  exprP >>> \exp ->
             accept $ ELet () defs exp


-- parse a case expression
eCaseP :: Parser Token (Expr ())
eCaseP =
    rsvP "case" >>> \_ ->
    exprP >>> \exp ->
    tokcutP "Expected 'of' Token to close the scrutinee of a case expr" $
    rsvP "of" >>> \_ ->    
    altsP >>> \alts -> -- scrutinee binding
                accept $ ECase () exp $ alts


-- parse an Alts section in a case expression, accept an Alts object
-- (partially applied: scrutinee binding is parsed by eCaseP
altsP :: Parser Token (Alts ())
altsP = 
  let
    name = "alts" --error "this alts not given a name!"
    scrtP | reWriteSTG = emptyP `using` const (EAtom () $ Var "parsed_rewrite_stg")
            | otherwise = tokcutP "Expected variable binding the case scrutinee" $
                          eAtomP
  in
    scrtP >>> \scrt ->
    tokcutP "Expected left brace to open the alt block of a case expr"
    lbraceP >>> \_ ->
    tokcutP "Expected one or more alts separated by semicolons" $
    sepByP' altP semiP >>> \alts ->
    tokcutP "Expected right brace to close the alt block of a case expr"
    rbraceP >>> \_ ->                
                  accept $ Alts () alts name scrt


-- parse a case expression alternative, accept an Alt object
altP :: Parser Token (Alt ())
altP =
  let aconNameP = orExList [intP `using` show, conNameP] in
   orExList [varNameP >>> \v ->
                          accept $ ADef () v,
             aconNameP >>> \con ->
             many' varNameP >>> \vs ->
                                 accept $ ACon () con vs
            ] >>> \alt ->
   tokcutP "Expected a '->' symbol after an alt's pattern"
   arrowP >>> \_ ->
   exprP >>> \exp ->
              accept $ alt exp -- fully apply Alt constructor to Expr


-- parse an atom (variable or literal)
atomP :: Parser Token Atom
atomP = orExList [
  varNameP `using` Var,
  intP `using` LitI,
  --longP `using` LitL,
  fltP `using` LitF,
  --dblP `using` LitD,
  conNameP `using` LitC
  ]

---------------------------- DataDef parsing ---------------------------


-- parse a data definition accepting the TyCon object that describes it
tyConP :: Parser Token TyCon
tyConP =
  rsvP "data" >>> \_ ->

  optP (rsvP "unboxed") >>> \b ->
  
  tokcutP "Expected valid constructor name in datatype declaration" $
  conNameP >>> \con ->
  
  many' varNameP >>> \tyvars ->

  tokcutP "Expected '=' Token to bind datatype declaration"
  eqP >>> \_ ->

  tokcutP "Expected one or more data constructor definitions separated by '|'" $
  sepByP dataConP barP >>> \dcs ->
                            let boxed = maybe True (const False) b
                            in accept $ TyCon boxed con tyvars dcs


-- parse a data constructor as a DataCon object
dataConP :: Parser Token DataCon
dataConP =
  conNameP >>> \con ->

  isNextP (orExList [semiP, eofP, barP]) >>> \b ->

  -- this feels hacky and wrong, but allows for a slightly better error message
  if b then accept $ DataCon con []

  else tokcutP "Expected valid monotypes in data constructor" $
       some' monoTypP >>> \mTypes ->
                           accept $ DataCon con mTypes


-- parse a monotype in a data constructor as a Monotype object
monoTypP :: Parser Token Monotype
monoTypP = orExList [mVarP, mFunP, mConP, inparensP monoTypP]


-- parse a variable as an MVar Monotype (e.g. 'a' in Just a)
mVarP :: Parser Token Monotype
mVarP = varNameP `using` MVar


-- parse a Monotype function as an MFun object (e.g. a->b)
mFunP :: Parser Token Monotype
mFunP =
  inparensP $
  monoTypP >>> \m ->
  peekP arrowP >>> \_ ->
  tokcutP "Expected valid monotype(s) following '->' token in data constructor" $
  some' (arrowP `xthen` monoTypP) >>> \ms ->
                                       accept $ foldr1 MFun (m:ms)


-- parse a type constructor in a monotype as an MCon (e.g. 'Tree a' in Branch (Tree a) (Tree a) )
mConP :: Parser Token Monotype
mConP =
  let berr = error "Boxity not set in MCon" in
   orExList [conNameP >>> \con ->
                           accept $ MCon berr con [],
             lparenP >>> \_ ->
             conNameP >>> \con ->
             many' monoTypP >>> \mts ->
             rparenP >>> \_ ->
                          accept $ MCon berr con mts]
