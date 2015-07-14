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

{-
notes 6.26
literal integers are tentatively being considered data constructors
for their corresponding unboxed values. For parsing, this means that
a TokInt is valid in place of XX in
case e0 of { XX -> e1 }
num = CON (XX)
as of this writing, there is some contradiction in the Prelude.stg
file being used for testing.
case expressions are used as above but so too are
one = CON(I 1) and so on though I as a data constructor is defined
as
data Int = I Int#
implying only unboxed ints can be used to construct it.
-}

module Parser
(
  Parsed(..),
  parse,
  fromParsed,
) where

import Tokenizer
import AST
import ADT
import qualified Data.Map as Map
import Data.List (groupBy)
import PPrint



instance (PPrint a, PPrint b) => PPrint (Parsed a b) where
  toDoc (Parsed a) =
    (lcomment $ text "PARSED:") $+$ toDoc a
  toDoc (Unparsed b) =
    bcomment $ text "UNPARSED:" $+$ toDoc b


-- make it easy to group parsed and unparsed input together for later filtration
data Parsed a b = Parsed a | Unparsed b deriving (Show)

-- type pseudonym for Parsers
type Parser inp val = [inp] -> [(val, [inp])]

parse :: [Token] -> ([TyCon], [Obj ()]) -- (ObjDefs, DataDefs)
parse = splitDefs . fst . head . prog

prog :: Parser Token [Def ()]
prog = tokcutP
       "Expected top level object and data definitions separated by semicolons" $
       sepByP' defP semiP `thenx` (tokcutP "EOF not found" eofP)

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

fromParsed :: [Parsed a b] -> ([a],[b])
fromParsed =
  let f x (ps, us) = case x of
        (Parsed p)   -> (p:ps, us)
        (Unparsed u) -> (ps, u:us)
  in foldr f ([],[])


groupParsed :: [Parsed a b] -> [[Parsed a b]]
groupParsed =
  let f (Parsed _) (Parsed _)     = True
      f (Unparsed _) (Unparsed _) = True
      f _ _                       = False
  in groupBy f

    

-- uncurried cons is used on several occasions to combine the results of ordered parsers
cons = uncurry (:)
      
----------------------------- Parsers for Tokens ---------------------------


tokP :: Token -> Parser Token Token
tokP _ [] = reject []
tokP t1 (t2:inp) =
  case (t1,t2) of
   (TokInt _ _  , TokInt _ _ ) -> accept t2 inp
   (TokFlt _ _  , TokFlt _ _ ) -> accept t2 inp
   (TokId _ _   , TokId _ _  ) -> accept t2 inp
   (TokCon _ _  , TokCon _ _ ) -> accept t2 inp
   (TokPrim _ _ , TokPrim _ _) -> accept t2 inp
   (TokRsv x _  , TokRsv y _ ) -> if x == y then accept t2 inp else reject inp
   (TokEOF _    , TokEOF _   ) -> accept t2 inp
   _                           -> reject inp


-- hacky way of ignoring warnings when simply trying to match equality in Token
-- data constructors (don't want to make instance of Eq for this)
tokP1 :: (a -> Token) -> Parser Token Token
tokP1 t = tokP $ t undefined
tokP2 :: (a -> b -> Token) -> Parser Token Token
tokP2 t = tokP1 $ t undefined

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
varNameP = tokP2 (TokId) `using` (subHash.tks)

-- Match Integer Token, accept Int
intP :: Parser Token Int
intP = tokP2 (TokInt) `using` ivl

-- Match Float Token, accept Float
fltP :: Parser Token Float
fltP = tokP2 (TokFlt) `using` fvl

-- Match Primop Token, accept Primop
primP :: Parser Token Primop
primP = tokP2 (TokPrim) `using` getPrimop
  where getPrimop (TokPrim s _) =
          snd . head $ filter ((== s).fst) primopTable

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
eofP = tokP1 (TokEOF)

-- Given a parser, match parens surrounding what it would match
inparensP :: Parser Token v -> Parser Token v
inparensP p = xthenx lparenP p rparenP

-- similar, for braces,
inbracesP :: Parser Token v -> Parser Token v
inbracesP p = xthenx lbraceP p rbraceP

-- specialized cutp
tokcutP msg p inp = cutP (show $
                  text "Parse error:" <+>
                  (if not $ null inp then toDoc $ head inp else text "end of input:") $+$
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
                        accept $ PAP () f atoms 


-- parse a constructor application,
-- accept partially applied CON constructor
conP :: Parser Token (String -> Obj ())
conP = objPat "CON" $
       tokcutP "Expected a valid constructor name"
       conNameP >>> \nm ->
       many' atomP >>> \atoms ->
                        accept $ CON () nm atoms


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
  some' atomP >>> \args ->
                   accept $ EFCall () fn $ map (EAtom ()) args

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
  tokcutP "Expected left brace to open the alt block of a case expr"
  lbraceP >>> \_ ->
  altsP >>> \alts ->
  tokcutP "Expected right brace to close the alt block of a case expr"
  rbraceP >>> \_ ->
               accept $ ECase () exp alts


-- parse an Alts section in a case expression, accept an Alts object
altsP :: Parser Token (Alts ())               
altsP =
  let
    name = "alts" --error "this alts not given a name!" 
  in
   tokcutP "Expected one or more alts separated by semicolons" $
   sepByP' altP semiP >>> \alts ->
                             accept $ Alts () alts name


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
  --boolP `using` LitB,
  fltP `using` LitF
  --dblP `using` LitD,
  --chrP `using` LitC
  ]

---------------------------- DataDef parsing ---------------------------


-- parse a data definition accepting the TyCon object that describes it
tyConP :: Parser Token TyCon
tyConP =
  rsvP "data" >>> \_ ->

  optionally (rsvP "unboxed" `using` (const False)) True >>> \boxed ->
  
  tokcutP "Expected valid constructor name in datatype declaration" $
  conNameP >>> \con ->
  
  many' varNameP >>> \tyvars ->

  tokcutP "Expected '=' Token to bind datatype declaration"
  eqP >>> \_ ->

  tokcutP "Expected one or more data constructor definitions separated by '|'" $
  sepByP dataConP barP >>> \dcs ->
                            accept $ TyCon boxed con tyvars dcs


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
                               
                                



{- old functions for building Params objects and ConMaps
toParamPairs t@(TyCon boxed tnm vars dCons) =
  let tPair =
        (tnm,
         TyConParam { tarity = length vars,
                      ttag   = error "no tag set",
                      tboxed = boxed,
                      tdatacons = map dName dCons,
                      tycon = t})
      dHelp tyName boxed d@(DataCon dnm mtypes) =
        (dnm,
         DataConParam { darity = length mtypes,
                        dtag = error "no tag set",
                        dboxed = boxed,
                        dtycon = tyName,
                        datacon = d})
      dPairs = map (dHelp tnm boxed) dCons
      dName (DataCon n _) = n
  in
   (tPair, dPairs)


toConMaps tycons =
  let
    pairs = map toParamPairs tycons
    tconMap = Map.fromList $ map fst pairs
    dconMap = Map.fromList $ concatMap snd pairs
  in
   (tconMap, dconMap)
-}
-------------------------------- general Parser combinators ------------------------


-- accept parser returns its arguments (value "parsed") and the entire input
-- in the appropriate list-of-tuples format
accept :: v -> Parser i v
accept a inp = [(a,inp)]


-- reject parser
reject :: Parser i v
reject inp = []


-- satisfy a predicate NOTE: Failure does not yield unconsumed input in any form
satisfy :: (a -> Bool) -> Parser a a
satisfy pred [] = reject []
satisfy pred (x:xs)
  | pred x = accept x xs
  | otherwise = reject xs

-- accept anything that a parser does not match
notP :: Parser i v -> Parser i i
notP _ [] = reject []
notP p ii@(i:is) = case p ii of
              [] -> accept i is
              x  -> reject ii

-- match a literal
literal :: (Eq a) => a -> Parser a a
literal x = satisfy (x ==)


-- Match a string (read: sequence) of literal input
litString :: (Eq a) => [a] -> Parser a [a]
litString [] = accept []
litString (x:xs) = (literal x `ordered` litString xs) `using` cons


-- modify the accepted "output" of a parser, given a mutator function
using :: Parser i v1 -> (v1 -> v2) -> Parser i v2
using prsr f inp = [(f val, rest) | (val, rest) <- prsr inp]


-- The inclusive OR Parser, returns the appended results of two parsers on the same input
orInc :: Parser i v -> Parser i v -> Parser i v
orInc p1 p2 inp = (p1 inp) ++ (p2 inp)


-- The exclusive OR Parser returns the result of the first parser that succeeds on the input
orEx :: Parser i v1 -> Parser i v2 -> Parser i (Either v1 v2)
orEx p1 p2 inp = case p1 inp of
  [] -> map (\(a,b) -> (Right a, b)) (p2 inp)
  x -> map (\(a,b) -> (Left a, b)) x


-- List form of orEx, but Parsers in List must be of the same type
orExList :: [Parser i v] -> Parser i v
orExList [] inp = reject inp
orExList (p:ps) inp = case p inp of
  [] -> orExList ps inp
  x  -> x


-- match two parsers in order, combine their results in a tuple
ordered :: Parser i v1 -> Parser i v2 -> Parser i (v1,v2)
ordered p1 p2 inp = [((v1, v2), unused) | (v1, rest) <- p1 inp, (v2, unused) <- p2 rest]


-- many parser matches 0 or more chained matches of a given parser.  All possible matches
-- are returned in the [(i,v)] format, in descending order by length of match in i
many :: Parser i v -> Parser i [v]
many p = ((ordered p (many p)) `using` cons) `orInc` (accept [])


-- Greedy form of many, matches only the longest chain matched
-- by a Parser
many' :: Parser i v -> Parser i [v]
many' p = orExList [ordered p (many' p) `using` cons, accept []]
                         

-- match one or more chained matches given a parser (returns all possible chainings of length
-- 1 or more)
some :: Parser i v -> Parser i [v]
some p = (ordered p (many p)) `using` cons


-- Greedy form of some.  Does not return all possible matches
-- Useful when p is already a complex parser. roughly equivalent to head.some
some' :: Parser i v -> Parser i [v]
some' p = (ordered p (many' p)) `using` cons




-- inSeq accepts a list of parsers, a function for combining their
-- results (tuples) and a "seed" parser to define a folding operation
-- on the Parser list.
inSeq :: ((v1,v2) -> v2) -> Parser i v2 -> [Parser i v1] -> Parser i v2
inSeq f sd = foldr aux sd
        where aux p1 p2 = using (ordered p1 p2) f


-- version of inSeq that appends the results of Parsers in the list
inSeq' :: [Parser i v] -> Parser i [v]
inSeq' = inSeq cons (accept [])              


-- versions of ordered that ignore part of the input
thenx p1 p2 = using (ordered p1 p2) fst
xthen p1 p2 = using (ordered p1 p2) snd
xthenx p1 p2 p3 = p1 `xthen` p2 `thenx` p3
between p1 p2 = xthenx p2 p1 p2


-- Match any of a list of literals
anyEq :: Eq i => [i] -> Parser i i
anyEq ls = satisfy (\l -> or $ map (== l) $ ls)



-- match a sequence of input matched by p1 [p2 p1 p2 p1....p1]
-- fails if no match for p1 is found
sepByP :: Parser i v1 -> Parser i v2 -> Parser i [v1]
sepByP p1 p2 =
  p1 >>> \x ->
  many' (p2 `xthen` p1) >>> \xs ->
                             accept (x:xs)

-- same as sepByP, optionally including a final separator
sepByP' p1 p2 = sepByP p1 p2 `thenx` (optionally p2 undefined)

-- same as sepByP, but forcing at least one separation match, e.g.
-- p1 p2 p1 [p1 p2 p1 p2 ... p1]
sepByP1 :: Parser i v1 -> Parser i v2 -> Parser i [v1]
sepByP1 p1 p2 =
  some' (p1 `thenx` p2) >>> \xs ->
  p1 >>> \x ->
          accept (x:xs)


-- Parser that follows the Maybe pattern.
-- if the given parser succeeds, it returns as normal,
-- otherwise, the alternative is returned with no input
-- consumed
optionally :: Parser i v -> v -> Parser i v
optionally p alt inp = case p inp of
  [] -> accept alt inp
  x -> x


-- "sequencing" parser, from the parsing chapter in Hutton's Programming in Haskell
-- this can be used like a more general form of the ordered parser by defining f
-- as a lambda and nesting sequences therein until, at the last lambda expression,
-- all the bound variables from the lambdas (the result returned by the parser on the left
-- of the sequencing operator) may be used to compose some kind of output
-- e.g.
-- literal 'a' >>> \v1 -> literal 'b' >>> \v2 -> accept (v1, v2)
-- describes a parser that matches "ab" and returns it as a tuple ('a','b')
(>>>) :: Parser i v1 -> (v1 -> Parser i v2) -> Parser i v2
p >>> f = \i -> case p i of
                 [] -> reject i
                 ((v,o):_) -> (f v) o

-- cutP accepts a parser and an error message and fails hard, displaying the message
-- and some context information if the parser does not match input
cutP :: Show i => String -> Parser i v -> Parser i v
cutP m p inp = case p inp of
  [] -> error m
  x  -> x                 


-- try to parse with given parser, but don't consume input
-- if parser fails, peekP also fails
peekP :: Parser i v -> Parser i v
peekP p inp = case p inp of
               [] -> reject inp
               xs -> [(x,inp) | (x,_) <- xs]

-- look ahead and don't consume input,
-- accept True if given parser succeeds, False otherwise
isNextP :: Parser i v -> Parser i Bool
isNextP p inp =
  let b = not $ null $ p inp
  in accept b inp
