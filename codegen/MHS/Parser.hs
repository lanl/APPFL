{-# LANGUAGE NamedFieldPuns #-}

module MHS.Parser
(
  parse,
) where

import ParserComb
import MHS.Tokenizer
import MHS.AST
import qualified Data.Map as Map
import Data.List (groupBy)
import Data.Char (isNumber)
import PPrint


parse :: [Token] -> [Defn]
parse toks = case prog toks of
  [] -> error $ unlines
        ["parse error, not sure where...",
         show $ pprint toks]
  x:xs -> fst x

prog :: Parser Token [Defn]
prog = inbracesP tdecls
--prog = many' defP `thenx`
--       tokcutP "Expected EOF at end of program" eofP

tdecls = sepByP' tdecl (some' semiP)
tdecl = orExList [dDefP, decl]

decls = sepByP' decl (some' semiP)
decl = orExList [oDefP, tDefP]

defP :: Parser Token Defn
defP = orExList [oDefP, dDefP, tDefP]

  
      
----------------------------- Parsers for Tokens ---------------------------


tokP :: Token -> Parser Token Token
tokP _ [] = reject []
tokP t1 (t2:inp) =
  case (t1,t2) of
   (TokNum _ _  , TokNum _ _ ) -> accept t2 inp
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
conNameP = tokP2 TokCon `using` tks

-- Match variable token, accept its String
varNameP :: Parser Token String
varNameP = tokP2 TokId `using` tks

-- Match a numeric token
numTokP :: Parser Token String
numTokP = tokP2 TokNum `using` tks

-- Match numeric literals
boxIntP :: Parser Token Int
boxIntP = satisfy f `using` (read . tks)
  where f (TokNum s _) = all isNumber s
        f _ = False

uboxIntP :: Parser Token Int
uboxIntP = satisfy f `using` (read . init . tks)
  where f (TokNum s _) = all isNumber (init s) && last s == '#'
        f _ = False

boxDblP = satisfy f `using` (read . tks)
  where f (TokNum s _) | '.' `elem` s =
                         let (w, d:ds) = break (== '.') s
                         in all isNumber w && all isNumber ds
        f _ = False

uboxDblP = satisfy f `using` (read . init . tks)
  where f (TokNum s _ ) | '.' `elem` s =
                          let (w, d:ds) = break (== '.') s
                          in all isNumber w &&
                             all isNumber (init ds) &&
                             last ds == '#'
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
lambdaP = rsvP "\\"
doubleColonP = rsvP "::"



-- optional braces (for let defs and case clauses)
optBracesP p =
  isNextP lbraceP >>> \b ->
  if not b
  then p
  else inbracesP p

optParens p =
  isNextP lparenP >>> \b ->
  if not b
  then p
  else inparensP p

-- match EOF Token
eofP = tokP1 (TokEOF)

-- Given a parser, match parens surrounding what it would match
inparensP :: Parser Token v -> Parser Token v
inparensP p = xthenx lparenP p $ tokcutP "Expected closing paren" rparenP

-- similar, for braces,
inbracesP :: Parser Token v -> Parser Token v
inbracesP p = xthenx lbraceP p $ tokcutP "Expected closing brace" rbraceP

-- specialized cutp
tokcutP msg p inp = cutP (show $
                  text "Parse error:" <+>
                  (if not $ null inp then pprint $ head inp else text "end of input:") $+$
                  text msg)
                  p inp

---------------------------- Type Annotation Parsing ---------------------

tDefP :: Parser Token Defn
tDefP =
  varNameP >>> \name ->
  doubleColonP >>> \_ ->
  typeP >>> \mtyp ->
--  semiP >>> \_ ->
             accept $ TDefn name mtyp
  


                    
---------------------------- Object Parsing -------------------------------

-- Parse an Object definition 
oDefP :: Parser Token Defn              
oDefP =
  varNameP >>> \name ->
  many' patternP >>> \pats ->
  eqP >>> \_ ->
  tokcutP "Expected valid expression on rhs of object definition"
  exprP >>> \exp ->
--  tokcutP "Expected semicolon to terminate object definition"
--  semiP >>> \_ ->
             accept $   -- if args occur on lhs, build function expression
             ODefn name (if null pats
                         then exp
                         else (EFn pats exp)) Nothing




-- parse an expression, accept an Exp object
exprP :: Parser Token Exp
exprP =
  let mults = some' (orExList
              [eAtomP, inparensP exprP]) >>> \es ->
                                             accept $ foldl1 EAp es  
-- if only one Exp is parsed, foldl1 leaves it unchanged,
-- otherwise left-associative application is parsed, so
-- e1 e2 e3 e4 == ((((e1) e2) e3) e4)
-- let, case and lambda expressions are all treated slightly differently
-- to conform with haskell parsing of function application
-- case (+) of f -> f; 1 2 produces a compile error (even with braces in alts)
-- but surrounding it with parens makes the application parse correctly
  in orExList
     [eLetP, eCaseP, eFunP, mults]

     
-- parse an anonymous function expression
-- should behave as in Haskell, with everything to the right
-- of the arrow being considered the body of the lambda unless parenthesized
eFunP :: Parser Token Exp
eFunP =
  lambdaP >>> \_ ->
  tokcutP "Expected one or more pattern bindings in lambda expression"
  (some' patternP) >>> \pats ->
  tokcutP "Expected '->' to precede lambda body"
  arrowP >>> \_ ->
  tokcutP "Expected valid expression in lambda body"
  exprP >>> \exp ->
             accept $ EFn pats exp

-- parse an atom expression
eAtomP :: Parser Token Exp
eAtomP = atomP `using` EAt


-- parse a let expression
eLetP :: Parser Token Exp
eLetP =
   rsvP "let" >>> \_ ->
   tokcutP "Expected one or more valid declarations for a let expr"
   (inbracesP decls) >>> \defs ->
   tokcutP "Expected 'in' to open a let expr's sub expression"
   (rsvP "in") >>> \_ ->
   exprP >>> \exp ->
              accept $ ELt exp defs


-- parse a case expression
eCaseP :: Parser Token Exp
eCaseP =
  rsvP "case" >>> \_ ->
  exprP >>> \exp ->
  tokcutP "Expected 'of' Token to close the scrutinee of a case expr"
  (rsvP "of") >>> \_ ->
  inbracesP clausesP >>> \cls ->
                          accept $ ECs exp cls


clausesP = sepByP' clauseP (some' semiP)

-- parse a case expression alternative, accept an Alt object
clauseP :: Parser Token (Pattern, Exp)
clauseP =
  casePatP >>> \pat ->
  tokcutP "Expected a '->' symbol after a clause's pattern"
  arrowP >>> \_ ->
  tokcutP "Expected a valid expression in clause body"
  exprP >>> \exp ->
--  tokcutP "Expected a semicolon to terminate a clause's body"
--  semiP >>> \_ ->
             accept (pat, exp)


casePatP :: Parser Token Pattern
casePatP inp = (some patternP >>> \pats ->
  case pats of
   Match{str, npats=[]}:ps -> accept (Match str ps)
   [d@Default{}] -> accept d
   [m@Match{}] -> accept m
   [] -> error "case clause requires a pattern or variable to match against"
   _ -> tokcutP
        ("strange pattern in case clause: " ++ show (hsep $ map unparse pats) ++
        (show $ pprint $ take 20 inp)) reject) inp



patternP = orExList [varPatP, simplConPatP, nestConPatP]
simplConPatP = orExList
               [conNameP, numTokP] >>> \p ->
                                        accept $ Match p []
                                                  
nestConPatP = inparensP (
  conNameP >>> \c ->
  many' patternP >>> \ps ->
                      accept $ Match c ps)

varPatP :: Parser Token Pattern
varPatP = varNameP `using` Default
  

-- parse an atom (variable or literal)
atomP :: Parser Token Atm
atomP = orExList [
  varNameP `using` AtmVar,
  conNameP `using` AtmCon,
  primP `using` AtmOp,
  boxIntP `using` LBInt,
  boxDblP `using` LBDbl,
  uboxIntP `using` LUBInt,
  uboxDblP `using` LUBDbl]

---------------------------- DataDef parsing ---------------------------


-- parse a data definition accepting the TyCon object that describes it
dDefP :: Parser Token Defn
dDefP =
  rsvP "data" >>> \_ ->

  optP  (rsvP "unboxed") >>> \b ->
  
  tokcutP "Expected valid constructor name in datatype declaration"
  conNameP >>> \con ->
  
  many' varNameP >>> \vars ->

  tokcutP "Expected '=' Token to bind datatype declaration"
  eqP >>> \_ ->

  tokcutP "Expected one or more data constructor definitions separated by '|'"
  (sepByP constrP barP) >>> \dcs ->

--  tokcutP "Expected semicolon to terminate datatype definition"
--  semiP >>> \_ ->
             let boxed = maybe True (const False) b
                 mvs = map MVar vars
                 mtyp = MCon boxed con mvs
                 cs = map dcon dcs
                 dcs' = map (\d -> d{cons = cs}) dcs
                 def = DDefn mtyp dcs'
             in accept def


-- parse a data constructor as a DataCon object
constrP :: Parser Token Constr
constrP =
  conNameP >>> \con ->

  isNextP (orExList [semiP, eofP, barP]) >>> \b ->

  -- this feels hacky and wrong, but allows for a slightly better error message
  if b then accept $ DCon con [] undefined

  else tokcutP "Expected valid monotypes in data constructor"
       (some' atypeP) >>> \mTypes ->
                           accept $ DCon con mTypes undefined


typeP =
  btypeP >>> \t ->
  many' (arrowP `xthen` typeP) >>> \ts ->
                                   accept $ foldr1 MFun (t:ts)

btypeP =
  let err = True --error "boxity not set in MCON"
      cAp = 
        conNameP >>> \c -> -- only permit type applicaton for con names (e.g. List a -> Int, not m a -> Int)
        many' atypeP >>> \ms ->
        accept $ case () of 
                  _ | c == "Int#" -> biIntMCon -- MPrim UBInt -- hacky
                    | c == "Double#" -> biDoubleMCon -- MPrim UBDouble
                    | otherwise -> MCon err c ms
                                   
  in orExList [cAp, atypeP]

atypeP =
  let err = True --error "boxity not set in MCON"
      primTypP inp = case inp of
        (TokCon "Int#" _:rs)    -> accept "Int_h" rs --accept UBInt rs
        (TokCon "Double#" _:rs) -> accept "Double_h" rs --UBDouble rs
        _ -> reject inp
  in
   orExList [
     primTypP >>> \p -> accept $ MCon False p [], --MPrim p,
     conNameP >>> \c -> accept $ MCon err c [],
     varNameP >>> \v -> accept $ MVar v,
     inparensP typeP]
                    
      
                    

--TODO: Fix this
conMonoTypP = orExList [mVarP, inparensP mFunP, mConP, inparensP monoTypP]

-- parse a monotype in a data constructor as a Monotype object
monoTypP :: Parser Token Monotype
monoTypP = orExList [mVarP, mFunP, mConP, inparensP monoTypP]


-- parse a variable as an MVar Monotype (e.g. 'a' in Just a)
mVarP :: Parser Token Monotype
mVarP = varNameP `using` MVar


-- parse a Monotype function as an MFun object (e.g. a->b)
mFunP :: Parser Token Monotype
mFunP =
  monoTypP >>> \m ->
  peekP arrowP >>> \_ ->
  tokcutP "Expected valid monotype(s) following '->' token in data constructor"
  (some' $ arrowP `xthen` monoTypP) >>> \ms ->
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
                               
                                
