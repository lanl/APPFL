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
             | "let" <letDefs> "in" <expr>
             | "case" <expr> "of" <alts>

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

module NewParser
(
  Parsed(..),
  parse,
  fromParsed,
) where

import NewTokenizer
import NewAST
import qualified Data.Map as Map
import Data.List (groupBy)
import Data.Char (isNumber)
import PPrint



instance (PPrint a, PPrint b) => PPrint (Parsed a b) where
  pprint (Parsed a) =
    (lcomment $ text "PARSED:") $+$ pprint a
  pprint (Unparsed b) =
    bcomment $ text "UNPARSED:" $+$ pprint b


-- make it easy to group parsed and unparsed input together for later filtration
data Parsed a b = Parsed a | Unparsed b deriving (Show)

-- type pseudonym for Parsers
type Parser inp val = [inp] -> [(val, [inp])]

parse :: [Token] -> [Defn]
parse =  fst . head . prog

prog :: Parser Token [Defn]
prog = many' defP `thenx`
       tokcutP "Expected EOF at end of program" eofP

defP :: Parser Token Defn
defP = orExList [oDefP, dDefP, tDefP]


testDBG filename =
  do
    f <- readFile filename
    let (ps,us) = fromParsed $ parseDBG f
        doc = text "##PARSED##" $+$ vcat (concatMap (map unparse) ps) $+$
              text "##UNPARSED##" $+$ vcat (map pprint us)
    print doc
    

parseDBG :: String -> [Parsed [Defn] Token]
parseDBG = fst . head . progDBG . tokenize

progDBG :: Parser Token [Parsed [Defn] Token]
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
conNameP =  TokCon `using` tks

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
  semiP >>> \_ ->
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
  tokcutP "Expected semicolon to terminate object definition"
  semiP >>> \_ ->
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
   optBracesP $
   tokcutP "Expected one or more declarations for a let expr"
   (many' $ orExList [oDefP, tDefP]) >>> \defs ->
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
  optBracesP $ some' clauseP >>> \cls ->
                                accept $ ECs exp cls


-- parse a case expression alternative, accept an Alt object
clauseP :: Parser Token (Pattern, Exp)
clauseP =
  casePatP >>> \pat ->
  tokcutP "Expected a '->' symbol after a clause's pattern"
  arrowP >>> \_ ->
  tokcutP "Expected a valid expression in clause body"
  exprP >>> \exp ->
  tokcutP "Expected a semicolon to terminate a clause's body"
  semiP >>> \_ ->
             accept (pat, exp)


casePatP :: Parser Token Pattern
casePatP = some patternP >>> \pats ->
  case pats of
   Match{str, npats = []}:ps -> accept $ Match str ps
   [d@Default{}] -> accept $ d
   [] -> error "case clause requires a pattern or variable to match against"
   _ -> error $ "strange pattern in case clause: " ++ show (hsep $ map unparse pats)



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

  optP (rsvP "unboxed" `using` (const False)) True >>> \boxed ->
  
  tokcutP "Expected valid constructor name in datatype declaration"
  conNameP >>> \con ->
  
  many' varNameP >>> \vars ->

  tokcutP "Expected '=' Token to bind datatype declaration"
  eqP >>> \_ ->

  tokcutP "Expected one or more data constructor definitions separated by '|'"
  (sepByP constrP barP) >>> \dcs ->

  tokcutP "Expected semicolon to terminate datatype definition"
  semiP >>> \_ ->
             let mvs = map MVar vars
                 mtyp = MCon boxed con mvs
                 def = DDefn mtyp dcs
             in accept def


-- parse a data constructor as a DataCon object
constrP :: Parser Token Constr
constrP =
  conNameP >>> \con ->

  isNextP (orExList [semiP, eofP, barP]) >>> \b ->

  -- this feels hacky and wrong, but allows for a slightly better error message
  if b then accept $ DCon con []

  else tokcutP "Expected valid monotypes in data constructor"
       (some' atypeP) >>> \mTypes ->
                           accept $ DCon con mTypes


typeP =
  btypeP >>> \t ->
  many' (arrowP `xthen` typeP) >>> \ts ->
                                   accept $ foldr1 MFun (t:ts)

btypeP =
  let err = True --error "boxity not set in MCON"
      cAp = 
        conNameP >>> \c -> -- only permit type applicaton for con names (e.g. List a -> Int, not m a -> Int)
        many' atypeP >>> \ms -> accept $ MCon err c ms
  in orExList [cAp, atypeP]

atypeP =
  let err = True --error "boxity not set in MCON"
      primTypP inp = case inp of
        (TokCon "Int#" _:rs)    -> accept UBInt rs
        (TokCon "Double#" _:rs) -> accept UBDouble rs
        _ -> reject inp
  in
   orExList [
     primTypP >>> \p -> accept $ MPrim p,
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
sepByP' p1 p2 = sepByP p1 p2 `thenx` (optP p2 undefined)

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
optP :: Parser i v -> v -> Parser i v
optP p alt inp = case p inp of
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


