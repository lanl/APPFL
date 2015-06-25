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
module DavidParser
(
  Parser,
  cons,              -- uncurried (:)
  accept,            -- accept x for any input  
  reject,            -- reject any input, return []
  satisfy,           -- satisfy predicate on head of input
  literal,           -- match on literal equality
  litString,         -- match a sequence of literals
  using,             -- mutate returned input with a given function
  orEx,
  orExList,
  orInc,
  ordered,
  many,
  many',
  some,
  some',
  anyEq,
  inSeq,
  inSeq1,
  xthen,
  thenx,
  xthenx,
  between,
  optionally,
) where

import Tokenizer
import AST
import ADT
import qualified Data.Map as Map
import Data.List (groupBy)

-- uncurried cons is used on several occasions to combine the results of ordered parsers
cons = uncurry (:)

-- make it easy to group parsed and unparsed input together for later filtration
data Parsed a b = Parsed a | Unparsed b deriving (Show)
type Parser inp val = [inp] -> [(val, [inp])]


fromParsed :: [Parsed a b] -> ([a],[b])
fromParsed =
  let f x (ps, us) = case x of
        (Parsed p)   -> (p:ps, us)
        (Unparsed u) -> (ps, u:us)
  in foldr f ([],[])

catAdjParsed = map catParsed . groupParsed

groupParsed =
  let f (Parsed _) (Parsed _)     = True
      f (Unparsed _) (Unparsed _) = True
      f _ _                           = False
  in groupBy f

-- The following two functions 
-- from a list of Parsed objects, concatenate all the Parsed values into list inside a
-- single Parsed object
catParsed :: [Parsed a b] -> (Parsed [a] c)
catParsed = Parsed . fst . fromParsed

-- from a list of Parsed objects, concatenate all the Unparsed values into a list inside
-- a single Unparsed object
catUnparsed :: [Parsed a b] -> (Parsed c [b])
catUnparsed = Unparsed . snd . fromParsed

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
inSeq1 :: [Parser i v] -> Parser i [v]
inSeq1 = inSeq cons (accept [])              


-- versions of ordered that ignore part of the input
thenx p1 p2 = using (ordered p1 p2) fst
xthen p1 p2 = using (ordered p1 p2) snd
xthenx p1 p2 p3 = p1 `xthen` p2 `thenx` p3
between p1 p2 = xthenx p2 p1 p2


-- Match any of a list of literals
anyEq :: Eq i => [i] -> Parser i i
anyEq ls = satisfy (\l -> or . map (== l) $ ls)


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
-- as a lambda and nesting sequences therein until, at the "bottom" lambda expression,
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
cutP :: Show i => Parser i v -> String -> Parser i v
cutP p m inp = case p inp of
  [] -> error $
        "\nError" ++ (if null inp then ":" else " at " ++ show (head inp) ++":") ++ m
  x  -> x
                 
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

-- Match constructor token, accept its String
conNameP :: Parser Token String
conNameP = tokP2 (TokCon) `using` tks

-- Match variable token, accept its String
varNameP :: Parser Token String
varNameP = tokP2 (TokId) `using` tks

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
scP = rsvP ";"

-- match EOF Token
eofP = tokP1 (TokEOF)

-- Given a parser, match parens surrounding what it would match
inparensP :: Parser Token v -> Parser Token v
inparensP p = xthenx lparenP p rparenP

-- similar, for braces,
inbracesP :: Parser Token v -> Parser Token v
inbracesP p = xthenx lbraceP p rbraceP

-- accept anything that a parser does not match
notP :: Parser i v -> Parser i i
notP _ [] = reject []
notP p ii@(i:is) = case p ii of
              [] -> accept i is
              x  -> reject ii


---------------------------- Object Parsing -------------------------------

prog = many'
       (orExList [
           objDefSC `using` (Parsed . (:[]) . ObjDef),
           dataDefP `using` (Parsed . (:[]) . DataDef),
           notP eofP `using` (Unparsed. (:[]))
           ]
       )
       `thenx` cutP eofP "EOF not found"
       
     
objDefP =
  varNameP >>> \name ->
  eqP >>> \_ ->
  objP >>> \f -> accept $ f name

objDefSC = objDefP `thenx` scP -- semicolon at end

objP :: Parser Token (String -> Obj ())
objP = orExList [funP, papP, conP, thunkP, errorP]

-- helper abstracts the TokRsv "CAPS" lparenP [definition] rparenP pattern
objPat name p = rsvP name `xthen` (inparensP p)

funP :: Parser Token (String -> Obj ())
funP = objPat "FUN" $
       some' varNameP >>> \v1 ->
       arrowP >>> \_ ->
       exprP >>> \v2 ->
                  accept $ FUN () v1 v2

papP :: Parser Token (String -> Obj ())
papP = objPat "PAP" $
       varNameP >>> \f ->
       many' atomP >>> \atoms ->
                        accept $ PAP () f atoms 
                     

conP :: Parser Token (String -> Obj ())
conP = objPat "CON" $
       conNameP >>> \nm ->
       many' atomP >>> \atoms ->
                        accept $ CON () nm atoms

thunkP :: Parser Token (String -> Obj ())
thunkP = objPat "THUNK" $
         exprP >>> \e ->
                    accept $ THUNK () e

errorP :: Parser Token (String -> Obj ())
errorP = rsvP "ERROR" >>> \_ -> accept $ BLACKHOLE ()

exprP :: Parser Token (Expr ())
exprP = orExList [eFCallP, eAtomP, ePrimopP, eLetP, eCaseP]

eAtomP = atomP `using` (EAtom ())

atomP :: Parser Token Atom
atomP = orExList [
  varNameP `using` Var,
  intP `using` LitI,
  --boolP `using` LitB,
  fltP `using` LitF
  --dblP `using` LitD,
  --chrP `using` LitC
  ]

eFCallP :: Parser Token (Expr ())
eFCallP =
  varNameP >>> \fn ->
  some' atomP >>> \args ->
                   accept $ EFCall () fn args

ePrimopP :: Parser Token (Expr ())
ePrimopP =
  primP >>> \op ->
  some' atomP >>> \args ->
                   accept $ EPrimop () op args

eLetP :: Parser Token (Expr ())
eLetP =
  rsvP "let" >>> \_ ->
  lbraceP >>> \_ ->
  objDefP >>> \d ->
  many' (scP `xthen` objDefP) >>> \ds ->
  rbraceP >>> \_ ->
  rsvP "in" >>> \_ ->
  exprP >>> \exp ->
             accept $ ELet () (d:ds) exp


eCaseP =
  rsvP "case" >>> \_ ->
  exprP >>> \exp ->
  rsvP "of" >>> \_ ->
  lbraceP >>> \_ ->
  altsP >>> \alts ->
  rbraceP >>> \_ ->
               accept $ ECase () exp alts
  
altsP =
  let name = error "this alts not given a name!"
  in
   altP >>> \a ->
   many' (scP `xthen` altP) >>> \as ->
                                 accept $ Alts () (a:as) name

altP =
   orExList [varNameP >>> \v ->
                          accept $ ADef () v,
             conNameP >>> \con ->
             many' varNameP >>> \vs ->
                                 accept $ ACon () con vs
            ] >>> \alt ->
   arrowP >>> \_ ->
   exprP >>> \exp ->
              accept $ alt exp -- apply pap'd Alt constructor to Expr



---------------------------- DataDef parsing ---------------------------


dataDefP =
  dataP >>> \_ ->
  optionally (rsvP "unboxed" `using` (const False)) True >>> \boxed ->
  conNameP >>> \con ->
  many' varNameP >>> \tyvars ->
  dataConP >>> \dc ->
  many' (barP `xthen` dataConP) >>> \dcs ->
                                     accept $ TyCon boxed con tyvars (dc:dcs)
                    
dataConP =
  conNameP >>> \con ->
  many' monoTypP >>> \mTypes ->
                      accept $ DataCon con mTypes

monoTypP = orExList [mVarP, mFunP, mConP, inparensP monoTypP]

mVarP = varNameP `using` MVar

mFunP =
  inparensP $
  monoTypP >>> \m ->
  some' (arrowP `xthen` monoTypP) >>> \ms ->
                                      accept $ foldr1 MFun (m:ms)
                                      
mConP = orExList [conNameP >>> \con ->
                                accept $ MCon con [],
                  lparenP >>> \_ ->
                  conNameP >>> \con ->
                  many' monoTypP >>> \mts ->
                                      accept $ MCon con mts]
                               
                                



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
