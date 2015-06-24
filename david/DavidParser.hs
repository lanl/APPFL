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



The simple Parser combinators are at the end of the file

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
         

type Parser inp val = [inp] -> [(val, [inp])]









-------------------------------- general Parser combinators ------------------------

-- uncurried cons is used on several occasions to combine the results of ordered parsers
cons = uncurry (:)


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

