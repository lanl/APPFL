{-# LANGUAGE TypeSynonymInstances #-}
module ParserComb
(
  Parsed (..),
  Parser,
  extractParsed,
  groupParsed,
  accept,
  reject,
  emptyP,
  satisfy,
  notP,
  litP,
  ordP,
  xorP,
  litString,
  using,
  orInc,
  orEx,
  orExList,
  many',
  many,
  some',
  some,
  inSeq,
  inSeq',
  thenx,
  xthen,
  xthenx,
  between,
  sepByP,
  sepByP',
  sepByP1,
  optP,
  cutP,
  peekP,
  isNextP,
  asManyAsP,
  atLeastButNotMoreThanP,
  (>>>)
) where


import PPrint
import Data.List (groupBy)

instance (PPrint a, PPrint b) => PPrint (Parsed a b) where
    pprint (Parsed a) =
        lcomment $ text "PARSED:" $+$ pprint a
    pprint (Unparsed b) =
        bcomment $ text "UNPARSED:" $+$ pprint b


-- make it easy to group parsed and unparsed input together for later filtration
data Parsed a b = Parsed a | Unparsed b deriving (Show)

-- type alias for Parsers
type Parser inp val = [inp] -> [(val, [inp])]

    
extractParsed :: [Parsed a b] -> ([a],[b])
extractParsed =
    let f x (ps, us) =
            case x of
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


-- accept parser returns its arguments (value "parsed") and the entire input
-- in the appropriate list-of-tuples format
accept :: v -> Parser i v
accept a inp = [(a,inp)]


-- reject parser
reject :: Parser i v
reject inp = []

emptyP :: Parser a ()
emptyP inp = [((),inp)]

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
litP :: (Eq a) => a -> Parser a a
litP x = satisfy (x ==)


-- Match a string (read: sequence) of literal input
litString :: (Eq a) => [a] -> Parser a [a]
litString [] = accept []
litString (x:xs) = (litP x `ordP` litString xs) `using` cons


-- modify the accepted "output" of a parser, given a mutator function
using :: Parser i v1 -> (v1 -> v2) -> Parser i v2
using prsr f inp = [(f val, rest) | (val, rest) <- prsr inp]


-- The inclusive OR Parser, returns the appended results of two parsers on the same input
orInc :: Parser i v -> Parser i v -> Parser i v
orInc p1 p2 inp = (p1 inp) ++ (p2 inp)


-- Exclusive or parser that accepts result of the first succeeding parser it is given
-- where both parsers have the same type (as opposed to orEx)
xorP :: Parser i v -> Parser i v -> Parser i v
xorP p1 p2 inp = case p1 inp of
  [] -> p2 inp
  x -> x

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
ordP :: Parser i v1 -> Parser i v2 -> Parser i (v1,v2)
ordP p1 p2 inp = [((v1, v2), unused) | (v1, rest) <- p1 inp, (v2, unused) <- p2 rest]


notFollowedBy :: Parser i v1 -> Parser i v2 -> Parser i v1
notFollowedBy p1 p2 inp = [ (v1, inp') | (v1, inp') <- p1 inp, null $ p2 inp']

-- many parser matches 0 or more chained matches of a given parser.  All possible matches
-- are returned in the [(i,v)] format, in descending order by length of match in i
many :: Parser i v -> Parser i [v]
many p = (ordP p (many p) `using` cons) `orInc` accept []


-- Greedy form of many, matches only the longest chain matched
-- by a Parser
many' :: Parser i v -> Parser i [v]
many' p = orExList [ordP p (many' p) `using` cons, accept []]
                         

-- match one or more chained matches given a parser (returns all possible chainings of length
-- 1 or more)
some :: Parser i v -> Parser i [v]
some p = ordP p (many p) `using` cons


-- Greedy form of some.  Does not return all possible matches
-- Useful when p is already a complex parser. roughly equivalent to head.some
some' :: Parser i v -> Parser i [v]
some' p = ordP p (many' p) `using` cons


asManyAsP n = atLeastButNotMoreThanP 0 n
atLeastButNotMoreThanP lo hi p =
  many' p >>> \res ->
  let l = length res in
    if l < lo || l > hi
    then reject
    else accept res
                                               

-- inSeq accepts a list of parsers, a function for combining their
-- results (tuples) and a "seed" parser to define a folding operation
-- on the Parser list.
inSeq :: ((v1,v2) -> v2) -> Parser i v2 -> [Parser i v1] -> Parser i v2
inSeq f sd = foldr aux sd
        where aux p1 p2 = using (ordP p1 p2) f


-- version of inSeq that appends the results of Parsers in the list
inSeq' :: [Parser i v] -> Parser i [v]
inSeq' = inSeq cons (accept [])


-- versions of ordP that ignore part of the input
thenx p1 p2 = using (ordP p1 p2) fst
xthen p1 p2 = using (ordP p1 p2) snd
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
sepByP' p1 p2 = sepByP p1 p2 `thenx` optP p2

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
optP :: Parser i v -> Parser i (Maybe v)
optP p inp = case p inp of
  [] -> accept Nothing inp
  xs -> [(Just v, rs) | (v, rs) <- xs]


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
                 ((v,o):_) -> f v o

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
