{-# LANGUAGE NamedFieldPuns #-}

module DavidParser
(
  Parser,
  testReadComments,
  readComments,
  stripComments,
  cons,              -- uncurried (:)
  accept,            -- accept x for any input  
  reject,            -- reject any input, return []
  satisfy,           -- satisfy predicate on head of input
  literal,           -- match on literal equality
  litString,         -- match a sequence of literals
  using,             -- mutate returned input with a given function
  orEx,
  orInc,
  ordered,
  many,
  manyUntil,
  manyGreedy,
  some,
  someGreedy,
  anyEq,
  inSeq,
  inSeqAppend,
  xthen,
  thenx,
  xthenx,
  between,
  optionally,
  acceptOne,
  acceptOneAsList,
  acceptUntil,
  remove
) where
         
import ADT
import AST
import Data.List
import Data.Char
import Data.Either
import Data.Maybe
import Control.Monad -- for testing
import Debug.Trace

type Parser inp val = [inp] -> [(val, [inp])]


testFile = "../codegen/AST.hs"

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


-- match two parsers in order
ordered :: Parser i v1 -> Parser i v2 -> Parser i (v1,v2)
ordered p1 p2 inp = [((v1, v2), unused) | (v1, rest) <- p1 inp, (v2, unused) <- p2 rest]



-- many parser matches 0 or more chained matches of a given parser.  All possible matches
-- are returned in the [(i,v)] format, in descending order by length of match in i
many :: Parser i v -> Parser i [v]
many p = ((ordered p (many p)) `using` cons) `orInc` (accept [])


-- greedy version of many, matches only the longest chain matched
-- by a Parser (for efficiency)
manyGreedy :: Parser i v -> Parser i [v]
manyGreedy p = ((ordered p (manyGreedy p)) `orEx` (accept [])) `using` f
  where f (Right b) = b
        f (Left (a,b)) = a:b

                         
-- given two parsers matches a chain of input accepted by the first parser until the second
-- matches (it is possible for the first to never match).  This is useful when a terminating
-- sequence would be hard to match with a negated predicate in a satisfy Parser
-- this was initially made for matching block comments, where matching all input until a
-- two Char sequence is found was difficult to describe in terms of the other combinators
-- DEPRECATED
manyUntil' :: Parser i v -> Parser i v -> Parser i [v]
manyUntil' p1 p2 = (p2 `orEx` (ordered p1 (manyUntil' p1 p2))) `using` unpack
  where unpack (Left a) = a:[]
        unpack (Right (a , b)) = a : b

manyUntil :: Parser i v1 -> Parser i v2 -> ((Either v2 (v1,v3)) -> v3) -> Parser i v3
manyUntil p1 p2 f = p2 `orEx` ordered p1 (manyUntil p1 p2 f) `using` f


-- match one or more chained matches given a parser (returns all possible chainings of length
-- 1 or more
some :: Parser i v -> Parser i [v]
some p = (ordered p (many p)) `using` cons


someGreedy :: Parser i v -> Parser i [v]
someGreedy p = (ordered p (manyGreedy p)) `using` cons


litString :: (Eq a) => [a] -> Parser a [a]
litString [] = accept []
litString (x:xs) = (literal x `ordered` litString xs) `using` cons


-- inSeq accepts a list of parsers, a function for combining their
-- results (tuples) and a "seed" parser to define a folding operation
-- on the Parser list.
inSeq :: ((v1,v2) -> v2) -> Parser i v2 -> [Parser i v1] -> Parser i v2
inSeq f sd = foldr aux sd
        where aux p1 p2 = using (ordered p1 p2) f

inSeqAppend = inSeq cons (accept [])


thenx p1 p2 = using (ordered p1 p2) fst
xthen p1 p2 = using (ordered p1 p2) snd
xthenx p1 p2 p3 = p2 `xthen` p1 `thenx` p3
between p1 p2 = xthenx p1 p2 p2


-- The failOnReject Parser implements something like a "cut" for a given parser.  If the
-- given parser fails on the input, an exception is generated with a (hopefully) helpful
-- message
-- Error generated should be changed once lexer is done
failOnReject :: (Show i) => Parser i v -> Parser i v
failOnReject p inp = case p inp of
  [] -> let code = take 16 inp
        in error ("failed at " ++ show code ++ if length code < 16 then [] else "...")
  x -> x


-- Parser matching literal Char '='
chrEQ :: Parser Char Char
chrEQ = literal '='

-- Parser matching literal String "="
strEQ :: Parser String String
strEQ = literal "="


-- Match an upper case letter
chrUPPR :: Parser Char Char
chrUPPR = satisfy isUpper 


-- Match numbers (integers) of 1+ digits
number :: Parser Char String
number = some (satisfy isDigit)


-- Match a valid identifier (i.e. letter followed by alphanumeric or valid punctuation (single quote, underscore, more?)
identifier :: Parser Char String
identifier = (ordered (satisfy startPr) (many (satisfy restPr))) `using` cons
  where restPr c = or . map ($ c) $ [isAlphaNum, (== '\''), (== '_')]
        startPr c = or . map ($ c) $ [isAlpha, (== '_')]

-- Match any of a list of literals
anyEq :: Eq i => [i] -> Parser i i
anyEq ls = satisfy (\l -> or . map (== l) $ ls)


-- generic "accept some" input.
acceptOne = satisfy (const True)
acceptOneAsList = acceptOne `using` (:[])

--acceptUntil :: Either a b -> Parser i v
acceptUntil = manyUntil acceptOne
           


optionally :: Parser i v -> v -> Parser i v
optionally p alt inp = case p inp of
  [] -> accept alt inp
  x -> x

remove = undefined
-- remove p inp = (orEx
--             ((ordered (acceptUntil (p `using` (const []))) (remove p)) `using` marry)
--             (accept inp))
--            `using` (either id id) $ inp
--   where marry (a,b) = (concat a) ++ b


main = testReadComments testFile


testReadComments fileName = do
  file <- readFile fileName
  putStrLn (concatMap show (readComments file))
  return ()


data ScanState =
  BComment {depth::Int, str::String} |
  LComment {str::String} |
  Quote {str::String} |
  Top {str::String}

instance Show ScanState where
  show (Top s) = "\nTOP::\n" ++ s
  show (BComment d s) = "\nBLOCK COMMENT::\n" ++ s
  show (LComment s) = "\nLINE COMMENT::\n" ++ s
  show (Quote s) = "\nQUOTE::\n " ++ s


-- CHANGED 6-16: Place appropriate whitespace in comment strings
-- This preserves line and column information for tokenizer
-- The choice to do the substitution here is for efficiency and simplicity
-- (probably better than traversing the list of data structures and
-- mapping the whitespace substitution appropriately)
readComments inp = aux [(Top "")] inp
  where
    -- Ending in Block Comment or Quote state implies uneven delimiter counts
    aux all@((BComment{}):xs) [] = error ("mismatched comment braces:\n" ++
                                          (concatMap show (reverse all)))
    aux all@((Quote{}):xs) []  = error ("mismatched quotes:\n" ++
                                        (concatMap show (reverse all)))

    -- base case
    aux all [] = reverse . map (\x->x{str = reverse (str x)}) $ all
    
    -- pattern match everything else based on accumulator head and beginning sequence of input
    -- Top level state: match comment start characters or double quote, then default
    aux all@(Top {}:xs) ('{':'-':cs) = aux (BComment 1 "  ":all) cs
    aux all@(Top {}:xs) ('"':cs) = aux (Quote "\"":all) cs
    aux all@(Top {}:xs) ('-':'-':cs) = aux (LComment "  ":all) cs
    aux (Top s:xs) (c:cs) = aux (Top (c:s):xs) cs

    -- Quote state: match escape sequence or terminating quote, then default
    aux (Quote s:xs) ('\\':c:cs) = case c of -- stupid escape characters
                                      '\\' -> aux (Quote ('\\':'\\':s):xs) cs
                                      '"'  -> aux (Quote ('"':'\\':s):xs) cs
                                      x    -> aux (Quote (x:'\\':s):xs) cs
    aux (Quote s:xs) ('"':cs) = aux (Top "":Quote ('"':s):xs) cs
    aux (Quote s:xs) (c:cs) = aux (Quote (c:s):xs) cs

    -- Block Comment state: match nested comment (increment depth) or terminating
    -- char sequence (decrementing depth appropriately or reverting to Top state)
    -- then default
    aux (BComment d s:xs) ('{':'-':cs) = aux (BComment (d + 1) (' ':' ':s):xs) cs
    aux (BComment d s:xs) ('-':'}':cs) = case d of
                                            1 -> aux (Top "":BComment 0 (' ':' ':s):xs) cs
                                            _ -> aux (BComment (d - 1) (' ':' ':s):xs) cs
    aux (BComment d s:xs) (c:cs) = case c of
                                    '\n' -> aux (BComment d ('\n':s):xs) cs
                                    _ -> aux (BComment d (' ':s):xs) cs

    -- Line Comment state : match EOL char to terminate, then default
    aux (LComment s:xs) ('\n':cs) = aux (Top "":LComment ('\n':s):xs) cs
    aux (LComment s:xs) (c:cs) = aux (LComment (' ':s):xs) cs


stripComments :: String -> String
stripComments = concatMap str . readComments
