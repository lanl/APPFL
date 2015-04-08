module ParseComb (
  Parser,
  succeedp,
  failp,
  satisfyp,
  literalp,
  altp,
  thenp,
  cutp,
  usingp,
  manyp,
  somep,
  maybep,
  returnp,
  listp,
  xthenp,
  thenxp,
  anyp,
  sepbyp,
  anymapp,
  optp,
  exactp,

-- string/char specific

  alphap,
  numeralp,
  alphaornumeralp,
  whitep,
) where

import Data.Char

type Parser input result = [input] -> [(result,[input])]

succeedp :: b -> Parser a b -- b -> [a] -> [(b,[a])]
succeedp v inp = [(v, inp)]

failp :: Parser a b
failp inp = []

-- predicate first element of input list
satisfyp :: (a -> Bool) -> Parser a a
satisfyp p [] = failp []
satisfyp p (x:xs) | p x                = succeedp x xs
                    | otherwise        = failp xs

-- match first element of list
literalp :: Eq b => b -> Parser b b
literalp x = satisfyp (==x)

-- alternative
altp :: Parser a b -> Parser a b -> Parser a b
(p1 `altp` p2) inp = p1 inp ++ p2 inp

-- sequence
thenp :: Parser t1 t2 -> Parser t1 t3 -> Parser t1 (t2, t3)
(p1 `thenp` p2) inp = [((v1, v2),out2) | (v1, out1) <- p1 inp, (v2, out2) <- p2 out1]

-- cut
cutp msg p inp =
    case p inp of
      [] -> error ("cut error \"" ++ msg ++ "\" at \"" ++ concatMap show (take 15 inp))
      r -> r

-- just transform output
usingp :: Parser t1 t2 -> (t2 -> t3) -> Parser t1 t3
(p `usingp` f) inp = [(f v,out) | (v,out) <- p inp]

-- uncurrieed cons
ucons (x,xs) = x:xs

-- match list
listp :: Eq a => [a] -> Parser a [a]
listp [] = succeedp []
listp (x:xs) = (literalp x `thenp` listp xs) `usingp` ucons

-- zero or more
manyp :: Parser a b -> Parser a [b]
manyp p = ((p `thenp` manyp p) `usingp` ucons) `altp` (succeedp [])

-- zero or one

-- optp :: Parser a b -> b -> Parser a b
p `optp` v = p `altp` (succeedp v)

-- optp p = p `altp` (succeedp [])

-- one or more
somep :: Parser a b -> Parser a [b]
somep p = (p `thenp` manyp p) `usingp` ucons

-- embed into Maybe monad--note Nothing always in result
maybep :: Parser a t2 -> Parser a (Maybe t2)
maybep p = (p `usingp` Just) `altp` (succeedp Nothing)

-- replace result
returnp :: Parser t1 t2 -> t3 -> Parser t1 t3
returnp p v = p `usingp` (const v)
    where const x y = x

-- discard first result
xthenp :: Parser t1 t2 -> Parser t1 t3 -> Parser t1 t3
p1 `xthenp` p2 = (p1 `thenp` p2) `usingp` snd

-- discard second result
thenxp :: Parser t1 t3 -> Parser t1 t2 -> Parser t1 t3
p1 `thenxp` p2 = (p1 `thenp` p2) `usingp` fst

-- map p over list of arguments
anymapp :: (a -> Parser a1 b) -> [a] -> Parser a1 b
anymapp p = foldr (altp.p) failp

-- list of parsers:  anymapp p == anyp (map p)
anyp :: [Parser a b] -> Parser a b
anyp ps = foldr altp failp ps

-- sepbyp, discard separator, e.g. "a ; b ; c"
sepbyp :: Parser t1 t3 -> Parser t1 t2 -> Parser t1 [t3]
sepbyp p1 p2 = p1 `thenp` (manyp (p2 `xthenp` p1)) `usingp` uncurry (:)

exactp p inp =
    case p inp of
      [] -> error "ParseComb.exactp error, empty result"
      res@((parsed, residual):_) -> 
          case residual of
            (x:xs) -> error ("ParseComb.exactp error, crap left at end" ++ residual)
            _ -> res

-- alphabetic

alphap = satisfyp isAlpha

numeralp = satisfyp isDigit

alphaornumeralp = satisfyp isAlphaNum

whitep = satisfyp isSpace




