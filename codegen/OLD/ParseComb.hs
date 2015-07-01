module ParseComb (
  Parser,
  succeedp,
  failp,
  satisfyp,
  literalp,
  altp,
  thenp,
  cutp,
  errorp,
  usingp,
  onep,
  optlp,
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
  listsubp,

-- string/char specific

  alphap,
  numeralp,
  alphaornumeralp,
  alphanumus,
  whitep,
) where

import Data.Char
import Data.List

type Parser input result = [input] -> [(result,[input])]

succeedp :: b -> Parser a b -- b -> [a] -> [(b,[a])]
succeedp v inp = [(v, inp)]

failp :: Parser a b
failp _ = []

-- predicate first element of input list
satisfyp :: (a -> Bool) -> Parser a a
satisfyp _ [] = failp []
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
cutp :: Show a => String -> ([a] -> [t]) -> [a] -> [t]
cutp msg p inp =
    case p inp of
      [] -> error $ "cut error " ++ msg ++ " " ++ (intercalate " " $ map show $ take 15 inp)
      r -> r

errorp msg inp =
    error $ "errorp: " ++ msg ++ " " ++ (intercalate " " $ map show $ take 15 inp)

-- just transform output
usingp :: Parser t1 t2 -> (t2 -> t3) -> Parser t1 t3
(p `usingp` f) inp = [(f v,out) | (v,out) <- p inp]

-- uncurrieed cons
ucons :: (a, [a]) -> [a]
ucons (x,xs) = x:xs

-- match list
listp :: Eq a => [a] -> Parser a [a]
listp [] = succeedp []
listp (x:xs) = (literalp x `thenp` listp xs) `usingp` ucons

-- match list and substitute
listsubp xs sub inp =
    case listp xs inp of
      [] -> failp []
      [(_, inp')] -> [(sub, inp')]
      _ -> error "listsubp this should be impossible"

-- zero or more
manyp :: Parser a b -> Parser a [b]
manyp p = ((p `thenp` manyp p) `usingp` ucons) `altp` (succeedp [])

-- one 
onep :: Parser a b -> Parser a [b]
onep p = (p `thenp` succeedp []) `usingp` ucons

-- zero or one
optlp :: Parser a b -> Parser a [b]
optlp p = (onep p) `altp` (succeedp [])

optp :: Parser a b -> b -> Parser a b
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
returnp p v = p `usingp` (constp v)
    where constp x _ = x

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

exactp :: (t -> [(t1, String)]) -> t -> [(t1, String)]
exactp p inp =
    case p inp of
      [] -> error "ParseComb.exactp error, empty result"
      res@((parsed, residual):_) -> 
          case residual of
            (x:xs) -> error ("ParseComb.exactp error, crap left at end" ++ (x:xs))
            _ -> res

-- alphabetic

alphap :: Parser Char Char
alphap = satisfyp isAlpha

numeralp :: Parser Char Char
numeralp = satisfyp isDigit

alphaornumeralp :: Parser Char Char
alphaornumeralp = satisfyp isAlphaNum

alphanumus = satisfyp isAlphaNum `altp` satisfyp (=='_')

whitep :: Parser Char Char
whitep = satisfyp isSpace




