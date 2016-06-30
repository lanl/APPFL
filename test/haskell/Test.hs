{-# LANGUAGE
MagicHash, BangPatterns, NoImplicitPrelude #-}

module Test where

import GHC.Prim
import Prelude (Num (..), Bounded (..), Enum(..), Eq(..), Bool (..),
                Show (..),
                otherwise, error, Integer)
import GHC.Num (integerToInt, mkInteger)

import qualified GHC.Base as B (Int (..))

data Int = I# Int# deriving (Show)

naught = mkInteger True []
zero = 0



instance  Num Int  where
    I# x + I# y = I# (x +# y)
    I# x - I# y = I# (x -# y)
    negate (I# x) = I# (negateInt# x)
    I# x * I# y = I# (x *# y)
    abs n  = if n `geInt` 0 then n else negate n

    signum n | n `ltInt` 0 = negate 1
             | n `eqInt` 0 = 0
             | otherwise   = 1

    {-# INLINE fromInteger #-}   -- Just to be sure!
    fromInteger i = I# (integerToInt i)


instance Eq Int where
    (==) = eqInt
    (/=) = neInt

{-# INLINE eqInt #-}
{-# INLINE neInt #-}
eqInt, neInt :: Int -> Int -> Bool
(I# x) `eqInt` (I# y) = isTrue# (x ==# y)
(I# x) `neInt` (I# y) = isTrue# (x /=# y)

{-# INLINE gtInt #-}
{-# INLINE geInt #-}
{-# INLINE ltInt #-}
{-# INLINE leInt #-}
gtInt, geInt, ltInt, leInt :: Int -> Int -> Bool
(I# x) `gtInt` (I# y) = isTrue# (x >#  y)
(I# x) `geInt` (I# y) = isTrue# (x >=# y)
(I# x) `ltInt` (I# y) = isTrue# (x <#  y)
(I# x) `leInt` (I# y) = isTrue# (x <=# y)

instance  Bounded Int where
    minBound =  minInt
    maxBound =  maxInt

minInt = I# (-8#)
maxInt = I# (7#)

instance  Enum Int  where
    succ x
       | x == maxBound  = error "Prelude.Enum.succ{Int}: tried to take `succ' of maxBound"
       | otherwise      = x + 1
    pred x
       | x == minBound  = error "Prelude.Enum.pred{Int}: tried to take `pred' of minBound"
       | otherwise      = x - 1

    toEnum   (B.I# x) = I# x
    fromEnum (I# x) = B.I# x

    {-# INLINE enumFrom #-}
    enumFrom (I# x) = eftInt x maxInt#
        where !(I# maxInt#) = maxInt
        -- Blarg: technically I guess enumFrom isn't strict!

    {-# INLINE enumFromTo #-}
    enumFromTo (I# x) (I# y) = eftInt x y

    {-# INLINE enumFromThen #-}
    enumFromThen (I# x1) (I# x2) = efdInt x1 x2

    {-# INLINE enumFromThenTo #-}
    enumFromThenTo (I# x1) (I# x2) (I# y) = efdtInt x1 x2 y



-----------------------------------------------------
-- eftInt and eftIntFB deal with [a..b], which is the
-- most common form, so we take a lot of care
-- In particular, we have rules for deforestation

-- {-# RULES
-- "eftInt"        [~1] forall x y. eftInt x y = build (\ c n -> eftIntFB c n x y)
-- "eftIntList"    [1] eftIntFB  (:) [] = eftInt
-- #-}

{-# NOINLINE [1] eftInt #-}
eftInt :: Int# -> Int# -> [Int]
-- [x1..x2]
eftInt x0 y | isTrue# (x0 ># y) = []
            | otherwise         = go x0
               where
                 go x = I# x : if isTrue# (x ==# y)
                               then []
                               else go (x +# 1#)

{-# INLINE [0] eftIntFB #-}
eftIntFB :: (Int -> r -> r) -> r -> Int# -> Int# -> r
eftIntFB c n x0 y | isTrue# (x0 ># y) = n
                  | otherwise         = go x0
                 where
                   go x = I# x `c` if isTrue# (x ==# y)
                                   then n
                                   else go (x +# 1#)
                        -- Watch out for y=maxBound; hence ==, not >
        -- Be very careful not to have more than one "c"
        -- so that when eftInfFB is inlined we can inline
        -- whatever is bound to "c"


-----------------------------------------------------
-- efdInt and efdtInt deal with [a,b..] and [a,b..c].
-- The code is more complicated because of worries about Int overflow.

-- {-# RULES
-- "efdtInt"       [~1] forall x1 x2 y.
--                      efdtInt x1 x2 y = build (\ c n -> efdtIntFB c n x1 x2 y)
-- "efdtIntUpList" [1]  efdtIntFB (:) [] = efdtInt
--  #-}

efdInt :: Int# -> Int# -> [Int]
-- [x1,x2..maxInt]
efdInt x1 x2
 | isTrue# (x2 >=# x1) = case maxInt of I# y -> efdtIntUp x1 x2 y
 | otherwise           = case minInt of I# y -> efdtIntDn x1 x2 y

{-# NOINLINE [1] efdtInt #-}
efdtInt :: Int# -> Int# -> Int# -> [Int]
-- [x1,x2..y]
efdtInt x1 x2 y
 | isTrue# (x2 >=# x1) = efdtIntUp x1 x2 y
 | otherwise           = efdtIntDn x1 x2 y

{-# INLINE [0] efdtIntFB #-}
efdtIntFB :: (Int -> r -> r) -> r -> Int# -> Int# -> Int# -> r
efdtIntFB c n x1 x2 y
 | isTrue# (x2 >=# x1) = efdtIntUpFB c n x1 x2 y
 | otherwise           = efdtIntDnFB c n x1 x2 y

-- Requires x2 >= x1
efdtIntUp :: Int# -> Int# -> Int# -> [Int]
efdtIntUp x1 x2 y    -- Be careful about overflow!
 | isTrue# (y <# x2) = if isTrue# (y <# x1) then [] else [I# x1]
 | otherwise = -- Common case: x1 <= x2 <= y
               let !delta = x2 -# x1 -- >= 0
                   !y' = y -# delta  -- x1 <= y' <= y; hence y' is representable

                   -- Invariant: x <= y
                   -- Note that: z <= y' => z + delta won't overflow
                   -- so we are guaranteed not to overflow if/when we recurse
                   go_up x | isTrue# (x ># y') = [I# x]
                           | otherwise         = I# x : go_up (x +# delta)
               in I# x1 : go_up x2

-- Requires x2 >= x1
efdtIntUpFB :: (Int -> r -> r) -> r -> Int# -> Int# -> Int# -> r
efdtIntUpFB c n x1 x2 y    -- Be careful about overflow!
 | isTrue# (y <# x2) = if isTrue# (y <# x1) then n else I# x1 `c` n
 | otherwise = -- Common case: x1 <= x2 <= y
               let !delta = x2 -# x1 -- >= 0
                   !y' = y -# delta  -- x1 <= y' <= y; hence y' is representable

                   -- Invariant: x <= y
                   -- Note that: z <= y' => z + delta won't overflow
                   -- so we are guaranteed not to overflow if/when we recurse
                   go_up x | isTrue# (x ># y') = I# x `c` n
                           | otherwise         = I# x `c` go_up (x +# delta)
               in I# x1 `c` go_up x2

-- Requires x2 <= x1
efdtIntDn :: Int# -> Int# -> Int# -> [Int]
efdtIntDn x1 x2 y    -- Be careful about underflow!
 | isTrue# (y ># x2) = if isTrue# (y ># x1) then [] else [I# x1]
 | otherwise = -- Common case: x1 >= x2 >= y
               let !delta = x2 -# x1 -- <= 0
                   !y' = y -# delta  -- y <= y' <= x1; hence y' is representable

                   -- Invariant: x >= y
                   -- Note that: z >= y' => z + delta won't underflow
                   -- so we are guaranteed not to underflow if/when we recurse
                   go_dn x | isTrue# (x <# y') = [I# x]
                           | otherwise         = I# x : go_dn (x +# delta)
   in I# x1 : go_dn x2

-- Requires x2 <= x1
efdtIntDnFB :: (Int -> r -> r) -> r -> Int# -> Int# -> Int# -> r
efdtIntDnFB c n x1 x2 y    -- Be careful about underflow!
 | isTrue# (y ># x2) = if isTrue# (y ># x1) then n else I# x1 `c` n
 | otherwise = -- Common case: x1 >= x2 >= y
               let !delta = x2 -# x1 -- <= 0
                   !y' = y -# delta  -- y <= y' <= x1; hence y' is representable

                   -- Invariant: x >= y
                   -- Note that: z >= y' => z + delta won't underflow
                   -- so we are guaranteed not to underflow if/when we recurse
                   go_dn x | isTrue# (x <# y') = I# x `c` n
                           | otherwise         = I# x `c` go_dn (x +# delta)
               in I# x1 `c` go_dn x2

{-# INLINE isTrue# #-}
isTrue# :: Int# -> Bool   -- See Note [Optimizing isTrue#]
isTrue# x = tagToEnum# x
