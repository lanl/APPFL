{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Num
-- Copyright   :  (c) The University of Glasgow 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'Num' class and the 'Integer' type.
--
-----------------------------------------------------------------------------

module APPFL.Num ( module APPFL.Num
--                 , module APPFL.Integer
                 ) where

import APPFL.Base
-- import APPFL.Integer

-- Ordinarily, the Num typeclass is defined here, but we
-- do want to really use the normal Num class, rather than
-- an APPFL parallel
-- import GHC.Num (Num (..))

infixl 7  *, /, %
infixl 6  +, -

default ()              -- Double isn't available yet,
                        -- and we shouldn't be using defaults anyway


(+) = addInt
(-) = subInt
(/) = quotInt
(*) = mulInt
(%) = remInt

mod = (%)
div = (/)
negate (I# i#) = I# (negateInt# i#)

addInt :: Int -> Int -> Int
I# a# `addInt` I# b# = I# (a# +# b#)

subInt :: Int -> Int -> Int
I# a# `subInt` I# b# = I# (a# -# b#)

mulInt :: Int -> Int -> Int
I# a# `mulInt` I# b# = I# (a# *# b#)

-- -- | the same as @'flip' ('-')@.
-- --
-- -- Because @-@ is treated specially in the Haskell grammar,
-- -- @(-@ /e/@)@ is not a section, but an application of prefix negation.
-- -- However, @('subtract'@ /exp/@)@ is equivalent to the disallowed section.
-- {-# INLINE subtract #-}
-- subtract :: (Num a) => a -> a -> a
-- subtract x y = y - x

-- instance  Num Int  where
--     I# x + I# y = I# (x +# y)
--     I# x - I# y = I# (x -# y)
--     negate (I# x) = I# (negateInt# x)
--     I# x * I# y = I# (x *# y)
--     abs n  = if n `geInt` 0 then n else negate n

--     signum n | n `ltInt` 0 = negate 1
--              | n `eqInt` 0 = 0
--              | otherwise   = 1

--     {-# INLINE fromInteger #-}   -- Just to be sure!
--     fromInteger i = I# (integerToInt i)

-- instance Num Word where
--     (W# x#) + (W# y#)      = W# (x# `plusWord#` y#)
--     (W# x#) - (W# y#)      = W# (x# `minusWord#` y#)
--     (W# x#) * (W# y#)      = W# (x# `timesWord#` y#)
--     negate (W# x#)         = W# (int2Word# (negateInt# (word2Int# x#)))
--     abs x                  = x
--     signum 0               = 0
--     signum _               = 1
--     fromInteger i          = W# (integerToWord i)

-- instance  Num Integer  where
--     (+) = plusInteger
--     (-) = minusInteger
--     (*) = timesInteger
--     negate         = negateInteger
--     fromInteger x  =  x

--     abs = absInteger
--     signum = signumInteger

