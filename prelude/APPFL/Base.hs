{-

This is the APPFL Base library, parallel to GHC.Base.

The current goal is to get a simple Int type working.

It is largely derived from the GHC.Base module, and will be
incrementally expanded as more Haskell features are enabled.  See the
APPFL Prelude for why this sort of hackery is necessary.  The more
detailed comments from GHC have been stripped, unless I thought they
were particularly useful (as the Prelude structure notes below).



The overall structure of the GHC Prelude is a bit tricky.

  a) We want to avoid "orphan modules", i.e. ones with instance
        decls that don't belong either to a tycon or a class
        defined in the same module

  b) We want to avoid giant modules

So the rough structure is as follows, in (linearised) dependency order


GHC.Prim        Has no implementation.  It defines built-in things, and
                by importing it you bring them into scope.
                The source file is GHC.Prim.hi-boot, which is just
                copied to make GHC.Prim.hi

GHC.Base        Classes: Eq, Ord, Functor, Monad
                Types:   list, (), Int, Bool, Ordering, Char, String

Data.Tuple      Types: tuples, plus instances for GHC.Base classes

GHC.Show        Class: Show, plus instances for GHC.Base/GHC.Tup types

GHC.Enum        Class: Enum,  plus instances for GHC.Base/GHC.Tup types

Data.Maybe      Type: Maybe, plus instances for GHC.Base classes

GHC.List        List functions

GHC.Num         Class: Num, plus instances for Int
                Type:  Integer, plus instances for all classes so far (Eq, Ord, Num, Show)

                Integer is needed here because it is mentioned in the signature
                of 'fromInteger' in class Num

GHC.Real        Classes: Real, Integral, Fractional, RealFrac
                         plus instances for Int, Integer
                Types:  Ratio, Rational
                        plus intances for classes so far

                Rational is needed here because it is mentioned in the signature
                of 'toRational' in class Real

GHC.ST  The ST monad, instances and a few helper functions

Ix              Classes: Ix, plus instances for Int, Bool, Char, Integer, Ordering, tuples

GHC.Arr         Types: Array, MutableArray, MutableVar

                Arrays are used by a function in GHC.Float

GHC.Float       Classes: Floating, RealFloat
                Types:   Float, Double, plus instances of all classes so far

                This module contains everything to do with floating point.
                It is a big module (900 lines)
                With a bit of luck, many modules can be compiled without ever reading GHC.Float.hi


Other Prelude modules are much easier with fewer complex dependencies.
-}

{-# LANGUAGE Unsafe #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , BangPatterns
           , ExplicitForAll
           , MagicHash
           , UnboxedTuples
           , ExistentialQuantification
           , RankNTypes
           , RebindableSyntax
  #-}

-- -fno-warn-orphans is needed for things like:
-- Orphan rule: "x# -# x#" ALWAYS forall x# :: Int# -# x# x# = 0
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}

#define WORD_SIZE_IN_BITS 64#

module APPFL.Base
  ( module APPFL.Base
  , module APPFL.Types
  , module APPFL.Prim
  , module APPFL.Classes
  )
where

import APPFL.Prim
import APPFL.Classes
import APPFL.Types


-- recognizable dummy definition for those things that require
-- it for well-typedness.
_dummy :: bottom
_dummy = let x = x in x

--import GHC.Tuple ()     -- Note [Depend on GHC.Tuple]
--import APPFL.Integer ()   -- Note [Depend on GHC.Integer]


-- infixr 9 .
-- infixr 0 $, $!


{-
Note [Depend on GHC.Integer]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Integer type is special because TidyPgm uses
GHC.Integer.Type.mkInteger to construct Integer literal values
Currently it reads the interface file whether or not the current
module *has* any Integer literals, so it's important that
GHC.Integer.Type (in patckage integer-gmp or integer-simple) is
compiled before any other module.  (There's a hack in GHC to disable
this for packages ghc-prim, integer-gmp, integer-simple, which aren't
allowed to contain any Integer literals.)

Likewise we implicitly need Integer when deriving things like Eq
instances.

The danger is that if the build system doesn't know about the dependency
on Integer, it'll compile some base module before GHC.Integer.Type,
resulting in:
  Failed to load interface for ‘GHC.Integer.Type’
    There are files missing in the ‘integer-gmp’ package,

Bottom line: we make GHC.Base depend on GHC.Integer; and everything
else either depends on GHC.Base, or does not have NoImplicitPrelude
(and hence depends on Prelude).

Note [Depend on GHC.Tuple]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Similarly, tuple syntax (or ()) creates an implicit dependency on
GHC.Tuple, so we use the same rule as for Integer --- see Note [Depend on
GHC.Integer] --- to explain this to the build system.  We make GHC.Base
depend on GHC.Tuple, and everything else depends on GHC.Base or Prelude.
-}









-- |'otherwise' is defined as the value 'True'.  It helps to make
-- guards more readable.  eg.
--
-- >  f x | x < 0     = ...
-- >      | otherwise = ...
otherwise               :: Bool
otherwise               =  True


----------------------------------------------
-- 'Int' related definitions
----------------------------------------------

-- Ordinarily, these are made machine-dependent with
-- CPP Macros, but APPFL is always 64-bit
-- maxInt, minInt :: Int
-- minInt  = I# (-0x8000000000000000#)
-- maxInt  = I# 0x7FFFFFFFFFFFFFFF#


--------------------------------------------------
-- RebindableSyntax (re)bindings
--
-- block comments are taken from the GHC manual
--------------------------------------------------

{-
An integer literal 368 means “fromInteger (368::Integer)”,
rather than “Prelude.fromInteger (368::Integer)”.
-}
-- This is a dummy definition. Integer literals will be
-- hacked out of the AST into ours
--fromInteger :: Integer -> Int
fromInteger = _dummy

{-
Fractional literals are handed in just the same way, except
that the translation is fromRational (3.68::Rational).
-}                           

{-
The equality test in an overloaded numeric pattern uses
whatever (==) is in scope.
-}

{-
The subtraction operation, and the greater-than-or-equal test,
in n+k patterns use whatever (-) and (>=) are in scope.
-}

{-
Negation (e.g. “- (f x)”) means “negate (f x)”, both in numeric
patterns, and expressions.
-}

{-
Conditionals (e.g. “if e1 then e2 else e3”) means “ifThenElse
e1 e2 e3”. However case expressions are unaffected.
-}

ifThenElse :: Bool -> a -> a -> a
ifThenElse b t e | b         = t
                 | otherwise = e

{-                           
“Do” notation is translated using whatever functions (>>=),
(>>), and fail, are in scope (not the Prelude versions). List
comprehensions, mdo (The recursive do-notation), and parallel
array comprehensions, are unaffected.
-}

{-
Arrow notation (see Arrow notation) uses whatever arr, (>>>),
first, app, (|||) and loop functions are in scope. But unlike
the other constructs, the types of these functions must match
the Prelude types very closely. Details are in flux; if you
want to use this, ask!
-}



----------------------------------------------
-- The function type
----------------------------------------------

-- -- | Identity function.
-- id                      :: a -> a
-- id x                    =  x

-- -- | Constant function.
-- const                   :: a -> b -> a
-- const x _               =  x

-- -- | Function composition.
-- {-# INLINE (.) #-}
-- -- Make sure it has TWO args only on the left, so that it inlines
-- -- when applied to two functions, even if there is no final argument
-- (.)    :: (b -> c) -> (a -> b) -> a -> c
-- (.) f g = \x -> f (g x)

-- -- | @'flip' f@ takes its (first) two arguments in the reverse order of @f@.
-- flip                    :: (a -> b -> c) -> b -> a -> c
-- flip f x y              =  f y x

-- -- | Application operator.  This operator is redundant, since ordinary
-- -- application @(f x)@ means the same as @(f '$' x)@. However, '$' has
-- -- low, right-associative binding precedence, so it sometimes allows
-- -- parentheses to be omitted; for example:
-- --
-- -- >     f $ g $ h x  =  f (g (h x))
-- --
-- -- It is also useful in higher-order situations, such as @'map' ('$' 0) xs@,
-- -- or @'Data.List.zipWith' ('$') fs xs@.
-- {-# INLINE ($) #-}
-- ($)                     :: (a -> b) -> a -> b
-- f $ x                   =  f x

-- -- | Strict (call-by-value) application operator. It takes a function and an
-- -- argument, evaluates the argument to weak head normal form (WHNF), then calls
-- -- the function with that value.

-- ($!)                    :: (a -> b) -> a -> b
-- f $! x                  = let !vx = x in f vx  -- see #2273

-- -- | @'until' p f@ yields the result of applying @f@ until @p@ holds.
-- until                   :: (a -> Bool) -> (a -> a) -> a -> a
-- until p f = go
--   where
--     go x | p x          = x
--          | otherwise    = go (f x)

-- -- | 'asTypeOf' is a type-restricted version of 'const'.  It is usually
-- -- used as an infix operator, and its typing forces its first argument
-- -- (which is usually overloaded) to have the same type as the second.
-- asTypeOf                :: a -> a -> a
-- asTypeOf                =  const


-- {- |
-- Returns the 'tag' of a constructor application; this function is used
-- by the deriving code for Eq, Ord and Enum.

-- The primitive dataToTag# requires an evaluated constructor application
-- as its argument, so we provide getTag as a wrapper that performs the
-- evaluation before calling dataToTag#.  We could have dataToTag#
-- evaluate its argument, but we prefer to do it this way because (a)
-- dataToTag# can be an inline primop if it doesn't need to do any
-- evaluation, and (b) we want to expose the evaluation to the
-- simplifier, because it might be possible to eliminate the evaluation
-- in the case when the argument is already known to be evaluated.
-- -}
-- {-# INLINE getTag #-}
-- getTag :: a -> Int#
-- getTag !x = dataToTag# x

----------------------------------------------
-- Numeric primops
----------------------------------------------

-- Definitions of the boxed PrimOps; these will be
-- used in the case of partial applications, etc.

{-# INLINE quotInt #-}
{-# INLINE remInt #-}

quotInt, remInt, divInt, modInt :: Int -> Int -> Int
(I# x) `quotInt`  (I# y) = I# (x /# y)
(I# x) `remInt`   (I# y) = I# (x %# y)
(I# x) `divInt`   (I# y) = I# (x `divInt#` y)
(I# x) `modInt`   (I# y) = I# (x `modInt#` y)


quotRemInt :: Int -> Int -> (Int, Int)
(I# x) `quotRemInt` (I# y) = case x `quotRemInt#` y of
                             (# q, r #) ->
                                 (I# q, I# r)

divModInt :: Int -> Int -> (Int, Int)
(I# x) `divModInt` (I# y) = case x `divModInt#` y of
                            (# q, r #) -> (I# q, I# r)

divModInt# :: Int# -> Int# -> (# Int#, Int# #)
x# `divModInt#` y#
 | isTrue# (x# ># 0#) && isTrue# (y# <# 0#) =
                                    case (x# -# 1#) `quotRemInt#` y# of
                                      (# q, r #) -> (# q -# 1#, r +# y# +# 1# #)
 | isTrue# (x# <# 0#) && isTrue# (y# ># 0#) =
                                    case (x# +# 1#) `quotRemInt#` y# of
                                      (# q, r #) -> (# q -# 1#, r +# y# -# 1# #)
 | otherwise                                =
                                    x# `quotRemInt#` y#



--- Wrappers for the shift operations.  The uncheckedShift# family are
--- undefined when the amount being shifted by is greater than the size
--- in bits of Int#, so these wrappers perform a check and return
--- either zero or -1 appropriately.
---
--- Note that these wrappers still produce undefined results when the
--- second argument (the shift amount) is negative.

-- -- | Shift the argument left by the specified number of bits
-- -- (which must be non-negative).
-- shiftL# :: Word# -> Int# -> Word#
-- a `shiftL#` b   | isTrue# (b >=# WORD_SIZE_IN_BITS) = 0##
--                 | otherwise                          = a `uncheckedShiftL#` b

-- -- | Shift the argument right by the specified number of bits
-- -- (which must be non-negative).
-- -- The "RL" means "right, logical" (as opposed to RA for arithmetic)
-- -- (although an arithmetic right shift wouldn't make sense for Word#)
-- shiftRL# :: Word# -> Int# -> Word#
-- a `shiftRL#` b  | isTrue# (b >=# WORD_SIZE_IN_BITS) = 0##
--                 | otherwise                          = a `uncheckedShiftRL#` b

-- -- | Shift the argument left by the specified number of bits
-- -- (which must be non-negative).
-- iShiftL# :: Int# -> Int# -> Int#
-- a `iShiftL#` b  | isTrue# (b >=# WORD_SIZE_IN_BITS) = 0#
--                 | otherwise                          = a `uncheckedIShiftL#` b

-- -- | Shift the argument right (signed) by the specified number of bits
-- -- (which must be non-negative).
-- -- The "RA" means "right, arithmetic" (as opposed to RL for logical)
-- iShiftRA# :: Int# -> Int# -> Int#
-- a `iShiftRA#` b | isTrue# (b >=# WORD_SIZE_IN_BITS) = if isTrue# (a <# 0#)
--                                                           then (-1#)
--                                                           else 0#
--                 | otherwise                          = a `uncheckedIShiftRA#` b

-- -- | Shift the argument right (unsigned) by the specified number of bits
-- -- (which must be non-negative).
-- -- The "RL" means "right, logical" (as opposed to RA for arithmetic)
-- iShiftRL# :: Int# -> Int# -> Int#
-- a `iShiftRL#` b | isTrue# (b >=# WORD_SIZE_IN_BITS) = 0#
--                 | otherwise                          = a `uncheckedIShiftRL#` b


#ifdef __HADDOCK__
-- | A special argument for the 'Control.Monad.ST.ST' type constructor,
-- indexing a state embedded in the 'Prelude.IO' monad by
-- 'Control.Monad.ST.stToIO'.
data RealWorld
#endif
