{-
This module defines the primitive operations and supported by APPFL and
some helper functions that are primitive in nature, if not operation.

 * Some are already part of GHC.Prim and re-exported.
 * Some are aliased to GHC.Prim definitions for convenience.
 * Some are defined in terms of other primops to avoid rewriting functions
   that depend on them.

All modules using them should import from this module to ensure
no unsuported operations are used.
-}

{-# LANGUAGE MagicHash, UnboxedTuples, NoImplicitPrelude #-}

module APPFL.Prim
  ( module APPFL.Prim
  , module GHC.Prim
  )
where

import APPFL.VOID (VOID(..))

import GHC.Prim
  ( Char#, Int#, Word#, Float#, Double#, Addr#
  , (+#), (-#), (*#)
  , (>#), (<#), (>=#), (<=#)
  , (==#), (/=#)
  , quotInt#, remInt#, negateInt#
  , eqChar#, neChar#, gtChar#, geChar#, leChar#, ltChar#
  , eqWord#, neWord#, gtWord#, geWord#, leWord#, ltWord#
  , indexCharOffAddr#, chr#, ord#
  , uncheckedIShiftL#, uncheckedIShiftRA#, uncheckedIShiftRL#

  -- These are used when GHC derives instances of Eq, Ord and Enum
  -- We don't *need* to support this, but the implementation of these
  -- primitives should be simple.  GHC creates tags just as APPFL does:
  -- from 0 to n-1 for n constructors.
  -- tagToEnum# is only used for enumerated types, and any attempt to use it
  -- inappropriately is caught at the type checker
  , dataToTag#, tagToEnum#
  )


-- Aliases

infixl 7 %#, /#

-- | Modulus operator on unboxed Ints.
--   Non-haskelly syntax, just because
(%#) :: Int# -> Int# -> Int#
{-# INLINE (%#) #-}
a %# b = a `remInt#` b

-- | Truncating division operator on unboxed Ints.
(/#) :: Int# -> Int# -> Int#
{-# INLINE (/#) #-}
a /# b = a `quotInt#` b

-- | Negate an unboxed int.
ineg# :: Int# -> Int#
{-# INLINE ineg# #-}
ineg# a = negateInt# a


-- Primops not supported by GHC

-- | Return the greater of two unboxed ints
--   This definition may cause issues...
imax# :: Int# -> Int# -> Int#
imax# = let x = x in x


-- | Return the lesser of two unboxed ints
--   This definition may cause issues...
imin# :: Int# -> Int# -> Int#
imin# = let x = x in x


-- Implemented Primops/Ids

-- | Calculate quotient and remainder of a division operation.
--   (C-style divison and modulus)
quotRemInt# :: Int# -> Int# -> (# Int#, Int# #)
a `quotRemInt#` b =
  case a /# b of
    q -> case a %# b of
           r -> (# q, r #)

-- | COMPAT: Probably never actually used as a value, but might need
-- to be evaluated in the strict case.  See note in FromGHC.
void# :: VOID
void# = VOID
