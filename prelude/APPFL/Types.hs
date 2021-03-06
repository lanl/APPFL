{-# LANGUAGE MagicHash, NoImplicitPrelude, TypeFamilies, UnboxedTuples,
             RoleAnnotations #-}

{-
This module replaces GHC.Types, which cannot be used as a true source
file since it attempts to override builtin syntax.  For APPFL, this
provides the base types that *are* legal Haskell as well as pseudonyms
for [] and () types.

If we ever provide support for large tuples, we'll want to do something
similar (probably via script, like GHC)
-}

module APPFL.Types
  ( Bool(..), Int (..), Char (..), Word (..), Ordering (..)
  , isTrue#
  ) where

import GHC.Types as GHC
  (Int (..), Bool (..), Char (..),
   Word (..), Ordering(..))
  
import APPFL.Prim


isTrue# :: Int# -> GHC.Bool   
isTrue# 1# = GHC.True
isTrue# _  = GHC.False
-- The old (optimized for GHC codegen) version used tagToEnum#. We could do something
-- like this if we have issues with the above definitions, but for now they're fine.
-- See Note [Optimizing isTrue#] in GHC.Types (ghc-prim package)


-- newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))
-- type role IO representational

-- | This two-parameter class has instances for types @a@ and @b@ if
--      the compiler can infer that they have the same representation. This class
--      does not have regular instances; instead they are created on-the-fly during
--      type-checking. Trying to manually declare an instance of @Coercible@
--      is an error.
--
--      Nevertheless one can pretend that the following three kinds of instances
--      exist. First, as a trivial base-case:
--
--      @instance a a@
--
--      Furthermore, for every type constructor there is
--      an instance that allows to coerce under the type constructor. For
--      example, let @D@ be a prototypical type constructor (@data@ or
--      @newtype@) with three type arguments, which have roles @nominal@,
--      @representational@ resp. @phantom@. Then there is an instance of
--      the form
--
--      @instance Coercible b b\' => Coercible (D a b c) (D a b\' c\')@
--
--      Note that the @nominal@ type arguments are equal, the
--      @representational@ type arguments can differ, but need to have a
--      @Coercible@ instance themself, and the @phantom@ type arguments can be
--      changed arbitrarily.
--
--      The third kind of instance exists for every @newtype NT = MkNT T@ and
--      comes in two variants, namely
--
--      @instance Coercible a T => Coercible a NT@
--
--      @instance Coercible T b => Coercible NT b@
--
--      This instance is only usable if the constructor @MkNT@ is in scope.
--
--      If, as a library author of a type constructor like @Set a@, you
--      want to prevent a user of your module to write
--      @coerce :: Set T -> Set NT@,
--      you need to set the role of @Set@\'s type parameter to @nominal@,
--      by writing
--
--      @type role Set nominal@
--
--      For more details about this feature, please refer to
--      <http://www.cis.upenn.edu/~eir/papers/2014/coercible/coercible.pdf Safe Coercions>
--      by Joachim Breitner, Richard A. Eisenberg, Simon Peyton Jones and Stephanie Weirich.
--
--      @since 4.7.0.0
--data Coercible a b = MkCoercible ((~#) a b)
-- It's really ~R# (representational equality), not ~#,
-- but  * we don't yet have syntax for ~R#,
--      * the compiled code is the same either way
--      * TysWiredIn has the truthful types
-- Also see Note [Kind-changing of (~) and Coercible]

