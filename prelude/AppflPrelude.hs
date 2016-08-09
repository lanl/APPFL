{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Prelude
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The Prelude: a standard module. The Prelude is imported by default
-- into all Haskell modules unless either there is an explicit import
-- statement for it, or the NoImplicitPrelude extension is enabled.
--
-----------------------------------------------------------------------------

module AppflPrelude (

    -- * Standard types, classes and related functions

    -- ** Basic data types
    Bool(False, True),
    (&&), (||), not, otherwise, ifThenElse,

    _dummy,
    -- Maybe(Nothing, Just),
    -- maybe,

    -- Either(Left, Right),
    -- either,

    -- Ordering(LT, EQ, GT),
    -- Char, String,

    -- -- *** Tuples
    fst, snd, curry, uncurry,

    -- -- ** Basic type classes

    {-(==), (/=),-} (<), (<=), (>=), (>), --- INT ONLY (FOR NOW)
 
    -- Eq((==), (/=)),
    -- Ord(compare, (<), (<=), (>=), (>), max, min),
    -- Enum(succ, pred, toEnum, fromEnum, enumFrom, enumFromThen,
    --      enumFromTo, enumFromThenTo),
    -- Bounded(minBound, maxBound),

    -- ** Numbers

    -- *** Numeric types
    Int(..), (+), (-), (*), (/), mod, div, negate,
    fromInteger,
    Char (..) ,
    Eq(..),
    -- Integer, Float, Double,
    -- Rational, Word,


    -- -- *** Numeric type classes
    -- Num((+), (-), (*), negate, abs, signum, fromInteger),
    -- Real(toRational),
    -- Integral(quot, rem, div, mod, quotRem, divMod, toInteger),
    -- Fractional((/), recip, fromRational),
    -- Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
    --          asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh),
    -- RealFrac(properFraction, truncate, round, ceiling, floor),
    -- RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
    --           encodeFloat, exponent, significand, scaleFloat, isNaN,
    --           isInfinite, isDenormalized, isIEEE, isNegativeZero, atan2),

    -- -- *** Numeric functions
    -- subtract, even, odd, gcd, lcm, (^), (^^),
    -- fromIntegral, realToFrac,

{----------------------------------------------------------------------

This may make it in later

    -- ** Monoids
    Monoid(mempty, mappend, mconcat),

    -- ** Monads and functors
    Functor(fmap, (<$)), (<$>),
    Applicative(pure, (<*>), (*>), (<*)),
    Monad((>>=), (>>), return, fail),
    mapM_, sequence_, (=<<),

    -- ** Folds and traversals
    Foldable(elem,      -- :: (Foldable t, Eq a) => a -> t a -> Bool
             -- fold,   -- :: Monoid m => t m -> m
             foldMap,   -- :: Monoid m => (a -> m) -> t a -> m
             foldr,     -- :: (a -> b -> b) -> b -> t a -> b
             -- foldr', -- :: (a -> b -> b) -> b -> t a -> b
             foldl,     -- :: (b -> a -> b) -> b -> t a -> b
             -- foldl', -- :: (b -> a -> b) -> b -> t a -> b
             foldr1,    -- :: (a -> a -> a) -> t a -> a
             foldl1,    -- :: (a -> a -> a) -> t a -> a
             maximum,   -- :: (Foldable t, Ord a) => t a -> a
             minimum,   -- :: (Foldable t, Ord a) => t a -> a
             product,   -- :: (Foldable t, Num a) => t a -> a
             sum),      -- :: Num a => t a -> a
             -- toList) -- :: Foldable t => t a -> [a]

    Traversable(traverse, sequenceA, mapM, sequence),
----------------------------------------------------------------------}
    -- -- ** Miscellaneous functions
    id, const, (.), flip, ($),
    --until,
    -- asTypeOf, error, undefined,
    -- seq, ($!),

    -- -- * List operations
    foldr, foldr1, foldl, foldl1,  -- for now maybe do foldable later
    maximum, minimum,              -- int only for now...
    map, (++), filter,

    head, last, tail, init, null, length, (!!),
    reverse,
    -- -- *** Special folds
    and, or, any, all,
    concat,
    concatMap,
    -- -- ** Building lists
    -- -- *** Scans
    scanl, scanl1, scanr, scanr1,
    -- -- *** Infinite lists
    iterate, repeat, replicate, cycle,
    -- -- ** Sublists
    take, drop, splitAt, takeWhile, dropWhile, span,
    break,
    -- -- ** Searching lists
    elem, notElem,
    lookup,
    -- -- ** Zipping and unzipping lists
    -- zip, zip3, zipWith,
    zipWith3, unzip, unzip3,
    -- -- ** Functions on strings
    -- lines, words, unlines, unwords,

    -- -- * Converting to and from @String@
    -- -- ** Converting to @String@
    -- ShowS,
    -- Show(showsPrec, showList, show),
    -- shows,
    -- showChar, showString, showParen,


{----------------------------------------------------------------------
    Read typeclass stuff. Has lots of dependencies.

    -- ** Converting from @String@
    ReadS,
    Read(readsPrec, readList),
    reads, readParen, read, lex,
----------------------------------------------------------------------}

{----------------------------------------------------------------------
No IO Support

    -- * Basic Input and output
    IO,
    -- ** Simple I\/O operations
    -- All I/O functions defined here are character oriented.  The
    -- treatment of the newline character will vary on different systems.
    -- For example, two characters of input, return and linefeed, may
    -- read as a single newline character.  These functions cannot be
    -- used portably for binary I/O.
    -- *** Output functions
    putChar,
    putStr, putStrLn, print,
    -- *** Input functions
    getChar,
    getLine, getContents, interact,
    -- *** Files
    FilePath,
    readFile, writeFile, appendFile, readIO, readLn,
    -- ** Exception handling in the I\/O monad
    IOError, ioError, userError,
----------------------------------------------------------------------}

  ) where

-- import Control.Monad
-- import System.IO
-- import System.IO.Error
-- import Data.List
-- import Data.Either
-- import Data.Foldable    ( Foldable(..) )
-- import Data.Functor     ( (<$>) )
-- import Data.Maybe
-- import Data.Traversable ( Traversable(..) )
-- import Data.Tuple

-- import GHC.Base hiding ( foldr, mapM, sequence )
-- import Text.Read
-- import GHC.Enum
-- import GHC.Num
-- import GHC.Real
-- import GHC.Float
-- import GHC.Show

{-
The Prelude Problem:

GHC does not reparse and compile the Prelude dependencies every time
it's invoked.  It seems to know where it can find the compiled object
files and link them with the user code.  APPFL has no such
functionality: it needs the full program, less builtin primops, with each
invocation.

A simple solution to this would be to compile without the implicit
Prelude, or to redefine it in terms of our own base libraries,
borrowing the source files from GHC.  GHC will use a custom Prelude
file in place of the normal one, provided the custom Prelude is in the
directory where the target source file is located.  If that Prelude is
defined in terms of similarly local imports, the entire program will
be compiled each time.

But, there are some syntactic constructs that desugar to code
dependent on the GHC base libraries (do blocks, list comprehensions,
list ranges, numeric literals).  There is, as far as I can tell, no
way around this dependency.

The 1:1 solution would be to implement these dependencies in STG code
and pull them into the program as necessary, but that's annoying and
would certainly be prone to bugs.

The "simpler" solution is a little silly: Provide a qualified
implementation of the base functions that parallels GHC's and import
them.  GHC will produce their STG, regardless of whether they're
actually referenced in code.  The translation from GHC STG to APPFL
STG must then handle the substitution.

-}

-- Hack.
-- Maybe better to autogen tuples as needed when traversing AST


import APPFL.Base
import APPFL.Num
import APPFL.List
import APPFL.Tuple
