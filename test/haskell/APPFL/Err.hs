{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Err
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- The "GHC.Err" module defines the code for the wired-in error functions,
-- which have a special type in the compiler (with \"open tyvars\").
--
-- We cannot define these functions in a module where they might be used
-- (e.g., "GHC.Base"), because the magical wired-in type will get confused
-- with what the typechecker figures out.
--
-----------------------------------------------------------------------------

module APPFL.Err( absentErr, error, undefined ) where
import APPFL.CString ()
import APPFL.Types
import GHC.Prim
import APPFL.Integer ()   -- Make sure Integer is compiled first
                        -- because GHC depends on it in a wired-in way
                        -- so the build system doesn't see the dependency
--import {-# SOURCE #-} GHC.Exception( errorCallException )

-- | 'error' stops execution and displays an error message.
--error :: [Char] -> a
error s = raise# s --(errorCallException s)
-- Might be able to support this is some way.
-- At the very least, making this simply a primitive call makes this
-- easier to detect and handle at the STG level

-- | A special case of 'error'.
-- It is expected that compilers will recognize this and insert error
-- messages which are more appropriate to the context in which 'undefined'
-- appears.
undefined :: a
undefined =  error "Prelude.undefined"

-- | Used for compiler-generated error message;
-- encoding saves bytes of string junk.
absentErr :: a
absentErr = error "Oops! The program has entered an `absent' argument!\n"
