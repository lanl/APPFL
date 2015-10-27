{-# LANGUAGE OverloadedStrings #-}
-- | This module provides a more pleasant way to write C ASTs for language-c
-- As a simple example,
-- 
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Language.C.DSL
-- >
-- > example :: CFunDef
-- > example =
-- >   fun [intTy] "foo"[int "a", int "b"] $ block [
-- >       creturn $ "a" + "b"
-- >   ]
-- >
-- 
-- And when loaded into GHCi
-- 
-- > Main*> pretty example
-- > int foo(int a, int b)
-- > {
-- >   return a + b;
-- > }
-- 
-- This module also exports "Language.C" for simplicity.
module Language.C.DSL (
  module Language.C.DSL.StringLike,
  module Language.C.DSL.Exp,
  module Language.C.DSL.Stat,
  module Language.C.DSL.Decl,
  module Language.C) where

import Language.C
import Language.C.DSL.StringLike
import Language.C.DSL.Exp
import Language.C.DSL.Stat
import Language.C.DSL.Decl
