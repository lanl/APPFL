{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module PPrint
(
  toDoc,
  bar,
  arw,
  lcomment,
  bcomment,
  boolean,
  PPrint,
  module Text.PrettyPrint
) where
       

import Text.PrettyPrint
import qualified Data.Map as Map (toList)
import Data.List (find)



--------------------------- Pretty Printing -------------------------

class PPrint a where
  toDoc :: a -> Doc

bar = text "|"
arw = text "->"
lcomment d = text "--" <> d
bcomment d = text "{-" <+> (nest 3 d) $+$ text "-}"

boolean :: Bool -> Doc
boolean = text . show
