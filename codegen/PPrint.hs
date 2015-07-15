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
  listText,
  brackList,
  PPrint,
  module Text.PrettyPrint
) where
       

import Text.PrettyPrint
import qualified Data.Set as Set
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

brackList = brackets . hsep . punctuate comma
braceList = braces . hsep . punctuate comma

listText = brackList . map text


instance (PPrint a) => PPrint (Set.Set a) where
  toDoc s = braceList (map toDoc $ Set.toList s)

instance PPrint String where
  toDoc = text
