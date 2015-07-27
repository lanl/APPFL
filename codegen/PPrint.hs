{-# LANGUAGE
FlexibleInstances,
TypeSynonymInstances,
NamedFieldPuns,
OverlappingInstances #-}


module PPrint
(
  pprint,
  unparse,
  bar,
  arw,
  doubleColon,
  lambda,
  lcomment,
  bcomment,
  boolean,
  listText,
  brackList,
  prepunctuate,
  vertList,
  PPrint,
  Unparse,
  module Text.PrettyPrint
) where
       

import Text.PrettyPrint
import qualified Data.Set as Set
import Data.List (find)



--------------------------- Pretty Printing -------------------------

class PPrint a where
  pprint :: a -> Doc

class Unparse a where
  unparse :: a -> Doc


bar = text "|"
arw = text "->"
doubleColon = text "::"
lambda = text "\\"
lcomment d = text "--" <> d
bcomment d = text "{-" <+> (nest 3 d) $+$ text "-}"

boolean :: Bool -> Doc
boolean = text . show

brackList = brackets . hsep . punctuate comma
braceList = braces . hsep . punctuate comma

listText = brackList . map text

prepunctuate d = map (d<>)

vertList lchr rchr sepr maxlen xs =
  let (_,d) = vl 0 xs
  in lchr <> d
  where vl _ [] = ((<>), rchr)
        vl n (x:xs) =
          let xd = pprint x
              l = length (show xd) + n
              newl = l > maxlen
              (f, xsd) = vl (if newl then 0 else l) xs
              xsd' = xsd <> sepr
          in if newl
             then (($$), f xd xsd')
             else ((<+>), f xd xsd')


instance PPrint Doc where
  pprint = id

instance (PPrint a) => PPrint (Set.Set a) where
  pprint s = braceList (map pprint $ Set.toList s)

instance PPrint String where
  pprint = text

instance (PPrint a) => PPrint [a] where
  pprint = vertList lbrack rbrack comma 10
