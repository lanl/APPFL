{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

module PPrint
(
  -- symbol Docs
  bar, hash, arw, doubleColon, lambda, underscore,

  -- put Doc in a line/block comment
  lcomment, bcomment,

  -- turn a boolean into a Doc
  boolean,

  -- make an Int64 doc
  int64,

  -- List pretty printers
  listText, brackList,
  prepunctuate, postpunctuate,
  vertList,

  -- getting the original STG names with '#' characters
  --reHash,
  stgName,


  -- Pretty Printer typeclasses
  PPrint(..),
  Unparse(..),

  -- Re-export the HughesPJ lib
  module Text.PrettyPrint
) where


import Text.PrettyPrint
import Text.Show.Pretty (ppDoc)
import qualified Data.Set as Set
import Data.List (isSuffixOf)
import Data.Int (Int64)


--------------------------- Pretty Printing -------------------------

class PPrint a where
  pprint :: a -> Doc

class Unparse a where
  unparse :: a -> Doc

{-
reHash :: String -> String
reHash str | "_h" `isSuffixOf` str = reverse ('#' : drop 2 (reverse str))
           | otherwise = str
-}

stgName :: String -> Doc
stgName = text

nLines :: (Show a) => a -> Int
nLines = length . lines . show

bar, arw, hash, doubleColon, lambda, underscore :: Doc
lcomment, bcomment :: Doc -> Doc

underscore = char '_'
bar = char '|'
arw = text "->"
hash = char '#'
doubleColon = text "::"
lambda = text "\\"
lcomment d = text "--" <> d
bcomment d | isEmpty d = empty
           | otherwise =
             -- only line break for long comments
             let sepr | nLines d == 1 = (<+>)
                      | otherwise = ($+$)
             in text "{-" `sepr` (nest 3 d) `sepr` text "-}"




boolean :: Bool -> Doc
boolean = ppDoc

int64 :: Int64 -> Doc
int64 = ppDoc

brackList, braceList :: [Doc] -> Doc
brackList = brackets . hsep . punctuate comma
braceList = braces . hsep . punctuate comma

prepunctuate, postpunctuate :: Doc -> [Doc] -> [Doc]
prepunctuate d = map (d <>)
postpunctuate d = map (<> d)

listText :: [String] -> Doc
listText = brackList . map text


-- | PrettyPrint a list of printable things, chopping the list onto newlines
-- if necessary.
vertList
  :: PPrint t
  => Doc -- | Left list delimiter Doc
  -> Doc -- | Right list delimiter Doc
  -> Doc -- | Item separator Doc
  -> Int -- | Max line length before chopping
  -> [t] -- | Things to print
  -> Doc
vertList lchr rchr sepr maxlen ls =
  let (_,d) = vl 0 ls
  in lchr <> d
  where vl _ [] = ((<>), rchr)
        vl n (x:xs) =
          let xd = pprint x <> (if null xs then empty else sepr)
              l = length (show xd) + n
              newl = l > maxlen
              (f, xsd) = vl (if newl then 0 else l) xs
          in if newl
             then (($$), f xd xsd)
             else ((<+>), f xd xsd)


instance {-# OVERLAPPABLE #-} (Show a) => PPrint a where
  pprint = ppDoc

-- () metadata = empty document
-- empty is the identity Doc for the associative pretty printing operators
instance Unparse () where
  unparse () = empty

instance PPrint Doc where
  pprint = id

instance (PPrint a) => PPrint (Set.Set a) where
  pprint s = braceList (map pprint $ Set.toList s)

instance {-# OVERLAPPING #-} PPrint String where
  pprint = text

instance (PPrint a) => PPrint [a] where
  pprint = vertList lbrack rbrack comma 10

instance {-# OVERLAPPABLE#-} (PPrint a, PPrint b) => PPrint (a,b) where
  pprint (a,b) = parens (pprint a <> comma <+> pprint b)

instance PPrint a => PPrint (Maybe a) where
  pprint (Just a) = text "Just" <+> pprint a
  pprint Nothing = text "Nothing"

instance PPrint () where
  pprint () = empty

instance PPrint Char where
  pprint = char
