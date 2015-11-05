{-# LANGUAGE FlexibleInstances #-}
-- | The home of all the orphaned 'IsString' instances
-- used throughout the DSL.
module Language.C.DSL.StringLike where
import Data.String
import Language.C
instance IsString Ident where
  fromString = internalIdent
instance IsString CExpr where
  fromString s = CVar (fromString s) undefNode
instance IsString CDeclr where
  fromString str = CDeclr (Just $ fromString str) [] Nothing [] undefNode
instance IsString CDecl where
  fromString str = CDecl [CTypeSpec (CTypeDef (fromString str) undefNode)] [] undefNode
instance IsString CTypeSpec where
  fromString = flip CTypeDef undefNode . fromString
