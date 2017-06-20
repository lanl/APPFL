module Analysis.Language

where


type TyVar = String
type TyCon = String


data DataDef = DDef
  { typName :: TyCon
  , typVars :: [String]
  , constrs :: [Constructor]
  }

data Constructor = DCons
  { conName :: String
  , conArgs :: [Type]
  }



data Type
  = TFun Type Type
    -- ^ Function type: @t1 -> t2@. Could also be thought of as (TApp (TApp "->" t1) t2).

  | TVar TyVar
    -- ^ polymorphic type variable.
    
  | TPrim PrimType
    -- ^ Primitive types.
    
  | TApp TyCon [Type]
    -- ^ Type application. Not sure if it's worth separating out a constructor
    -- for non-parameterized types or just allowing things like @TApp "Bool" []@


-- Probably not going to expand on this, but who knows ...
data PrimType = PInt
