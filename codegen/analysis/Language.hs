{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module Analysis.Language

where


import PPrint
import Data.Coerce
import Data.Ord (comparing)
import Data.Char (isUpper)
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as M

type TyVar = String
type TyCon = String


data DataDef a = DDef
  { typName :: TyCon
  , typVars :: [String]
  , constrs :: [Constructor a]
  }
  deriving (Show)

data Constructor a = DCon
  { conName :: ID
  , conArgs :: [Type]
  }
  deriving (Show)

instance Eq (Constructor a) where
  (==) = (==) `on` conName
  
instance Ord (Constructor a) where
  compare = comparing conName

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
    
  deriving (Show)


newtype Prog a b = Prog { unprog :: ([ValDef' a b], [DataDef a]) }

data ValDef' a b = VDef
  { binding :: ID
  , rhsval  :: Expr' a b
  }
  deriving (Show)


data Expr' a b
  = LetRec { binds :: [ValDef' a b]
           , expr  :: Expr' a b
           , meta  :: b
           }
  | Lambda { parms :: [ID]
           , expr  :: Expr' a b
           , meta  :: b
           }
  | CaseOf { scrut :: Expr' a b
           , paths :: [Clause' a b]
           , meta  :: b
           }
  | Apply  { efun  :: Expr' a b 
           , earg  :: Expr' a b
           , meta  :: b
           }
  | Var    { name  :: ID
           , meta  :: b
           }
  | Lit    { lval  :: Literal
           , meta  :: b
           }
  deriving (Show)



unfoldAp e = go e [] []
  where go (Apply e1 e2 m) args metas = go e1 (e2:args) (m:metas)
        go ef args metas = (ef, args, metas)

data Literal = UBInt Int deriving (Show)

type ValDef a = ValDef' () a
type Expr a   = Expr' () a
type Clause a = Clause' () a

type Clause' a b = (Pattern, Expr' a b)

data Pattern = Pattern
  { patt :: ID   -- ^ could refer to a var or construct, both are uniquely identified
  , args :: [ID] -- ^ newly scoped variables matching constructor parameters
  }
  deriving (Show)

data ID = ID
  { occName :: String -- ^ similar to GHC: How the name occured in the program
  , uniq    :: Int    -- ^ The /real/ unique identifier for this element.
  }
  deriving (Show, Eq)

looksLikeCon :: ID -> Bool
looksLikeCon (occName -> x:_) = isUpper x
looksLikeCon _ = False

instance Ord ID where
  compare = comparing uniq


instance Unparse Type where
  unparse t = case t of
    TFun t1 t2 -> let wrap | TFun{} <- t1 = parens
                           | otherwise = id
                  in wrap (unparse t1) <+> arw <+> unparse t2
    TVar v     -> text v
    TPrim pt   -> unparse pt
    TApp c ts  -> text c <+> hsep (map maybeParen ts)
      where maybeParen t = case t of
              TFun{} -> parens (unparse t)
              TApp{} -> parens (unparse t)
              _      -> unparse t

-- Probably not going to expand on this, but who knows ...
data PrimType = PInt deriving (Show)

instance Unparse PrimType where
  unparse PInt = text "I#"



type SimpleProg a = Prog (a :-> Uniquified :-> SatCons :-> SatFuns)
type UniqProg a = Prog (a :-> Uniquified)

data a :-> b
data Uniquified
data SatFuns
data SatCons
  

ensureSimpleAST :: UniqProg a b -> SimpleProg a b
ensureSimpleAST = ensureSatFuns . ensureSatCons

ensureSatFuns :: Prog a b -> Prog (a :-> SatFuns) b
ensureSatFuns = undefined


-- Check to make sure all constructors are fully applied

ensureSatCons ::  Prog a b -> Prog (a :-> SatCons) b
ensureSatCons (Prog (vdefs, ddefs)) = Prog (newVDefs, newDDefs)
  where
    conArities = M.fromList $ concatMap (map getArity . constrs) ddefs
    getArity (DCon name args) = (name, length args)
    newDDefs = coerce ddefs -- don't need to do anything to datadefs
    newVDefs = map (satConBind conArities) vdefs


satConBind :: Map ID Int -> ValDef' a b ->  ValDef' (a :-> SatCons) b
satConBind arities (VDef b rhs) = VDef b $ satConExpr arities rhs

satConExpr :: Map ID Int -> Expr' a b -> Expr' (a :-> SatCons) b
satConExpr arities exp = case exp of
  LetRec binds ex meta -> LetRec newBinds newEx meta
    where newBinds = map (satConBind arities) binds
          newEx   = satConExpr arities ex

  Lambda parms expr meta ->
    Lambda parms (satConExpr arities expr) meta

  
  CaseOf scrut (unzip -> (pats, exps)) meta -> CaseOf newScrut newPaths meta
    where newScrut = satConExpr arities scrut
          newPaths = zip pats $ map (satConExpr arities) exps

  Apply efun earg ameta -> result
    where (Var name vmeta, eargs, metas) = unfoldAp exp
          newArgs = map (satConExpr arities) eargs
          zipped = zip newArgs metas
          toFold = init zipped
          ()
          folded = foldl (\arg (ap, m) -> Apply ap arg m) newBaseFun toFold
          newBaseFun = Var name vmeta
          newEFun = 
          result = if looksLikeCon name && saturated
                   then Apply folded ameta
                   else error $ show name ++
                        " is an unsaturated constructor application"
          saturated = undefined
    
