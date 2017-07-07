{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LiberalTypeSynonyms   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}


module Analysis.Language

where


import           Control.Monad.State
import           Data.Char           (isUpper)
import           Data.Coerce
import           Data.Function       (on)
import           Data.List           (intercalate)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Ord            (comparing)
import qualified GHC.Exts            as GHC (Constraint)

import           PPrint


type TyVar = String
type TyCon = String


data DataDef a = DDef
  { dType   :: Type
  , constrs :: [Constructor a]
  }

instance Show (DataDef a) where
  showsPrec _ (DDef ty cons) =
    showString $
    "DDef {dType = " ++ show ty ++
    ", constrs=[" ++ intercalate ", " conStrs ++ "]}"
    where
          conStrs = map mkConStr cons

mkConStr :: Constructor a -> String
mkConStr (DCon id tys _) = "DCon {conName = " ++ show id ++
                           ", conArgs = " ++ show tys ++ "}"

data Constructor a = DCon
  { conName :: ID
  , conArgs :: [Type]
  , def     :: DataDef a -- reference to the definition which this constructor
                         -- is a part of is circular, but awfully convenient.
  }

instance Show (Constructor a) where
  showsPrec _ d@(DCon id tys (DDef ty _)) = showString $ mkConStr d


instance Eq (Constructor a) where
  (==) = (==) `on` conName

instance Ord (Constructor a) where
  compare = comparing conName


-- It might be fun to use GADTs here for stronger guarantees about Types:
-- 
-- > data Poly
-- > data Mono
-- > type MonoType = Type Mono
-- > type PolyType = Type Poly
-- > data Type a where
-- >   TFun    :: MonoType -> MonoType   -> MonoType
-- >   TVar    :: TyVar                  -> MonoType
-- >   TPrim   :: PrimType               -> MonoType
-- >   TApp    :: TyVar    -> [MonoType] -> MonoType
-- >   TForall :: [TyVar]  -> MonoType   -> PolyType

data Type
  = TFun Type Type
    -- ^ Function type: @t1 -> t2@. Could also be thought of as (TApp (TApp "->"
    -- t1) t2).

  | TVar TyVar
    -- ^ polymorphic type variable.

  | TPrim PrimType
    -- ^ Primitive types.

  | TApp TyCon [Type]
    -- ^ Type application. Not sure if it's worth separating out a constructor
    -- for non-parameterized types or just allowing things like @TApp "Bool" []@

  | TForall [TyVar] Type

  deriving (Show, Eq, Ord)

finalResTy :: Type -> Type
finalResTy t = case t of
  TForall _ t' -> finalResTy t'
  TFun    _ t' -> finalResTy t'
  _            -> t

instance Unparse Type where
  unparse t = case t of
    TFun t1 t2 -> let wrap | TFun{} <- t1 = parens
                           | otherwise = id
                  in wrap (unparse t1) <+> arw <+> unparse t2
    TVar v     -> text v
    TPrim pt   -> unparse pt
    TForall vs t -> text "forall" <+> hsep (map text vs)
                    <+> char '.' <+> unparse t
    TApp c ts  -> text c <+> hsep (map maybeParen ts)
      where maybeParen t = case t of
              TFun{} -> parens (unparse t)
              TApp{} -> parens (unparse t)
              _      -> unparse t

-- Probably not going to expand on this, but who knows ...
data PrimType = PInt deriving (Show, Eq, Ord)

instance Unparse PrimType where
  unparse PInt = text "I#"


type SimpleProg a = Prog (a :-> Uniquify :-> SatCons :-> SatFuns)
type TypedProg a  = Prog (a :-> InferTypes)

type Saturated f a = f (a :-> SatCons :-> SatFuns)
type Unique    f a = f (a :-> Uniquify)
type Typed     f a = f (a :-> InferTypes)

data a :-> b
data Uniquify
data InferTypes
data SatFuns
data SatCons

type family Meta (f :: * -> *) a where
  Meta Clause (a :-> InferTypes) = (Type, Type)
  Meta f (a :-> InferTypes) = Type
  Meta f (a :-> b) = Meta f a
  Meta f a = a


type family MetaConstr constr a :: GHC.Constraint where
  MetaConstr constr a = ( constr (Meta Expr a)
                        , constr (Meta ValDef a)
                        , constr (Meta Clause a))

newtype Prog a = Prog { unprog :: ([ValDef a], [DataDef a]) }

deriving instance (MetaConstr Show a) => Show (Prog a)

data ValDef a = VDef
  { binding :: ID
  , rhsval  :: Expr a
  , vmeta   :: Meta ValDef a
  }

deriving instance (MetaConstr Show a) => Show (ValDef a)

pattern FunDef id parms expr emeta vmeta =
  VDef id (Lambda parms expr emeta) vmeta

data Expr a
  = Lit    { lval :: Literal
           , emeta :: Meta Expr a
           }
  | Var    { name :: ID
           , emeta :: Meta Expr a
           }
  | Lambda { parms :: [ID]
           , expr  :: Expr a
           , emeta  :: Meta Expr a
           }
  | CaseOf { scrut :: Expr a
           , bind  :: ID
           , paths :: [Clause a]
           , emeta  :: Meta Expr a
           }
  | LetRec { binds :: [ValDef a]
           , expr  :: Expr a
           , emeta  :: Meta Expr a
           }
  | Apply  { efun :: Expr a
           , earg :: Expr a
           , emeta :: Meta Expr a
           }

class HasType a where
  getType :: a -> Type

instance (Type ~ Meta Expr a) => HasType (Expr a) where
  getType = emeta

instance HasType Literal where
  getType (UBInt _) = TPrim PInt
  
deriving instance (MetaConstr Show a) => Show (Expr a)


unfoldAp e = go e [] []
  where go (Apply e1 e2 m) args metas = go e1 (e2:args) (m:metas)
        go ef args metas = (ef, args, metas)


data Literal = UBInt Int deriving (Show)


data Clause a
  = LitMatch { lpat  :: Literal
               -- ^ Can match on primitive literals
             , consq :: Expr a
             , cmeta :: Meta Clause a
             }
  | ConMatch { cpat  :: ID
               -- ^ could refer to a var or construct, both are uniquely
               -- identified
             , args  :: [ID]
               -- ^ newly scoped variables matching constructor parameters
             , consq :: Expr a
             , cmeta :: Meta Clause a
             }
  | Default  { consq :: Expr a
             , cmeta :: Meta Clause a
             }

deriving instance (MetaConstr Show a) => Show (Clause a)


data ID = ID
  { occName :: String -- ^ similar to GHC: How the name occured in the program
  , uniq    :: Int    -- ^ The /real/ unique identifier for this element.
  }
  deriving (Show, Eq)

looksLikeCon :: ID -> Bool
looksLikeCon (occName -> x:_) = isUpper x
looksLikeCon _ = False

looksLikeFun :: ID -> Bool
looksLikeFun = not . looksLikeCon

instance Ord ID where
  compare = comparing uniq


ensureSimpleAST :: Unique Prog a -> Unique (Saturated Prog) a
ensureSimpleAST = ensureSatFuns . ensureSatCons


-- Check to make sure all functions are fully applied

ensureSatFuns :: Prog a -> Prog (a :-> SatFuns)
ensureSatFuns (Prog (vdefs, ddefs)) = Prog (newVDefs, newDDefs)
  where newDDefs   = coerce ddefs
        funArities = mkFunArityMap vdefs M.empty
        newVDefs   = map (satFunBind funArities) vdefs

mkFunArityMap vdefs amap = foldr addLambda amap vdefs
  where addLambda (FunDef id ps _ _ _) amap = M.insert id (length ps) amap
        addLambda (VDef id _ _) amap = M.delete id amap

satFunBind arities (VDef b rhs m) = VDef b (satFunExpr arities rhs) m
satFunExpr arities exp = case exp of
  Lit v m -> Lit v m
  Var n m -> Var n m
  Lambda parms ex meta -> Lambda parms (satFunExpr newMap ex) meta
    where newMap = foldr M.delete arities parms

  CaseOf scrut bnd paths meta -> CaseOf newScrut bnd newPaths meta
    where withBindMap = M.delete bnd arities
          newScrut = satFunExpr arities scrut
          newPaths = map satFunClause paths
          satFunClause c = case c of
            Default e m       -> Default       (satFunExpr withBindMap e) m
            LitMatch l e m    -> LitMatch l    (satFunExpr withBindMap e) m
            ConMatch c vs e m -> ConMatch c vs (satFunExpr newMap e)      m
              where newMap = foldr M.delete withBindMap (c:vs)

  LetRec binds ex meta -> LetRec newBinds newEx meta
    where newBinds = map (satFunBind newMap) binds
          newMap = mkFunArityMap binds arities
          newEx = satFunExpr newMap ex

  Apply _ _ _ -> result
    where (Var name vmeta, eargs, metas) = unfoldAp exp
          newArgs = map (satFunExpr arities) eargs
          zipped = zip newArgs metas
          newApp = foldl (\arg (ap, m) -> Apply ap arg m) newBaseFun zipped
          newBaseFun = Var name vmeta
          result = if not (looksLikeFun name) || saturated
                   then newApp
                   else error $ show name ++
                        " is an unsaturated constructor application"
          -- If we can't find it in the map, assume it's saturated.
          saturated = maybe True (== length eargs) $ M.lookup name arities


--------------------------------------------------------------------------------
--   Check to ensure all constructors are fully applied
--------------------------------------------------------------------------------

ensureSatCons ::  Prog a -> Prog (a :-> SatCons)
ensureSatCons (Prog (vdefs, ddefs)) = Prog (newVDefs, newDDefs)
  where
    conArities = M.fromList $ concatMap (map getArity . constrs) ddefs
    getArity (DCon name args _) = (name, length args)
    newDDefs = coerce ddefs -- don't need to do anything to datadefs
    newVDefs = map (satConBind conArities) vdefs


satConBind :: Map ID Int -> ValDef a ->  ValDef (a :-> SatCons)
satConBind arities (VDef b rhs m) = VDef b (satConExpr arities rhs) m

satConExpr :: Map ID Int -> Expr a -> Expr (a :-> SatCons)
satConExpr arities exp = case exp of
  Lit v m -> Lit v m
  Var n m -> Var n m
  Lambda parms ex meta ->
    Lambda parms (satConExpr arities ex) meta

  CaseOf scrut bnd paths meta -> CaseOf newScrut bnd newPaths meta
    where newScrut = satConExpr arities scrut
          newPaths = map satConPath paths
          satConPath m = case m of
            LitMatch l e m    -> LitMatch l    (satConExpr arities e) m
            ConMatch c vs e m -> ConMatch c vs (satConExpr arities e) m
            Default  e m      -> Default       (satConExpr arities e) m

  LetRec binds ex meta -> LetRec newBinds newEx meta
    where newBinds = map (satConBind arities) binds
          newEx   = satConExpr arities ex

  Apply _ _ _ -> result
    where (Var name vmeta, eargs, metas) = unfoldAp exp
          newArgs = map (satConExpr arities) eargs
          zipped = zip newArgs metas
          newApp = foldl (\arg (ap, m) -> Apply ap arg m) newBaseFun zipped
          newBaseFun = Var name vmeta
          result = if not (looksLikeCon name) || saturated
                   then newApp
                   else error $ show name ++
                        " is an unsaturated constructor application"
          -- If we can't find it in the map, assume it's saturated.
          saturated = maybe True (== length eargs) $ M.lookup name arities


--------------------------------------------------------------------------------
-- Assign unique integers to every variable introduced to a scope, update the
-- identifiers in subsequent nodes with that value.
--------------------------------------------------------------------------------
type UniqState a = State (Map String Int, Int) a

uniquify :: Prog a -> Unique Prog a
uniquify (Prog (vdefs, ddefs)) = Prog (newVDefs, newDDefs)
  where (newDDefs, st) = runState (mapM uniqDDef ddefs) (M.empty, 0)
        newVDefs = evalState (uniqVDefs vdefs) st


-- | Ensure any new bindings resulting from a stateful Uniqification are
-- discarded afterwards.  This is required when new bindings may shadow old.
scoped :: UniqState a -> UniqState a
scoped s = do
  (umap, _) <- get
  v <- s
  (_ , i) <- get
  put (umap, i)
  pure v

-- | given an 'ID', set its unique integer, if unset.  The value either comes
-- from the map, if the name has already been given a unique value, or is given
-- the next available.  This assumes the map holds values that appropriately
-- represent the current scope this ID resides within.
setUniq :: ID -> UniqState ID
setUniq i@(ID name uniq)
  | uniq /= -1 = pure i
  | otherwise = do
      (umap, cur) <- get
      case M.lookup name umap of
        Nothing -> error $ show name ++  " not in scope!"
        Just v  -> put (umap, cur) >> pure (ID name v)

-- | Given an 'ID', add it to the map and assign it a new uniq value.  This is
-- for introducing a new scope, such as in 'LetRec', or 'CaseOf' expressions
newScope :: ID -> UniqState ID
newScope (ID name u) = do
  (umap, cur) <- get
  put (M.insert name cur umap, cur + 1)
  pure (ID name cur)


uniqDDef :: DataDef a -> UniqState (Unique DataDef a)
uniqDDef (DDef ty constrs) = do
  conFs <- mapM uniqConstr constrs
  let def = DDef ty (map ($ def) conFs)
  return def

uniqConstr :: Constructor a
           -- returning a function type  to resolve circular refs
           -> UniqState ((Unique DataDef a) ->  Unique Constructor a)
uniqConstr (DCon id ty _) = DCon <$> newScope id <*> pure ty

uniqVDefs:: [ValDef a] -> UniqState [ValDef (a :-> Uniquify)]
uniqVDefs defs = mapM (newScope . binding) defs >> mapM oneDef defs
 where oneDef (VDef id rhs m) = VDef <$> setUniq id <*> uniqExpr rhs <*> pure m

uniqExpr:: Expr a -> UniqState (Expr (a :-> Uniquify))
uniqExpr e = scoped $ case e of
  Lit l m  -> pure $ Lit l m
  Var id m -> Var <$> setUniq id <*> pure m
  Lambda pms e m    ->
    Lambda <$> mapM newScope pms <*> uniqExpr e <*> pure m
  CaseOf scr bnd pths m ->
    CaseOf <$> uniqExpr scr <*> newScope bnd
             <*> mapM uniqClause pths <*> pure m
  LetRec bnds exp m ->
    LetRec <$> uniqVDefs bnds <*> uniqExpr exp <*> pure m
  Apply f e m ->
    Apply <$> uniqExpr f <*> uniqExpr e <*> pure m

uniqClause :: Clause a -> UniqState (Clause (a :-> Uniquify))
uniqClause c = scoped $ case c of
  LitMatch l e m    -> LitMatch l <$> uniqExpr e                      <*> pure m
  ConMatch c vs e m -> ConMatch c <$> mapM newScope vs <*> uniqExpr e <*> pure m
  Default e m       -> Default    <$> uniqExpr e                      <*> pure m
