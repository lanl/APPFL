{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleInstances       #-}


module Analysis.Type where

import           Analysis.Language
import           Data.Coerce       (coerce)
import           Data.Map          (Map)
import qualified Data.Map          as M
import           Data.Maybe        (fromMaybe)
import           Data.Set          (Set)
import qualified Data.Set          as S

import           Control.Monad.RWS



type Fact        = (ID, Type)
type Facts       = Map ID Type
type Assumption  = (ID, Type)
type Assumptions = Map ID [Type]
type Monotypes   = Set Type
type Constraints = Set Constraint
data Constraint
  = Type :=: Type -- Equivalence
  | Type :<: Type -- Explicit constraint
  | Impl (Set Type) Type Type -- Implicit constraint
  deriving (Show, Eq, Ord)

-- Constraint Generation State
data CGState = CGS { assumps :: Assumptions
                   , monotys :: Monotypes
                   , nextInt :: Int
                   }

-- Constraint Generation Monad
type CGM = RWS Facts Constraints CGState


mkType :: ID -> Type
mkType (ID _ uniq) = TVar $ 't' : show uniq

modifyAssums :: (Assumptions -> Assumptions) -> CGM ()
modifyMonotys :: (Monotypes -> Monotypes) -> CGM ()
modifyAssums f = modify $ \c -> c{assumps = f $ assumps c}
modifyMonotys f = modify $ \c -> c{monotys = f $ monotys c}
addAssum id ty = modifyAssums $ M.insertWith (++) id [ty]
withMonotypes mts act = do
  mtys <- gets monotys
  modifyMonotys (S.union (S.fromList mts))
  v <- act
  modifyMonotys (const mtys)
  pure v

freshTVar :: CGM Type
freshTVar = do
  i <- gets nextInt
  modify $ \c -> c{nextInt = nextInt c + 1}
  return $ TVar $ 'g':'t': show i -- generated type

removeAssums :: [ID] -> CGM ()
removeAssums bs = modifyAssums $ M.filterWithKey (\b _ -> not $ b `elem` bs)

getAllAssums :: [ID] -> CGM Assumptions
getAllAssums ids = gets $ M.filterWithKey (\k _ -> k `elem` ids) . assumps

typecheck :: Unique Prog a -> (Constraints, Unique (Typed Prog) a)
typecheck (Prog (vdefs, ddefs)) =
  if M.null $ assumps endState
  then typed
  else error $ "Assumptions aren't empty?\n" ++ show (assumps endState)
  where facts = M.unions $ map makeFacts ddefs
        newDDefs = map coerceDef ddefs
        coerceDef (DDef ty cons) = DDef ty (map coerce cons)
        initState = CGS M.empty S.empty 0
        (newVDefs, endState, constraints) =
          runRWS (constrainVDefs vdefs) facts initState
        typed = (constraints, Prog (newVDefs, newDDefs))

makeFacts :: Unique DataDef a -> Facts
makeFacts (DDef t@(TApp tcon vs) cons) = M.fromList $ map mkFact cons
  where mkFact dc = (conName dc, theType)
        theType = TForall vars t
        vars = map (\case
                       TVar v -> v
                       _      -> error "DataDef type should be a simple TApp")
               vs
makeFacts _ = error "DataDef type should be a TApp type"


constrainVDefs :: [Unique ValDef a] -> CGM [Unique (Typed ValDef) a]
constrainVDefs defs = do
  newDefs <- mapM constrainVDef defs
  let newBinds = map binding newDefs
      newTypes = map vmeta newDefs
      tyMap = M.fromList $ zip newBinds newTypes

  monotys <- gets monotys

  -- All the assumptions about variables about to go out of scope
  localAssums <- getAllAssums newBinds

  -- Pair each of these assumptions (by ID) with the type set in their bound
  -- expressions for creating implicit constraints
  let constraints =
        constrainAssumsWith (flip (Impl monotys)) localAssums tyMap

  -- Add implicit constraints for each of the created pairs
  tell constraints

  -- Remove all assumptions about bindings about to leave scope
  removeAssums newBinds

  -- Now we're done, typed definitions can be returned
  pure newDefs


constrainVDef :: Unique ValDef a -> CGM (Unique (Typed ValDef) a)
constrainVDef (VDef name rhs _) = do
  -- Don't need to change assumptions or constraints here; just set the type and
  -- return the new VDef
  newRhs <- constrainExpr rhs
  pure $ VDef name newRhs (getType newRhs)


constrainExpr :: Unique Expr a -> CGM (Unique (Typed Expr) a)
constrainExpr e = case e of
  Lit v _ -> pure $ Lit v (getType v)
  Var n _ -> addAssum n ty >> ask >>= maybeConstrain >> pure (Var n ty)
    where ty = mkType n
          maybeConstrain :: Facts -> CGM ()
          maybeConstrain facts = case M.lookup n facts of
            Nothing -> pure ()
            Just f  -> tell . S.singleton $ ty :<: f

  Lambda params body _ ->
    let mtys = (map mkType params)
    in
      withMonotypes mtys $ do
      newBody <- constrainExpr body
      assums <- getAllAssums params

      let resTy = getType newBody -- type of body
          lamTy = foldr TFun resTy mtys -- function type for this expression
          constraints = constrainAssumsWith (:=:) assums
                        (M.fromList $ zip params mtys)

      tell constraints
      removeAssums params
      pure $ Lambda params newBody lamTy

  CaseOf sce scb clas _ ->
    do
      newClas  <- mapM constrainClause clas
      newScrt  <- constrainExpr sce

      -- Assumptions about the scrutinee binding
      scAssums <- gets $ M.findWithDefault [] scb . assumps

      -- get the types of the patterns matched and the resulting expressions
      let (pTys, t0:eTys) = unzip $ map cmeta newClas
          -- The types of patterns and the assumed type of the scrutinee binding
          -- all have to be equivalent to the type of the scrutinee itself
          eqScrutTys      = scAssums ++ pTys
          constrs         = S.fromList (map (:=: getType newScrt) eqScrutTys)
                            `S.union`
                            -- All resulting expressions have to have the same
                            -- type, so add equivalence constraints between the
                            -- first type and every other
                            S.fromList (map (:=: t0) eTys)
      -- Add the constraints
      tell constrs
      -- Remove assumptions about scrutinee binding
      modify $ \c -> c{assumps = M.delete scb $ assumps c}
      -- The Case expression has the same type as that of the result
      -- expressions, so we can arbitrarily use the type of the first (which
      -- must always exist)
      pure $ CaseOf newScrt scb newClas t0

  Apply f e _ -> do
    newF <- constrainExpr f
    newE <- constrainExpr e
    atyp <- freshTVar -- We want to have a type for every expression,
    let constr = getType newF :=: TFun (getType newE) atyp
    tell $ S.singleton constr
    pure $ Apply newF newE atyp

  LetRec binds res _ -> do
    -- Important that we handle the result "before" the bindings so the
    -- assumptions generated therein are usable in the rule for the definitions
    newRes   <- constrainExpr res
    newBinds <- constrainVDefs binds

    -- All the real work is done in 'constrainVDefs', so we just set the type
    -- and move on
    pure $ LetRec newBinds newRes (getType newRes)


constrainAssumsWith
  :: (Type -> Type -> Constraint) -- How to make a constraint
  -> Assumptions -- Assumptions about some variables
  -> Map ID Type -- Map of variable to "set" type
  -> Constraints -- Yields a Constraint Set
constrainAssumsWith comb assums tymap =
  S.fromList . concat . M.elems $
  M.intersectionWith (\t ts -> map (t `comb`) ts)
  tymap assums

constrainClause :: Unique Clause a -> CGM (Unique (Typed Clause) a)
constrainClause c = case c of
  LitMatch lit consq _ -> do
    newConsq <- constrainExpr consq
    let ty = (getType lit, getType newConsq)
    pure $ LitMatch lit newConsq ty

  ConMatch con args consq _ -> do
    fact     <- asks getFact
    newConsq <- constrainExpr consq
    assums   <- getAllAssums args
    let argTys  = map mkType args
        resTy   = finalResTy fact
        funTy   = foldr TFun resTy argTys
        constrs = S.insert (funTy :<: fact) $
          constrainAssumsWith (:=:) assums (M.fromList $ zip args argTys)
    tell constrs
    removeAssums args
    pure $ ConMatch con args newConsq (resTy, getType newConsq)
      where getFact facts = fromMaybe err $ M.lookup con facts
            err = error $ "Why isn't there a fact for " ++ show con

  Default consq _ -> do
    newConsq <- constrainExpr consq
    scTy     <- freshTVar
    pure $ Default newConsq (scTy, getType newConsq)


