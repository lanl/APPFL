{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeOperators       #-}


module Analysis.Type where

import           Analysis.Language
import           Data.Coerce       (coerce)
import           Data.Map          (Map)
import qualified Data.Map          as M
import           Data.Set          (Set)
import qualified Data.Set          as S

import           Control.Monad.RWS


-- TODO: Need to rethink the Assumptions.


type Fact        = (ID, Type)
type Facts       = Map ID Type
type Assumption  = (ID, Type)
type Assumptions = Set Assumption
type Monotypes   = Set Type

data Constraint
  = Type :==: Type -- Equivalence
  | Type :<=: Type -- Explicit constraint
  | Impl (Set Type) Type Type -- Implicit constraint
  deriving (Show, Eq, Ord)


type Constraints = Set Constraint
type ConstrainM = RWS Facts Constraints (Assumptions, Monotypes)


modifyAssums :: (Assumptions -> Assumptions) -> ConstrainM ()
modifyMonotys :: (Monotypes -> Monotypes) -> ConstrainM ()
modifyAssums f = modify $ \(a,ms) -> (f a, ms)
modifyMonotys f = modify $ \(a,ms) -> (a, f ms)

removeAllBinds :: [ID] -> ConstrainM ()
removeAllBinds bs = modifyAssums $ S.filter (\(b,_) -> not $ b `elem` bs)

typecheck :: Unique Prog a -> ()
typecheck (Prog (vdefs, ddefs)) = typed
  where facts = M.unions $ map makeFacts ddefs
        newDDefs = map coerceDef ddefs
        coerceDef (DDef ty cons) = DDef ty (map coerce cons)
        (newVDefs, assums, constraints) =
          runRWS (constrainVDefs vdefs) facts (S.empty, S.empty)
        typed = undefined

makeFacts :: Unique DataDef a -> Facts
makeFacts (DDef t@(TApp tcon vs) cons) = M.fromList $ map mkFact cons
  where mkFact dc = (conName dc, theType)
        theType = TForall vars t
        vars = map (\case
                       TVar v -> v
                       _      -> error "DataDef type should be a simple TApp")
               vs
makeFacts _ = error "DataDef type should be a TApp type"


constrainVDefs :: [Unique ValDef a] -> ConstrainM [Unique (Typed ValDef) a]
constrainVDefs defs = do
  newDefs <- mapM constrainVDef defs
  let newBinds = map binding newDefs
      newTypes = map vmeta newDefs
      tyMap = M.fromList $ zip newBinds newTypes

  localAssums <- S.filter (\(b,_) -> b `elem` newBinds) . fst <$> get
    
  -- need to pair assumed types and the 'set' types to make constraints and
  -- append them to the W of the RWS


  removeAllBinds newBinds
  undefined

constrainVDef :: Unique ValDef a -> ConstrainM (Unique (Typed ValDef) a)
constrainVDef (VDef name rhs _) = do
  newRhs <- constrainExpr rhs
  
  undefined

constrainExpr :: Unique Expr a -> ConstrainM (Unique (Typed Expr) a)
constrainExpr = undefined
