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



type Fact = (ID, Type)
type Facts = Map ID Type
type Assumption = (ID, Type)
type Assumptions = Set Assumption

data Constraint
  = Type :==: Type -- Equivalence
  | Type :<=: Type -- Explicit constraint
  | Impl (Set Type) Type Type -- Implicit constraint
  deriving (Show, Eq, Ord)


type Constraints = Set Constraint

type ConstrainM = RWS Facts Constraints Assumptions


typecheck :: Unique Prog a -> ()
typecheck (Prog (vdefs, ddefs)) = typed
  where facts = M.unions $ map makeFacts ddefs
        newDDefs = map coerceDef ddefs
        coerceDef (DDef ty cons) = DDef ty (map coerce cons)
        (newVDefs, assums, constraints) =
          runRWS (mapM constrainVDef vdefs) facts S.empty
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


constrainVDef :: Unique ValDef a -> ConstrainM (Unique (Typed ValDef) a)
constrainVDef (VDef binding rhsval _) = undefined
