module Analysis.FV where

import Analysis.Language
import Data.Set (Set)
import qualified Data.Set as S


fvVDef :: ValDef a -> Set ID
fvVDef (VDef n e m) = S.delete n $ fvExpr e

fvExpr :: Expr a -> Set ID
fvExpr e = case e of
  Lit{} -> S.empty
  Var n _ -> S.singleton n
  Lambda ps ex _ -> fvExpr ex S.\\ S.fromList ps
  CaseOf se sb cs _ -> S.delete sb $ fvExpr se `S.union` clauseFVs 
    where clauseFVs = S.unions $ map fvClause cs
  LetRec bnds ex _ -> S.unions (map fvVDef bnds) S.\\ S.fromList (map binding bnds)
  Apply f e _ -> fvExpr f `S.union` fvExpr e

fvClause :: Clause a -> Set ID
fvClause c = case c of
  LitMatch _ ce _ -> fvExpr ce
  ConMatch c as ce _ -> fvExpr ce S.\\ S.fromList as
  Default ce _ -> fvExpr ce
  
