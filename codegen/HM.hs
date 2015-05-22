{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import ADTnew
import AST
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.State

type Subst = Map.Map TyVar Monotype 
idSubst = Map.empty

compose :: Subst -> Subst -> Subst
compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1

class Substitutable a where
    apply :: Subst -> a -> a
    ftv   :: a -> Set.Set TyVar

instance Substitutable Polytype where
    ftv (PPoly tvs mt) = Set.difference (ftv mt) $ Set.fromList tvs
    ftv (PMono mt)     = ftv mt

    apply s (PPoly tvs mt) = PPoly tvs $ apply s' mt
                             where s' = foldr Map.delete s tvs
    apply s (PMono mt)     = PMono $ apply s mt

instance Substitutable Monotype where
    ftv (MVar v)     = Set.singleton v
    ftv (MFun m1 m2) = Set.union (ftv m1) (ftv m2)
    ftv (MCon boxed con ms) = foldr Set.union Set.empty $ map ftv ms

    apply s m@(MVar tv)         = Map.findWithDefault m tv s
    apply s (MFun m1 m2)        = MFun (apply s m1) (apply s m2)
    apply s (MCon boxed con ms) = MCon boxed con $ map (apply s) ms

instance Substitutable a => Substitutable [a] where
    ftv = foldr (Set.union . ftv) Set.empty
    apply = map . apply

instance Substitutable TypeEnv where
    ftv = ftv . Map.elems
    apply s = Map.map $ apply s


occursCheck :: Substitutable a => TyVar -> a -> Bool
occursCheck v t = Set.member v $ ftv t

-- bind var to monotype in a substitution if passes occurs check
bind :: TyVar -> Monotype -> Subst
bind v t | MVar v == t   = idSubst
         | occursCheck v t = error "infinite type!"
         | otherwise       = Map.singleton v t

unify :: Monotype -> Monotype -> Subst

unify (MVar v) t = bind v t
unify t (MVar v) = bind v t

-- just a special type constructor
unify (MFun l r) (MFun l' r') = unifys [l,r] [l',r']

unify (MCon b1 c1 ms1) (MCon b2 c2 ms2)
    | b1 /= b2 = error "unify boxedness mismatch!"
    | c1 /= c2 = error "unify constructor mismatch!"
    | otherwise = unifys ms1 ms2

unifys [] [] = idSubst
unifys (x:xs) [] = error "unifys length mismatch!"
unifys [] (x:xs) = error "unifys length mismatch!"
unifys (x:xs) (y:ys) =
    let s1 = unify x y
        s2 = unifys (map (apply s1) xs) (map (apply s1) ys)
    in compose s2 s1

-- type environment is program Var's to ADT Polytypes
type TypeEnv = Map.Map Var Polytype

extendEnv :: (Var, Polytype) -> TypeEnv -> TypeEnv
extendEnv (x, p) tmap = Map.insert x p tmap

lookupEnv :: Var -> TypeEnv -> Polytype
lookupEnv v env = case Map.lookup v env of
                    Nothing -> error $ "lookupEnv free variable " ++ v
                    Just p  -> p

-- for testing, a simple enriched lambda calculus

data LExpr = LLitI   Monotype Int
           | LLitB   Monotype Bool
           | LVar    Monotype Var
           | LApp    Monotype LExpr LExpr
           | LLam    Monotype Var LExpr
           | LLet    Monotype Var LExpr LExpr
           | LLetrec Monotype [(Var, LExpr)] LExpr
           | LFix    Monotype LExpr
           | LIf     Monotype LExpr LExpr LExpr
           | LOp     Monotype Char LExpr LExpr

-- need a fresh variable supply

freshTyVar :: State Int TyVar
freshTyVar = 
    do i <- get
       put $ i+1
       return $ 't':show i

instantiate :: Polytype -> State Int Monotype
instantiate (PPoly as m) =
    do as' <- mapM (const freshTyVar) as
       let s = Map.fromList $ zip as $ map MVar as'
       return $ apply s m

generalize :: TypeEnv -> Monotype -> Polytype
generalize env m =
    let as = Set.toList $ Set.difference (ftv m) (ftv env)
    in PPoly as m

monoBool = MCon True "B" []
monoInt  = MCon True "I" []

-- newtype State s a = State (s -> (a, s))
algW :: TypeEnv -> LExpr -> State Int (LExpr, Subst, Monotype)

algW env (LLitI _ i) = 
    return (LLitI monoInt i,
                  idSubst,
                  monoInt)

algW env (LLitB _ b) = 
    return (LLitB monoBool b,
            idSubst,
            monoBool)

algW env (LVar _ v) =
    let p = lookupEnv v env in
    do m <- instantiate p
       return (LVar m v,
               idSubst,
               m)

algW env (LApp _ e1 e2) =
    do (e1', s1, m1) <- algW env e1
       (e2', s2, m2) <- algW (apply s1 env) e2
       v <- freshTyVar
       let tv = MVar v
       let s3 = unify (apply s2 m1) (MFun m2 tv)
       let m3 = apply s3 tv
       let s4 = s3 `compose` s2 `compose` s1
       return(LApp m3 e1' e2',
              s4,
              m3)

algW env (LLam _ x e) = 
    do v <- freshTyVar
       let tv = MVar v
       let env' = extendEnv (x, PPoly [] tv) env
       (e1, s1, m1) <- algW env' e
       return (LLam m1 x e1, 
               s1, 
               MFun (apply s1 tv) m1)

algW env (LLet _ x e f) =
    do (e1, s1, t1) <- algW env e
       let env' = apply s1 env
           t' = generalize env' t1
       (f1, s2, t2) <- algW (extendEnv (x, t') env') f
       return (LLet t2 x e1 f1,
               compose s1 s2,
               t2)

algW env (LLetrec _ defs e) =
    error "not implemented"

algW env (LFix _ e) =
    do (e1, s1, t1) <- algW env e
       v <- freshTyVar
       let tv = MVar v
           s2 = unify (MFun tv tv) t1
           t2 = apply s1 tv
       return (LFix t2 e1,
               s2,
               t2)

algW env (LIf _ e1 e2 e3) =
    do (f1, s1, t1) <- algW env e1
       (f2, s2, t2) <- algW env e2
       (f3, s3, t3) <- algW env e3
       let s4 = unify t1 monoBool
           s5 = unify t2 t3
           t4 = apply s5 t2
           s6 = foldl1 compose [s5,s4,s3,s2,s1]
       return (LIf t4 f1 f2 f3,
               s6,
               t4)

algW env (LOp _ o e1 e2) =
    error "not implemented"



