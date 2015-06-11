{-# LANGUAGE FlexibleInstances    #-}

module BU (
  Subst,
  compose,
  Substitutable(..),
  freshMonoVar,
  freshMonoVars,
  Constraint(..),
  Constraints,
  show,
  Assumption,
  Assumptions,
  dropAss,
  solve
) where

import AST
import ADT
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
    freevars   :: a -> Set.Set TyVar

instance Substitutable Polytype where
    freevars (PPoly tvs mt) = (freevars mt) `Set.difference` (Set.fromList tvs)
    freevars (PMono mt)     = freevars mt

    apply s (PPoly tvs mt) = PPoly tvs $ apply s' mt
                             where s' = foldr Map.delete s tvs
    apply s (PMono mt)     = PMono $ apply s mt

instance Substitutable Monotype where
    freevars (MVar v)     = Set.singleton v
    freevars (MFun m1 m2) = Set.union (freevars m1) (freevars m2)
    freevars (MCon boxed con ms) = foldr Set.union Set.empty $ map freevars ms
    freevars _ = Set.empty

    apply s m@(MVar tv)         = Map.findWithDefault m tv s
    apply s (MFun m1 m2)        = MFun (apply s m1) (apply s m2)
    apply s (MCon boxed con ms) = MCon boxed con $ map (apply s) ms
    apply s _                   = error "substituting for a primitive type?"

instance Substitutable [Monotype] where
    freevars = foldr (Set.union . freevars) Set.empty
    apply = map . apply

--instance (Substitutable a, Ord a) => Substitutable (Set.Set a) where
--    freevars = freevars . Set.toList
--    apply = Set.map . apply

occursCheck :: Substitutable a => TyVar -> a -> Bool
occursCheck v t = Set.member v $ freevars t

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

-- if they're equal there's nothing to do
unify m1 m2 | m1 == m2 = idSubst
-- unify MPrimInt MPrimInt = idSubst
-- unify MPrimDouble MPrimDoubleInt = idSubst
-- unify MPrimBool MPrimBool = idSubst

unifys [] [] = idSubst
unifys (x:xs) [] = error "unifys length mismatch!"
unifys [] (x:xs) = error "unifys length mismatch!"
unifys (x:xs) (y:ys) =
    let s1 = unify x y
        s2 = unifys (map (apply s1) xs) (map (apply s1) ys)
    in compose s2 s1

indent i xs = (take i $ repeat ' ') ++ indent' i xs
    where
      indent' i ('\n':x:xs) = '\n' : (take i $ repeat ' ') ++ indent' i (x:xs)
      indent' i "\n"        = "\n"
      indent' i (x:xs)      = x : indent' i xs
      indent' i ""          = ""

-- need a fresh variable supply

freshTyVar :: State Int TyVar
freshTyVar = 
    do i <- get
       put $ i+1
       return $ 't':show i

freshMonoVar :: State Int Monotype
freshMonoVar =
    do v <- freshTyVar
       return $ MVar v

freshMonoVars 0 = return []
freshMonoVars n = 
    do
      v <- freshMonoVar
      vs <- freshMonoVars (n-1)
      return $ v:vs

instantiate :: Polytype -> State Int Monotype
instantiate (PPoly as m) =
    do ms <- mapM (const freshMonoVar) as
       let s = Map.fromList $ zip as ms
       return $ apply s m

generalize ms t = 
    let as = Set.toList $ Set.difference (freevars t) (freevars ms)
    in PPoly as t

-- Assumptions & Constraints

data Constraint = EqC  Monotype Monotype
                | ExpC Monotype Polytype
                | ImpC Monotype (Set.Set Monotype) Monotype
                  deriving (Eq, Ord)

instance Show Constraint where
    show (EqC m1 m2) = "EqC " ++ show m1 ++ " " ++ show m2
    show (ExpC m p) = "ExpC " ++ show m ++ " " ++ show p
    show (ImpC m1 ms m2) = "ImpC " ++ show m1 ++ " " ++ 
                           show (Set.toList ms) ++ " " ++ show m2

type Constraints = Set.Set Constraint

instance Substitutable Constraint where
    freevars _ = error "don't need freevars for Constraint"

    apply s (EqC t1 t2) = EqC  (apply s t1) (apply s t2)
    apply s (ExpC t p)  = ExpC (apply s t)  (apply s p)
    apply s (ImpC t1 ms t2) = ImpC (apply s t1)
                                   (apply s ms)
                                   (apply s t2)

instance Substitutable [Constraint] where
    freevars _ = error "don't need freevars for [Constraint]"

    apply = map . apply

instance Substitutable (Set.Set Monotype) where
    freevars = freevars . Set.toList
    apply = Set.map . apply

class ActiveVars a where
    activeVars :: a -> Set.Set TyVar

instance ActiveVars Constraint where
    activeVars (EqC t1 t2) = freevars t1 `Set.union` freevars t2
    activeVars (ExpC t p) = freevars t `Set.union` freevars p
    activeVars (ImpC t1 ms t2) = freevars t1 `Set.union`
                                 (freevars ms `Set.intersection` freevars t2)

instance ActiveVars [Constraint] where
    activeVars cs = foldr Set.union Set.empty $ map activeVars cs

type Assumption = (Var, Monotype) -- really just TyVar?

-- Set, not Map, because a var may have multiple assumptions
-- alternatively, Map.Map Var (Set.Set Monotype)
type Assumptions = Set.Set Assumption

dropAss x as = Set.fromList [ (x', a) | (x', a) <- Set.toList as, x /= x' ]


-- Solvers

solve :: Constraints -> State Int Subst
solve cs = solve1 (Set.toList cs) [] False

solve1 :: [Constraint] -> [Constraint] -> Bool -> State Int Subst
solve1 [] [] _ = return idSubst
solve1 [] ys False = error $ "can't make progress with " ++ show ys
solve1 [] ys True  = solve1 ys [] False
solve1 (EqC t1 t2 : cs) ys _ = 
    let s = unify t1 t2
    in do
      res <- solve1 (apply s cs) (apply s ys) True
      return $ compose res s

solve1 (ExpC t p : cs) ys _ = 
    do t' <- instantiate p
       solve1 (EqC t t' : cs) ys True

solve1 (c@(ImpC t1 ms t2) : cs) ys progress = 
    if Set.null $ (freevars t2 `Set.difference` freevars ms) 
                  `Set.intersection` activeVars cs
    then solve1 (ExpC t1 (generalize ms t2) : cs) ys True
    else solve1 cs (c:ys) progress
      
instance Substitutable (Maybe Monotype) where
    freevars _ = error "why are we doing this?"
    apply s Nothing = Nothing
    apply s (Just t) = Just (apply s t)

