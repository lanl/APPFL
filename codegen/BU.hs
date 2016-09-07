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

--import AST(BuiltinType(..)) -- for hack in unify

import ADT
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.State
-- import AST(Var) -- shouldn't depend on AST
type Var = String

type Subst = Map.Map TyVar Monotype
idSubst = Map.empty

compose :: Subst -> Subst -> Subst
compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1

class Substitutable a where
    apply    :: Subst -> a -> a
    freevars :: a -> Set.Set TyVar

instance Substitutable Polytype where
    freevars (PPoly tvs mt) = (freevars mt) `Set.difference` (Set.fromList tvs)
    freevars (PMono mt)     = freevars mt

    apply s  (PPoly tvs mt) = PPoly tvs $ apply s' mt
                              where s' = foldr Map.delete s tvs
    apply s  (PMono mt)     = PMono $ apply s mt

instance Substitutable Monotype where
    freevars (MVar v)        = Set.singleton v
    freevars (MFun m1 m2)    = Set.union (freevars m1) (freevars m2)
    freevars (MCon _ con ms) = foldr Set.union Set.empty $ map freevars ms
    freevars (MPVar v)       = Set.singleton v
    freevars _               = Set.empty

    apply s m@(MPVar tv)     = Map.findWithDefault m tv s
    apply s (MFun m1 m2)     = MFun (apply s m1) (apply s m2)
    apply s (MCon b con ms)  = MCon b con $ map (apply s) ms
    apply s m@(MVar tv)      = Map.findWithDefault m tv s
    apply s m                = m

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
bind v t | MPVar v == t    = idSubst
         | MVar v == t     = idSubst
         | occursCheck v t = error "infinite type!"
         | otherwise       = Map.singleton v t

unify :: Monotype -> Monotype -> Subst

-- unify unboxed and poly -> error
--unify (MPVar p) (MPrim u) = let e = error $ "cannot unify unboxed type " ++ show u ++ " with polymorphic type variable" in seq e e
--unify (MPrim u) (MPVar p) = let e = error $ "cannot unify unboxed type " ++ show u ++ " with polymorphic type variable" in seq e e
unify (MPVar p) c@(MCon (Just False) _ _) = let e = error $ "cannot unify unboxed type " ++ show c ++ " with polymorphic type variable" in seq e e
unify c@(MCon (Just False) _ _) (MPVar p) = let e = error $ "cannot unify unboxed type " ++ show c ++ " with polymorphic type variable"in seq e e

-- replace least specified--uncommitted--before committed poly
unify t (MVar v) = bind v t
unify (MVar v) t = bind v t

unify (MPVar v) t = bind v t
unify t (MPVar v) = bind v t

-- just a special type constructor
unify (MFun l r) (MFun l' r') = unifys [l,r] [l',r']

unify m1@(MCon b1 c1 ms1) m2@(MCon b2 c2 ms2)
    | b1 /= b2 = error $ "unify boxedness mismatch! "  ++ show m1 ++ " " ++ show m2
    | c1 /= c2 = error $ "unify constructor mismatch! " ++ show c1 ++ " " ++ show c2
    | otherwise = unifys ms1 ms2

-- TODO: there must be a cleaner way.
unify (MPrim PInt) (MCon (Just False) "Int#" []) = idSubst
unify (MCon (Just False) "Int#" []) (MPrim PInt) = idSubst
unify (MPrim PDouble) (MCon (Just False) "Double#" []) = idSubst
unify (MCon (Just False) "Double#" []) (MPrim PDouble) = idSubst


-- if they're equal there's nothing to do
unify m1 m2 | m1 == m2 = idSubst
            | otherwise = error $ "unify:  can't unify " ++ show m1 ++ " with " ++ show m2

unifys [] [] = idSubst
unifys (x:xs) [] = error "unifys length mismatch!"
unifys [] (x:xs) = error "unifys length mismatch!"
unifys (x:xs) (y:ys) =
    let s1 = unify x y
        s2 = unifys (map (apply s1) xs) (map (apply s1) ys)
    in compose s2 s1

-- need a fresh variable supply

freshTyVar :: State Int TyVar
freshTyVar =
    do i <- get
       put $ i+1
       return $ 't':show i


freshPolyVar :: State Int Monotype
freshPolyVar =
    do v <- freshTyVar
       return $ MPVar v

freshPolyVars 0 = return []
freshPolyVars n =
    do
      v <- freshPolyVar
      vs <- freshPolyVars (n-1)
      return $ v:vs

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
    do ms <- mapM (const freshPolyVar) as
       let s = Map.fromList $ zip as ms
       return $ apply s m
instantiate p = error $ "HMStg.instantiate p=" ++ show p

monoToPoly as m@(MVar v) | Set.member v as = MPVar v
                         | otherwise = m
monoToPoly as (MFun a b) = MFun (monoToPoly as a) (monoToPoly as b)
monoToPoly as (MCon b c ms) = MCon b c $ map (monoToPoly as) ms
--monoToPoly as m@MPrim{} = m
monoToPoly as m@MPVar{} = m
monoToPoly as m = error $
                  "HMStg.monoToPoly: m=" ++ show m

generalize :: Substitutable a => a -> Monotype -> Polytype
generalize ms t =
    let as = Set.difference (freevars t) (freevars ms)
    -- necessary?  as in t become MPVar--not necessary, just skating on thin ice
    --    t' = monoToPoly as t
        t' = t
    in PPoly (Set.toList as) t'

-- Assumptions & Constraints

data Constraint = EqC  Monotype Monotype
                | ExpC Monotype Polytype
                | ImpC Monotype (Set.Set Monotype) Monotype
                  deriving (Eq, Ord)

instance Show Constraint where
    show (EqC m1 m2) = "EqC " ++ show m1 ++ " , " ++ show m2
    show (ExpC m p) = "ExpC " ++ show m ++ " , " ++ show p
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
    activeVars (EqC t1 t2)     = freevars t1 `Set.union` freevars t2
    activeVars (ExpC t p)      = freevars t `Set.union` freevars p
    activeVars (ImpC t1 ms t2) = freevars t1 `Set.union`
                                 (freevars ms `Set.intersection` freevars t2)

instance ActiveVars [Constraint] where
    activeVars cs = foldr Set.union Set.empty $ map activeVars cs

type Assumption = (Var, Monotype)

-- Set, not Map, because a var may have multiple assumptions
-- alternatively, Map.Map Var (Set.Set Monotype)
type Assumptions = Set.Set Assumption

dropAss x as = Set.fromList [ (x', a) | (x', a) <- Set.toList as, x /= x' ]


-- Solvers

solve :: Constraints -> State Int Subst
solve cs = solve1 (Set.toList cs) [] False
-- solve cs = error $ show $ Set.toList cs

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
       `Set.intersection` activeVars (cs ++ ys)
    then solve1 (ExpC t1 (generalize ms t2) : cs) ys True
    else solve1 cs (c:ys) progress

instance Substitutable (Maybe Monotype) where
    freevars _ = error "why are we doing this?"

    apply s Nothing = Nothing
    apply s (Just t) = Just (apply s t)
