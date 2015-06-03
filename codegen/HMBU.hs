{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ParallelListComp     #-}

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

    apply s m@(MVar tv)         = Map.findWithDefault m tv s
    apply s (MFun m1 m2)        = MFun (apply s m1) (apply s m2)
    apply s (MCon boxed con ms) = MCon boxed con $ map (apply s) ms

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

unifys [] [] = idSubst
unifys (x:xs) [] = error "unifys length mismatch!"
unifys [] (x:xs) = error "unifys length mismatch!"
unifys (x:xs) (y:ys) =
    let s1 = unify x y
        s2 = unifys (map (apply s1) xs) (map (apply s1) ys)
    in compose s2 s1

-- for testing, a simple enriched lambda calculus

data LExpr = LLitI   {typ :: Maybe Monotype,                  mtvs :: Set.Set Monotype,
                      i :: Int}
           | LLitB   {typ :: Maybe Monotype,                  mtvs :: Set.Set Monotype,
                      b :: Bool}
           | LVar    {typ :: Maybe Monotype, ftv :: Monotype, mtvs :: Set.Set Monotype,
                      v :: Var}
           | LApp    {typ :: Maybe Monotype, ftv :: Monotype, mtvs :: Set.Set Monotype,
                      e1 :: LExpr, e2 :: LExpr}
           | LLam    {typ :: Maybe Monotype, ftv :: Monotype, mtvs :: Set.Set Monotype,
                      v :: Var, e1 :: LExpr}
           | LLet    {typ :: Maybe Monotype,                  mtvs :: Set.Set Monotype,
                      v :: Var, e1 :: LExpr, e2 :: LExpr}
           | LLetrec {typ :: Maybe Monotype,                  mtvs :: Set.Set Monotype,
                      defs :: [(Var, LExpr)], e1 :: LExpr}
           | LFix    {typ :: Maybe Monotype, ftv :: Monotype, mtvs :: Set.Set Monotype,
                      e1 :: LExpr}
           | LIf     {typ :: Maybe Monotype, ftv :: Monotype, mtvs :: Set.Set Monotype,
                      e1 :: LExpr, e2 :: LExpr, e3 :: LExpr}
           | LOp     {typ :: Maybe Monotype, ftv :: Monotype, mtvs :: Set.Set Monotype,
                      op :: String, e1 :: LExpr, e2 :: LExpr}
             deriving(Eq)

instance Show LExpr where
    show LLitI{typ,mtvs,i} = show i ++ "::" ++ showj mtvs typ
    show LLitB{typ,mtvs,b} = show b ++ "::" ++ showj mtvs typ
    show LVar{typ,mtvs,ftv,v} = v ++ "::" ++ showj mtvs typ
    show LApp{typ,mtvs,ftv,e1,e2} = show e1 ++ " (" ++ show e2 ++ ")" ++ "::" ++ showj mtvs typ
    show LLam{typ,mtvs,ftv,v,e1} = "\\" ++ v ++ " ->\n" ++ 
                               indent 2 (show e1) ++ "::" ++ showj mtvs typ
    show LLet{typ,mtvs,v,e1,e2} = "let " ++ v ++ " = \n" ++
                                  indent 2 (show e1) ++ "\n" ++
                                  "in\n" ++
                                  indent 2 (show e2) ++ "::" ++ showj mtvs typ
    show LLetrec{typ,mtvs,defs,e1} =
        "letrec" ++ "::" ++ showj mtvs typ ++ "\n" ++
          indent 2 (concatMap (showDef mtvs) defs) ++
        "in\n" ++
          indent 2 (show e1)

showDef mtvs (x,e) = x ++ "::" ++ showj mtvs (typ e) ++ " =\n" ++ indent 2 (show e) ++ "\n"
         
showj mtvs (Just t) = show $ generalize mtvs t
                         

indent i xs = (take i $ repeat ' ') ++ indent' i xs
    where
      indent' i ('\n':x:xs) = '\n' : (take i $ repeat ' ') ++ indent' i (x:xs)
      indent' i "\n"        = "\n"
      indent' i (x:xs)      = x : indent' i xs
      indent' i ""          = ""

litI i       = LLitI Nothing Set.empty i
litB b       = LLitB Nothing Set.empty b
var v        = LVar  Nothing monoJunk Set.empty v
app e1 e2    = LApp  Nothing monoJunk Set.empty e1 e2
lam v e1     = LLam  Nothing monoJunk Set.empty v e1
llet v e1 e2 = LLet  Nothing Set.empty v e1 e2
letrec ds e = LLetrec Nothing Set.empty ds e

-- tests
-- 1
ex1 = litI 1

-- let x = x in x
ex2 = llet "x" (var "x") (var "x")

-- \x -> x
ex3 = lam "x" (var "x")

-- \x -> let y = x
--       in y
ex4 = lam "x" $ llet "y" (var "x") (var "y")

-- let id = \x -> let y = x
--                in y
-- in id id
ex5 = llet "id" ex4 $ app (var "id") (var "id")

ex6 = letrec [("f", lam "i" $ app (var "g") (var "i")),
              ("g", lam "i" $ litI 5)]
             (app (var "f") (litI 4))

ex9 = letrec [("g", lam "i" $ app (var "h") (var "i")),
              ("f", lam "i" $ app (var "g") (var "i")),
              ("h", lam "i" $ litI 5)]
             (app (var "f") (litI 4))

ex7 = llet "f" (lam "i" (var "i")) (app (var "f") (litI 4))
      
ex8 = letrec [("id", (lam "i" (var "i")))] (app (var "id") (litI 4))
      

doit e0 =
    let (e1,i) = runState (dv e0 Set.empty) 0
        (as,cs,e2) = bu e1
        (s, _) = runState (solve cs) i
        Just t = typ e2
    in (generalize (Set.empty :: Set.Set Monotype) $ apply s t,
        t,
        Set.toList as,
--        e2,
        (backSub s e2),
        apply s $ Set.toList cs,
--        cs,
        Map.toList s)

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

monoBool = MCon True "B" []
monoInt  = MCon True "I" []
monoJunk = MCon True "not initialized" []

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

-- newtype State s a = State (s -> (a, s))

-- create fresh type variables, monomorphic type variables, for BU in advance

dv :: LExpr -> (Set.Set Monotype) -> State Int LExpr

dv e@LLitI{} mtvs = return e{mtvs = mtvs}

dv e@LLitB{} mtvs = return e{mtvs = mtvs}

dv e@LVar{} mtvs =
    do b <- freshMonoVar 
       return e{ftv = b, mtvs = mtvs}

dv e@LApp{e1,e2} mtvs =
    do b <- freshMonoVar
       e1' <- dv e1 mtvs
       e2' <- dv e2 mtvs
       return e{ftv = b, 
                mtvs = mtvs,
                e1 = e1', 
                e2 = e2'}

dv e@LLam{e1} mtvs =
    do b <- freshMonoVar
       e1' <- dv e1 (Set.insert b mtvs) -- new mono type var
       return e{ftv = b, 
                mtvs = mtvs,  -- per the paper, doesn't matter mtvs or mtvs + {b}
                e1 = e1'}

dv e@LLet{e1,e2} mtvs =
    do e1' <- dv e1 mtvs
       e2' <- dv e2 mtvs
       return e{mtvs = mtvs,
                e1 = e1', 
                e2 = e2'}

dv e@LLetrec{defs, e1} mtvs =
    do e1' <- dv e1 mtvs
       defs' <- dvdefs defs mtvs -- use monad comprehension here
       return e{mtvs = mtvs,
                defs = defs',
                e1 = e1'}

dvdefs [] _ = return []
dvdefs ((v,e):defs) mtvs = 
    do e' <- dv e mtvs
       es' <- dvdefs defs mtvs
       return $ (v, e') : es'

bu :: LExpr -> (Assumptions, Constraints, LExpr)

bu e@LLitI{} =
    (Set.empty,
     Set.empty,
     e{typ = Just monoInt})

bu e@LLitB{} =
    (Set.empty,
     Set.empty,
     e{typ = Just monoBool})

bu e@LVar{ftv,v} =
    (Set.singleton (v,ftv),
     Set.empty,
     e{typ = Just ftv})

bu e@LApp{ftv, mtvs, e1, e2} =
    let (as1, cs1, e1') = bu e1
        (as2, cs2, e2') = bu e2
        Just t1 = typ e1'
        Just t2 = typ e2'
    in (Set.union as1 as2,
        cs1 `Set.union`
            cs2 `Set.union`
            Set.singleton (EqC t1 (MFun t2 ftv)),
        e{typ = Just ftv, e1 = e1', e2 = e2'})

bu e@LLam{ftv, mtvs, v, e1} = 
    let (as, cs, e1') = bu e1
        Just t = typ e1'
        cs1 = Set.fromList [EqC t' ftv | (v', t') <- Set.toList as, v == v']
    in (dropAss v as,
        Set.union cs cs1,
        e{typ = Just $ MFun ftv t, e1 = e1'})

bu e@LLet{mtvs,v,e1,e2} = 
    let (as1, cs1, e1') = bu e1
        (as2, cs2, e2') = bu e2
        Just t1 = typ e1'
        cs3 = Set.fromList [ImpC t' mtvs t1 | (v',t') <- Set.toList as2, v==v']
    in (Set.union as1 (dropAss v as2), -- careful, as1 could refer to outer v
        cs1 `Set.union` cs2 `Set.union` cs3,
        e{typ = typ e2', e1 = e1', e2 = e2'})

bu e@LLetrec{mtvs, defs, e1} =
    let (xs, es) = unzip defs
        (as, cs, es') = buRec xs es mtvs
        (as1, cs1, e1') = buIn [ (x,t) | Just t <- map typ es' | x <- xs ]  mtvs e1
    in (foldr dropAss (Set.union as as1) xs,
        Set.union cs cs1,
        e{typ = typ e1',
          defs = zip xs es',
          e1 = e1'})

buRec xs es mtvs =
    let (ass, css, es') = unzip3 $ map bu es
        ias = zip [0..] ass
        ixts = zip3 [0..] xs [t | Just t <- map typ es']
        ncs = Set.fromList 
              [ if (i==j) then EqC t' t else ImpC t' mtvs t
                | (i,x,t) <- ixts, (j,as) <- ias, (x',t') <- Set.toList as, x == x' ]
        cs = foldr Set.union ncs css
        as = foldr Set.union Set.empty ass
    in (as, cs, es')

buIn xts mtvs e1 = 
    let (as1, cs1, e1') = bu e1
        ncs = [ ImpC t' mtvs t | (x,t) <- xts, (x',t') <- Set.toList as1, x == x' ]
        cs' = cs1 `Set.union` Set.fromList ncs
    in (as1, cs', e1')

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
      
-- Back substitution

instance Substitutable (Maybe Monotype) where
    freevars _ = error "why are we doing this?"
    apply s Nothing = Nothing
    apply s (Just t) = Just (apply s t)

backSub s e@LLitI{mtvs} = e

backSub s e@LLitB{mtvs} = e

backSub s e@LVar{typ,mtvs} = e{typ = apply s typ,
                               mtvs = apply s mtvs}

backSub s e@LApp{typ,mtvs,e1,e2} = e{typ = apply s typ,
                                     mtvs = apply s mtvs,
                                     e1 = backSub s e1,
                                     e2 = backSub s e2}

backSub s e@LLam{typ,mtvs,v,e1} = e{typ = apply s typ,
                                    mtvs = apply s mtvs,
                                    e1 = backSub s e1}
                            
backSub s e@LLet{typ,mtvs,e1,e2} = e{typ = apply s typ,
                                     mtvs = apply s mtvs,
                                     e1 = backSub s e1,
                                     e2 = backSub s e2}

backSub s e@LLetrec{typ,mtvs,defs,e1} =
    e{typ = apply s typ,
      mtvs = apply s mtvs,
      defs = [(x,backSub s e) | (x,e) <- defs],
      e1 = backSub s e1}


