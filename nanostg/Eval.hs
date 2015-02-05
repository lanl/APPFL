{-# LANGUAGE FlexibleInstances #-}

module Eval
( eval
, replace -- debug
) where

import Data.Map as M hiding (map)
import Data.Maybe
import Data.List
import Parser

type Heap = M.Map Variable Object
type FreshVars = [Variable]
type BoundVars = [Variable]
type Output = String

-- setup heap and add Decls to it
initHeap :: [Declaration] -> Heap
initHeap ds = M.fromList (map declFN ds)

declFN :: Declaration -> (Variable, Object)
declFN (Declaration v o) = (v,o)

lookupHeap :: Variable -> Heap -> Object
lookupHeap v h | lookup == Nothing = error "can't find var" 
               | otherwise = fromJust lookup
                where lookup = M.lookup v h


updateHeap :: Heap -> Variable -> Object -> Heap
updateHeap h v o = M.insert v o h 

initFreshVars :: FreshVars
initFreshVars = ['$':show i | i <- [0..]]

spliter :: [a] -> [(a,a)]
spliter(x:y:zs) = (x,y):spliter zs

split :: [(a,a)] -> ([a],[a])
split xs = (map fst xs, map snd xs) 

eval :: Program -> Output
eval prog@(Program ds) 
    = fst $ evalProg prog (initHeap ds) initFreshVars

evalProg :: Program -> Heap -> FreshVars -> (Output, Heap)
evalProg (Program ds) h fv 
    = evalMain (lookupHeap "main" h) h fv

-- assume for now that main is a THUNK or CON and evaluate it
evalMain :: Object -> Heap -> FreshVars -> (Output, Heap)
evalMain (THUNK e) h fv = evalExpr e h fv
evalMain (CON c as) h fv = evalCON c as h fv
evalMain o h fv = error "bad main"

evalLiteral :: Literal -> Output
evalLiteral (Int x) =  show x

evalAtom:: Atom -> Heap -> FreshVars -> (Output, Heap)
evalAtom (Literal x) h fv = (evalLiteral x, h)
evalAtom (Variable x) h fv = evalObj (lookupHeap x h) h fv

evalExpr ::  Expression -> Heap -> FreshVars -> (Output, Heap)
evalExpr (Atom a) h fv = evalAtom a h fv
evalExpr (FunctionCall f k as) h fv 
    = evalFunctionCall f k as h fv
evalExpr (SatPrimCall op as) h fv 
    = evalSatPrimCall op as h fv
evalExpr (Let vos e) h fv = evalLet vos e h fv
evalExpr (Case e as) h fv = evalCase e as h fv

evalFunctionCall :: Variable -> FunctionArity -> [Atom] -> Heap -> FreshVars -> (Output, Heap)
evalFunctionCall = error "functioncall not done"

evalSatPrimCall :: Primitive -> [Atom] -> Heap -> FreshVars -> (Output, Heap)
evalSatPrimCall p (a1:a2:as) h fv 
    | p == Add = (show (read x1 + read x2), h)
               where (x1,h1) = evalAtom a1 h fv
                     (x2,h2) = evalAtom a2 h fv

evalLet :: [(Variable,Object)] -> Expression -> Heap -> FreshVars -> (Output, Heap)
-- todo: make freshvar, replace var in expr, make heap object
evalLet vos e h fv 
    = (debug, h') 
    where (v,o) = head vos -- only doing first let for now
          h' = updateHeap h v o
          (s,h2) = evalObj o h' fv 
          debug = v ++ "=" ++ s 

evalCase :: Expression -> [Alternative] -> Heap -> FreshVars -> (Output, Heap)
evalCase e (a:as) h fv 
    = (s,he)
    where (v,he)  = evalExpr e h fv
          -- v is a literal/pointer or constructor
          -- check if it is a pointer
          lookup = M.lookup v h 
          -- todo: deal w/ heap from evalObj
          value = if lookup == Nothing then v else fst $ evalObj (fromJust lookup) h fv
          str = read value
          (s,ha) = evalAlternative a str he fv -- only doing first alt so far
                   

evalAlternative :: Alternative -> String -> Heap -> FreshVars -> (Output, Heap)
evalAlternative (DefaultAlt v e) s h fv = evalDefaultAlt v e s h fv
evalAlternative (Alt c vs e) con h fv = error "no alt"

evalDefaultAlt :: Variable -> Expression -> String -> Heap -> FreshVars -> (Output, Heap)
evalDefaultAlt v e s h fv = error "no default alt" 
                           
evalObj :: Object -> Heap -> FreshVars -> (Output, Heap)
evalObj (FUN vs e) h fv = error "FUN Obj not done"
evalObj (PAP v as) h fv = error "PAP Obj not done"
evalObj (CON c as) h fv = evalCON c as h fv
evalObj (THUNK e) h fv =  error "THUNK Obj not done" 

-- simple CON evaluation as we don't have data types yet
evalCON :: Constructor -> [Atom] -> Heap -> FreshVars -> (Output, Heap)
evalCON c as h fv 
    = (con, h)
    where con = "(" ++ c ++ " " ++ intercalate " " [fst $ evalAtom a h fv | a <- as] ++ ")"

-- replace a non-bound variable in a expression

type Replacement = (Variable, Atom) --todo make list?

class Replace a where replace ::  Replacement -> BoundVars -> a -> a

instance Replace Expression where
  replace r bvs (Atom a) 
    = Atom (replace r bvs a)

  replace r bvs (FunctionCall f k as) 
    = FunctionCall f k as'
    where bvs' = f:bvs --f is boundvar?
          as' = replace r bvs' as 

  replace r bvs (SatPrimCall op as) 
    = SatPrimCall op as
    where as' = replace r bvs as

  -- need to update boundvars
  replace r bvs (Let vos e) 
    = Let vos' e' 
    where (v,o) = head vos -- todo: only doing first let for now
          bvs' = v:bvs
          o' = replace r bvs' o
          e' = replace r bvs' e
          vos' = [(v,o')] -- hack for now as only doing first let

  replace r bvs (Case e alts) 
    = Case e' alts'
    where e' = replace r bvs e
          alts' = replace r bvs alts

instance Replace Atom where
  replace (var, atom) bvs (Variable x) | x == var && (notElem var bvs) = atom
  replace (var, atom) bvs x = x -- return original

instance Replace [Atom] where
  replace r bvs as 
    = [replace r bvs a | a <-as ]

-- need to update boundvars
instance Replace Alternative where
  replace r bvs (DefaultAlt v e) 
    = DefaultAlt v e'
    where e' = replace r bvs e

  replace r bvs (Alt c vs e) 
    = error "no Alt yet"

-- todo: only doing first alt of list for now
instance Replace [Alternative] where
  replace r bvs alts 
    = [replace r bvs (head alts)]

instance Replace Object where
  -- need update boundvars
  replace r bvs (FUN vs e) 
    = error "no replace FUN yet"

  replace r bvs (PAP f as) 
    = PAP f (replace r bvs as)

  replace r bvs (CON c as) 
    = CON c (replace r bvs as)

  replace r bvs (THUNK e) 
    = THUNK (replace r bvs e)

