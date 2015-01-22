-- This is more me trying to figure out what to do than anything useful...

module Eval
( eval
) where

import Data.Map as M hiding (map)
import Data.Maybe
import Data.List
import Parser

type Heap = M.Map Variable Object
type FreshVars = [Variable]

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

eval :: Program -> String
eval prog@(Program ds) = fst $ evalProg prog (initHeap ds) initFreshVars

evalProg :: Program -> Heap -> FreshVars -> (String, Heap)
evalProg (Program ds) h fv = evalObj (lookupHeap "main" h) h fv

evalObj :: Object -> Heap -> FreshVars -> (String, Heap)
evalObj = error "not done"

{-
evalProg :: Program -> State -> String
evalProg (Program ds) s@(h,st) = evalObj (lookupHeap "main" h) s

evalDecl :: Declaration -> State -> String
evalDecl (Declaration v o) (h,st) = evalObj o (newh,st) 
                                  where newh = M.insert v o h

evalObj :: Object -> State -> String
evalObj (THUNK e) s = evalExpr e s
evalObj (CON c as) s =  "(" ++ c ++ " " ++ intercalate " " [evalAtom a s | a <- as] ++ ")"

evalExpr :: Expression -> State -> String
evalExpr (Atom a) s = evalAtom a s
evalExpr (SatPrimCall p as) s = evalPrim p as s
evalExpr (Let v o e) (h,st) = evalExpr e (newh,st)
                              where newh = M.insert v o h

evalPrim :: Primitive -> [Atom] -> State -> String
evalPrim p (x1:x2:xs) s | p == Add = show (read (evalAtom x1 s) + read (evalAtom x2 s))

evalAtom :: Atom -> State -> String
evalAtom (Literal x) _ = evalLiteral x
evalAtom (Variable x) s@(h,st) = evalObj (lookupHeap x h) s

evalLiteral :: Literal -> String
evalLiteral (Int x) = show x
-}
