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
evalProg (Program ds) h fv = evalMain (lookupHeap "main" h) h fv

-- assume for now that main is a THUNK or CON and evaluate it
evalMain :: Object -> Heap -> FreshVars -> (String, Heap)
evalMain (THUNK e) h fv = evalExpr e h fv
evalMain (CON c as) h fv = evalCON c as h fv
evalMain o h fv = error "bad main"

evalLiteral :: Literal -> String
evalLiteral (Int x) = show x

evalAtom:: Atom -> Heap -> FreshVars -> (String, Heap)
evalAtom (Literal x) h fv = (evalLiteral x, h)
evalAtom (Variable x) h fv = evalObj (lookupHeap x h) h fv

evalExpr ::  Expression -> Heap -> FreshVars -> (String, Heap)
evalExpr (Atom a) h fv = evalAtom a h fv
evalExpr (FunctionCall f k as) h fv = evalFunctionCall f k as h fv
evalExpr (SatPrimCall op as) h fv = evalSatPrimCall op as h fv
evalExpr (Let v o e) h fv = evalLet v o e h fv
evalExpr (Case e as) h fv = evalCase e as h fv

evalFunctionCall :: Variable -> FunctionArity -> [Atom] -> Heap -> FreshVars -> (String, Heap)
evalFunctionCall = error "functioncall not done"

evalSatPrimCall :: Primitive -> [Atom] -> Heap -> FreshVars -> (String, Heap)
evalSatPrimCall p (a1:a2:as) h fv | p == Add = (show (read x1 + read x2), h)
                                          where (x1,h1) = evalAtom a1 h fv
                                                (x2,h2) = evalAtom a2 h fv

evalLet :: Variable -> Object -> Expression -> Heap -> FreshVars -> (String, Heap)
-- todo: make freshvar, replace var in expr, make heap object
evalLet v o e h fv = (debug, h') 
                     where h' = updateHeap h v o
                           (s,h2) = evalObj o h' fv 
                           debug = v ++ "=" ++ s 

evalCase :: Expression -> [Alternative] -> Heap -> FreshVars -> (String, Heap)
evalCase e (a:as) h fv = (s,he)
                    where (value,he)  = evalExpr e h fv
                          (s,ha) = evalAlternative a value he fv -- only doing first alt so far 
                   

evalAlternative :: Alternative -> String -> Heap -> FreshVars -> (String, Heap)
evalAlternative (DefaultAlt n e) v h fv = error "no alt"
                                        
                           

evalObj :: Object -> Heap -> FreshVars -> (String, Heap)
evalObj (FUN vs e) h fv = error "FUN Obj not done"
evalObj (PAP v as) h fv = error "PAP Obj not done"
evalObj (CON c as) h fv = evalCON c as h fv
evalObj (THUNK e) h fv =  error "THUNK Obj not done" 

-- simple CON evaluation as we don't have data types yet
evalCON :: Constructor -> [Atom] -> Heap -> FreshVars -> (String, Heap)
evalCON c as h fv = (con, h)
                   where con = "(" ++ c ++ " " ++ intercalate " " [fst $ evalAtom a h fv | a <- as] ++ ")"

