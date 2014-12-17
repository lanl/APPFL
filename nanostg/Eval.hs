module Eval
( eval
) where

import Data.Map as M hiding (map)
import Parser

type Heap = M.Map Variable Object
type FreeVars = [Variable]
type State = (Heap, FreeVars)

initState :: [Declaration] -> State
initState ds = (initHeap(ds), [])

initHeap :: [Declaration] -> Heap
initHeap ds = M.fromList (map declFN ds)

declFN :: Declaration -> (Variable, Object)
declFN (Declaration v o) = (v,o)

eval :: Program -> String
eval prog@(Program ds) = evalProg prog (initState ds)

evalProg :: Program -> State -> String
evalProg (Program ds) s = concat [evalDecl d s | d <- ds]

evalDecl :: Declaration -> State -> String
evalDecl (Declaration "main" o) s = evalObj o s

evalObj :: Object -> State -> String
evalObj (THUNK e) s = evalExpr e s

evalExpr :: Expression -> State -> String
evalExpr (SatPrimCall Add as) s = show (evalAtom (head as) s + evalAtom (last as) s) 

evalAtom :: Atom -> State -> Int
evalAtom (Literal x) _ = evalLiteral x
evalAtom (Variable x) s = error "TODO"

evalLiteral :: Literal -> Int
evalLiteral (Int x) = x

