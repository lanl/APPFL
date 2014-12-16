module Eval
( eval
) where

import Parser

eval :: Program -> String
eval (Program ds) = concat [evalDecl d | d <- ds]

evalDecl :: Declaration -> String
evalDecl (Declaration "main" o) = evalObj o

evalObj :: Object -> String
evalObj (THUNK e) = evalExpr e

evalExpr :: Expression -> String
evalExpr (SatPrimCall Add as) = show (evalAtom(head as) + evalAtom(last as)) 

evalAtom :: Atom -> Int
evalAtom (Literal x) = evalLiteral x

evalLiteral :: Literal -> Int
evalLiteral (Int x) = x

