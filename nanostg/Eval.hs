{-# LANGUAGE FlexibleInstances #-}

module Eval
( eval
, replace -- debug
) where

import Data.Map as M hiding (map, filter, split)
import Data.Maybe
import Data.List
import Data.Char
import Parser

type Heap = M.Map Variable Object
type FreshVars = [Variable]
type State = (Heap, FreshVars)
type BoundVars = [Variable]
type Output = String
type Lets = [(Variable, Object)]

-- setup heap and add Decls to it
initHeap :: [Declaration] -> Heap
initHeap ds = M.fromList (map declFN ds)

declFN :: Declaration -> (Variable, Object)
declFN (Declaration v o) = (v,o)

lookupHeap :: Variable -> Heap -> Object
lookupHeap v h | lookup == Nothing = error ("can't find var " ++ v)
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

getFresh :: FreshVars -> (Variable, FreshVars)
getFresh fv = (head fv1, fv2)
            where (fv1,fv2) = split $ spliter fv
           
eval :: Program -> Output
eval prog@(Program ds) 
    = fst $ evalProg prog (initHeap ds, initFreshVars)

evalProg :: Program -> State -> (Output, State)
evalProg (Program ds) st@(h,fv)
    = evalMain (lookupHeap "main" h) st 

-- assume for now that main is a THUNK or CON and evaluate it
evalMain :: Object -> State -> (Output, State)
evalMain (THUNK e) st = (showExpression e', st') 
                              where (e', st') = evalExpression e st
evalMain (CON c as) st = (showCON c as', st) 
                         where as' = [fst $ evalAtom a st | a <- as] 
evalMain o st = error "bad main"

evalExpression :: Expression -> State -> (Expression, State)
evalExpression (Atom a) st = (Atom a', st')
                             where (a',st') = evalAtom a st
evalExpression (Let ls e) st = evalLet ls e st
evalExpression (Case e as) st = evalCase e as st 
evalExpression (SatPrimCall op as) st = evalSatPrimCall op as st

evalAtom :: Atom -> State -> (Atom, State)
evalAtom (Literal x) st = (Literal x, st)
evalAtom (Variable x) st@(h,fv) = (Variable x', st')
                                where (obj, st') = evalObject (lookupHeap x h) st
                                      x' = showObject obj 

evalObject :: Object -> State -> (Object, State)
evalObject (FUN vs e) st = error "FUN Obj not done"
evalObject (PAP v as) st = error "PAP Obj not done"
evalObject (CON c as) st = (CON c as', st)
                           where as' = [fst $ evalAtom a st | a <- as]
evalObject (THUNK e) st =  error "THUNK Obj not done"

evalLet :: [(Variable,Object)] -> Expression -> State -> (Expression, State)
evalLet ls e st@(h,fv)
    = (e2, (h2, fv2)) 
    where (v,o) = head ls -- only doing first let for now
          (v', fv1) = getFresh fv
          a = Variable v'
          h1 = updateHeap h v' o
          e1 = replace (v,a) [] e
          -- as a test eval here really only should do this in case stmt
          (e2, (h2,fv2)) = evalExpression e1 (h1,fv1)   

evalCase :: Expression -> [Alternative] -> State -> (Expression, State)
-- CaseCon
evalCase (Atom (Variable v)) alts st@(h,fv)
    | isCON obj =  error "no casecon"
                where obj = M.lookup v h
                      (Just as) = getCONAtoms obj 
-- CaseAny
evalCase (Atom v) alts st@(h,fv)
    | isLiteral v || isValue v h = error "no caseany"

evalCase e alts st@(h,fv)
    = (e2, st2)
    where (e1, st1) = evalExpression e st
          --e' is atom now?
          (e2, st2) = evalCase e1 alts st1

isLiteral :: Atom -> Bool
isLiteral (Literal _) = True
isLiteral _ = False     

isValue :: Atom -> Heap -> Bool
isValue (Variable v) h 
    = if M.lookup v h == Nothing then False else True
isValue (Literal _) _ = False

getCONAtoms :: Maybe Object -> Maybe [Atom]
getCONAtoms (Just (CON _ as )) = Just as
getCONAtoms _ = Nothing 

isConstructor :: Atom -> Heap -> Bool
isConstructor (Variable v) h = isCON (M.lookup v h)
isConstructor (Literal _) _ = False 

isCON :: Maybe Object -> Bool
isCON (Just (CON _  _)) = True
isCON _ = False   
    
evalSatPrimCall :: Primitive -> [Atom] -> State -> (Expression, State)
evalSatPrimCall p (a1:a2:as) st 
    | p == Add = (Atom $ Literal $ Int (x1+x2), st)
               where x1 = read (showAtom a1) :: Int  
                     x2 = read (showAtom a2) :: Int 

matchAlt :: Constructor -> [Alternative] -> [([Variable], Expression)]
matchAlt c1 ((Alt c2 xs e):alts) 
    = if c1 == c2 then [(xs, e)] else matchAlt c1 alts
matchAlt _ _ = [] 

matchDefaultAlt :: [Alternative] ->  [(Variable, Expression)]
matchDefaultAlt ((Alt _ _ _ ):alts) = matchDefaultAlt alts
matchDefaultAlt ((DefaultAlt v e):alts) = [(v,e)]
matchDefaultAlt _ = []

showExpression :: Expression -> Output
showExpression (Atom a) = showAtom a

showAtom :: Atom -> Output
showAtom (Literal (Int x)) = show x
showAtom (Variable x) = x

showObject :: Object -> Output
showObject (CON c as) = showCON c as

showCON :: Constructor -> [Atom] -> Output
showCON c as = "(" ++ c ++ " " ++ intercalate " " [showAtom a | a <- as] ++ ")"


{-
evalLiteral : Literal -> Outputtom a) h fv = evalAtom a h fv
--evalLiteral (Int x) =   "I#" ++ show x
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
    = (s, h') 
    where (v,o) = head vos -- only doing first let for now
          h' = updateHeap h v o
          (s,h2) = evalObj o h' fv 
          debug = v ++ "=" ++ s 

evalCase :: Expression -> [Alternative] -> Heap -> FreshVars -> (Output, Heap)
evalCase e (a:as) h fv 
    = (s ,he)
    where (y@(x:xs),he) = evalExpr e h fv
          isValue = isUpper x
          lookup = M.lookup y h
          isLit = lookup == Nothing
          debug = if isValue then "value " else
                    if isLit then "literal " else "variable " 
          
          -- only deal w/ literal
          (s,h') = evalAlternative a y he fv -- only doing first alt so far 

evalAlternative :: Alternative -> String -> Heap -> FreshVars -> (Output, Heap)
evalAlternative (DefaultAlt v e) s h fv = evalDefaultAlt v e s h fv
evalAlternative (Alt c vs e) s h fv = evalAlt c vs e s h fv 

evalAlt :: Constructor -> [Variable] -> Expression -> String -> Heap -> FreshVars -> (Output, Heap)
evalAlt c vs e s h fv = (debug, h)
                           where debug = "alt " ++ c ++ show vs ++ " " ++ stripParen s 


stripParen :: String -> String
stripParen = stripChars "()" 

stripChars :: String -> String -> String
stripChars = filter . flip notElem


evalDefaultAlt :: Variable -> Expression -> String -> Heap -> FreshVars -> (Output, Heap)
-- replace v w/ y in e
evalDefaultAlt v e (x:xs) h fv = (out, h')
                                 where isValue = isUpper x
                                       a = if isValue then error "Value!" else makeAtom (x:xs) h
                                       e' = replace (v,a) [] e 
                                       (out, h') = evalExpr e' h fv

--we must be evaling to far if we have to do this.. 
makeAtom :: String -> Heap -> Atom
makeAtom y h = if isLit then Literal (Int (read y)) else (Variable y)
                  where  lookup = M.lookup y h
                         isLit = lookup == Nothing

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
-}
-- replace a non-bound variable in a expression

type Replacement = (Variable, Atom) --todo make list?

class Replace a where replace ::  Replacement -> BoundVars -> a -> a

instance Replace Expression where
  replace r bvs (Atom a) 
    = Atom (replace r bvs a)

  replace r bvs (FunctionCall f k as) 
    = FunctionCall f k as'
    where as' = replace r bvs as 

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

instance Replace Variable where
  replace (vin, Variable vout) bvs v 
    = if v == vin && (notElem vin bvs) then vout else v

-- need to update boundvars
instance Replace Alternative where
  replace r bvs (DefaultAlt v e) 
    = DefaultAlt v e'
    where e' = replace r (v:bvs) e

  replace r bvs (Alt c vs e) 
    = Alt c vs e'
    where e' = replace r (vs++bvs) e

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

