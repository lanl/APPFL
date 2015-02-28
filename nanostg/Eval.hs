-- start of big-step interpreter for stg-like lang.

module Eval
( eval
) where

import Data.Map as M hiding (map, filter, split)
import Data.Maybe
import Data.List
import Data.Char
import AST
import Replace

type Heap = M.Map Variable Object
type FreshVars = [Variable]
type State = (Heap, FreshVars)
type Output = String

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
-- Let Expression
evalExpression (Let ls e) st@(h,fv)  
   = evalExpression e1 (h1,fv1)   -- should we evalExpression here?
    where (v,o) = head ls -- only doing first let for now
          (v', fv1) = getFresh fv
          a = Variable v'
          h1 = updateHeap h v' o
          e1 = replace (v,a) [] e

-- CaseCon Expression
evalExpression (Case (Atom (Variable v)) alts) st@(h,fv) 
    | isCON obj = evalExpression e1 st 
                where obj = M.lookup v h
                      Just (c, as) = getCON obj 
                      Just (xs,e) = matchAlt c alts 
                      -- list of replacements
                      reps = zip xs as
                      e1 = replaceMany reps [] e  
                      
-- CaseAny Expression
evalExpression (Case (Atom v) alts) st@(h,fv) 
    | isLiteral v || isValue v h = evalExpression e1 st
                                 where Just (x,e) = matchDefaultAlt alts
                                       e1 = replace (x,v) [] e
      
-- Case Expression
evalExpression (Case e alts) st@(h,fv) 
    = evalExpression (Case e1 alts) st1
    where (e1, st1) = evalExpression e st
          --e1 is atom now (either literal or pointer to heap object)

-- Saturated Primitive Expression
evalExpression (SatPrimCall op (a1:a2:as)) st 
    | op == Add = (Atom $ Literal $ Int (x1+x2), st)
    | op == Sub = (Atom $ Literal $ Int (x1-x2), st)
    | op == Mul = (Atom $ Literal $ Int (x1*x2), st)
    | op == Div = (Atom $ Literal $ Int (div x1 x2), st)
               where x1 = read b1 :: Int  
                     x2 = read b2 :: Int 
                     b1 = showAtom $ fst $ evalAtom a1 st
                     b2 = showAtom $ fst $ evalAtom a2 st

-- FunctionCall Expression
evalExpression (FunctionCall f k as) st@(h,fv) 
    = evalExpression e1 st
    where Just (FUN xs e)  = M.lookup f h
          reps = zip xs as
          e1 = replaceMany reps [] e  

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

isLiteral :: Atom -> Bool
isLiteral (Literal _) = True
isLiteral _ = False     

isValue :: Atom -> Heap -> Bool
isValue (Variable v) h 
    = if M.lookup v h == Nothing then False else True
isValue (Literal _) _ = False

getCON :: Maybe Object -> Maybe (Constructor, [Atom])
getCON (Just (CON c as )) = Just (c,as)
getCON _ = Nothing 

isCON :: Maybe Object -> Bool
isCON (Just (CON _  _)) = True
isCON _ = False   
    

matchAlt :: Constructor -> [Alternative] -> Maybe ([Variable], Expression)
matchAlt c1 ((Alt c2 xs e):alts) 
    = if c1 == c2 then Just (xs, e) else matchAlt c1 alts
matchAlt _ _ = Nothing

matchDefaultAlt :: [Alternative] ->  Maybe (Variable, Expression)
matchDefaultAlt ((Alt _ _ _ ):alts) = matchDefaultAlt alts
matchDefaultAlt ((DefaultAlt v e):alts) = Just (v,e)
matchDefaultAlt _ = Nothing

showExpression :: Expression -> Output
showExpression (Atom a) = showAtom a
showExpression e = "debug " ++ show e

showAtom :: Atom -> Output
showAtom (Literal (Int x)) = show x
showAtom (Variable x) = x

showObject :: Object -> Output
showObject (CON c as) = showCON c as

showCON :: Constructor -> [Atom] -> Output
showCON c as = "(" ++ c ++ " " ++ intercalate " " [showAtom a | a <- as] ++ ")"

