-- start of big-step interpreter for stg-like lang.

module Eval
( eval
, evalString
) where

import Data.Map as M hiding (map, filter, split)
import Data.Maybe
import Data.Char
import AST
import Parser
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
lookupHeap v h | lookup == Nothing = error ("can't find var " ++ v ++ " in " ++ show h)
               | otherwise = fromJust lookup
                where lookup = M.lookup v h

updateHeapVar :: Heap -> Variable -> Object -> Heap
updateHeapVar h v o = M.insert v o h 

-- do a bunch of vars at once
updateHeapVars :: Heap -> [Variable] -> [Object] -> Heap
updateHeapVars h (v:vs) (o:os) = updateHeapVars (M.insert v o h) vs os
updateHeapVars h [] [] = h 

initFreshVars :: FreshVars
initFreshVars = ['$':show i | i <- [0..]]

spliter :: [a] -> [(a,a)]
spliter(x:y:zs) = (x,y):spliter zs

split :: [(a,a)] -> ([a],[a])
split xs = (map fst xs, map snd xs) 

getFreshVars :: Int -> FreshVars -> ([Variable], FreshVars)
getFreshVars n fv = (take n fv1, fv2)
            where (fv1,fv2) = split $ spliter fv

getFreshVar :: FreshVars -> (Variable, FreshVars)
getFreshVar fv = (v,fv1)  
               where ([v],fv1) = getFreshVars 1 fv

evalString :: [Char] -> Eval.Output
evalString = eval.parseString
 
eval :: Program -> Output
eval prog@(Program ds) 
    = fst $ evalProgram prog (initHeap ds, initFreshVars)

evalProgram :: Program -> State -> (Output, State)
evalProgram (Program ds) st
      = (display e1, st1) 
      where (e1, st1) = evalLoop (Atom (Variable "main")) st

evalLoop :: Expression -> State -> (Expression, State)
evalLoop (Atom a) st@(h,fv) | isLiteral a || isValue a h 
  = (Atom a', st')
  where (a',st') = evalAtom a st
-- not Literal or Value try again
evalLoop e st = evalLoop e' st'
              where (e',st') = evalExpression e st

-- Update
evalUpdate :: Variable -> Expression -> State -> State
evalUpdate x (Atom (Variable y)) st@(h,fv) 
    = (updateHeapVar h x (lookupHeap y h), fv)
evalUpdate x e st = st -- do nothing

evalExpression :: Expression -> State -> (Expression, State)

-- THUNK 
evalExpression (Atom (Variable x)) st@(h,fv) 
    | isTHUNK obj = (e, evalUpdate x e (h1,fv))
                  where obj = M.lookup x h
                        Just e = getTHUNK obj
                        h1 = updateHeapVar h x BLACKHOLE
  
-- Let Expression (only 1 let)
evalExpression (Let ((v,o):ls) e) st@(h,fv)  
    | ls == [] = (e1, (h1,fv1))
    where (v', fv1) = getFreshVar fv
          a = Variable v'
          e1 = replace (v,a) [] e
          h1 = updateHeapVar h v' o

-- Letrec Expression
evalExpression (Let ls e) st@(h,fv)  
    = (e1, (h1,fv1))
    where (vs,os) = unzip ls
          (vs1,fv1) = getFreshVars (length ls) fv
          as = [Variable v | v <- vs1]
          reps = zip vs as
          os1 = replaceMany reps [] os
          e1 = replaceMany reps [] e
          h1 = updateHeapVars h vs1 os1

-- CaseCon Expression
-- if is constructor and no match then fall though to caseAny
evalExpression (Case (Atom (Variable v)) alts) st@(h,fv) 
    | isCON obj && match /= Nothing = (e1,st) 
                where obj = M.lookup v h
                      Just (c, as) = getCON obj 
                      match = matchAlt c alts 
                      Just (xs,e) = match
                      -- list of replacements
                      reps = zip xs as
                      e1 = replaceMany reps [] e  
                      
-- CaseAny Expression
evalExpression (Case (Atom a) alts) st@(h,fv) 
    | isLiteral a || isValue a h = (e1, st) 
                                 where Just (x,e) = matchDefaultAlt alts
                                       e1 = replace (x,a) [] e
      
-- Case Expression
evalExpression (Case e alts) st@(h,fv) 
    = ((Case e1 alts), st1)
    where (e1, st1) = evalLoop e st
          --e1 is atom now (either literal or pointer to heap object)

-- Saturated Primitive Expression
evalExpression (SatPrimCall op as) st 
    | op == Add = (Atom $ Literal $ Int (x1+x2), st)
    | op == Sub = (Atom $ Literal $ Int (x1-x2), st)
    | op == Mul = (Atom $ Literal $ Int (x1*x2), st)
    | op == Div = (Atom $ Literal $ Int (div x1 x2), st)
    | op == Equal = (Atom $ Literal $ Int (compareop (==) x1 x2), st) 
    | op == NotEqual = (Atom $ Literal $ Int (compareop (/=) x1 x2), st) 
    | op == LessThan = (Atom $ Literal $ Int (compareop (<) x1 x2), st) 
    | op == GreaterThan = (Atom $ Literal $ Int (compareop (>) x1 x2), st) 
    | op == LessThanOrEqual = (Atom $ Literal $ Int (compareop (<=) x1 x2), st) 
    | op == GreaterThanOrEqual = (Atom $ Literal $ Int (compareop (>=) x1 x2), st) 
    | op == IntToBool = evalBool (head as) st
               where x1 = read (display $ fst $ evalAtom (head as) st) :: Int  
                     x2 = read (display $ fst $ evalAtom (as !! 1) st) :: Int 
                     

-- Knowncall Expression
evalExpression (FunctionCall f k as) st@(h,fv) 
    = evalExpression e1 st
    where Just (FUN xs e)  = M.lookup f h
          reps = zip xs as
          e1 = replaceMany reps [] e  

-- if no rules apply 
evalExpression e st = error ("no eval rule for " ++ show e)


-- In this case we need to make a True or False constructor
-- add it to the heap and return the (fresh) variable that points to it
evalBool :: Atom -> State -> (Expression, State) 
evalBool (Literal (Int x)) st@(h,fv) 
    = (a1,(h1,fv1)) 
    where (v, fv1) = getFreshVar fv
          -- [] at the end as bool constructor takes no args
          newobj = CON (if x == 1 then "True" else "False") []
          h1 = updateHeapVar h v newobj
          a1 = Atom $ Variable v

evalAtom :: Atom -> State -> (Atom, State)
evalAtom (Literal x) st = (Literal x, st)
evalAtom (Variable x) st@(h,fv) = (Variable x', st')
                                where (obj, st') = evalObject (lookupHeap x h) st
                                      x' = display obj 

evalObject :: Object -> State -> (Object, State)
evalObject (CON c as) st = (CON c as', st)
                           where as' = [fst $ evalAtom a st | a <- as]
evalObject o _ =  error ("eval Non CON object" ++ show o)

isLiteral :: Atom -> Bool
isLiteral (Literal _) = True
isLiteral _ = False     

isValue :: Atom -> Heap -> Bool
isValue (Variable v) h 
    = if obj == Nothing then False else isValueObj obj
    where obj = M.lookup v h
isValue (Literal _) _ = False

-- FUN, PAP, CON are Values
isValueObj :: Maybe Object -> Bool
isValueObj (Just (FUN _ _)) = True
isValueObj (Just (PAP _ _)) = True
isValueObj (Just (CON _ _)) = True
isValueObj _ = False

getCON :: Maybe Object -> Maybe (Constructor, [Atom])
getCON (Just (CON c as )) = Just (c,as)
getCON _ = Nothing 

isCON :: Maybe Object -> Bool
isCON (Just (CON _  _)) = True
isCON _ = False   

getTHUNK :: Maybe Object -> Maybe Expression 
getTHUNK (Just (THUNK e)) = Just e
getTHUNK _ = Nothing 
    
isTHUNK :: Maybe Object -> Bool
isTHUNK (Just (THUNK _)) = True
isTHUNK _ = False   

matchAlt :: Constructor -> [Alternative] -> Maybe ([Variable], Expression)
matchAlt c1 (a@(Alt c2 xs e):alts) 
    = if c1 == c2 then Just (xs, e) else matchAlt c1 alts
matchAlt c alts = Nothing

matchDefaultAlt :: [Alternative] ->  Maybe (Variable, Expression)
matchDefaultAlt ((Alt _ _ _ ):alts) = matchDefaultAlt alts
matchDefaultAlt ((DefaultAlt v e):alts) = Just (v,e)
matchDefaultAlt alts = error ("matchDefaultAlt " ++ show alts)  

compareop ::  (Int -> Int -> Bool) -> Int -> Int -> Int
compareop op x y = if op x y then 1 else 0 

