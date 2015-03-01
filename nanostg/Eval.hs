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
          
evalString :: [Char] -> Eval.Output
evalString = eval.parseString
 
eval :: Program -> Output
eval prog@(Program ds) 
    = fst $ evalProgram prog (initHeap ds, initFreshVars)

evalProgram :: Program -> State -> (Output, State)
evalProgram (Program ds) st
      = (display e2, st2) 
      where (e1, st1) = evalExpression (Atom (Variable "main")) st
            -- e' is a atom here
            (e2,st2) = evalFinalExpression e1 st1

evalFinalExpression :: Expression -> State -> (Expression, State)
evalFinalExpression (Atom a) st = (Atom a', st')
                               where (a',st') = evalAtom a st
evalFinalExpression e _ = error ("non atom expression " ++  display e)

-- Update
evalUpdate :: Variable -> Expression -> State -> State
evalUpdate x (Atom (Variable y)) st@(h,fv) 
    = (updateHeap h x (lookupHeap y h), fv)

evalExpression :: Expression -> State -> (Expression, State)

-- THUNK 
evalExpression (Atom (Variable x)) st@(h,fv) 
    | isTHUNK obj = (e1, evalUpdate x e1 st1)
                  where obj = M.lookup x h
                        Just e = getTHUNK obj
                        h1 = updateHeap h x BLACKHOLE
                        (e1, st1) = evalExpression e (h1,fv)
  
-- Let Expression
evalExpression (Let ((v,o):ls) e) st@(h,fv)  
    | ls == [] = (e1, (h1,fv1))
    where (v', fv1) = getFresh fv
          a = Variable v'
          h1 = updateHeap h v' o
          e1 = replace (v,a) [] e

-- Letrec Expression
evalExpression (Let ls e) st@(h,fv)  
    = error "no letrec"

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
                     b1 = display $ fst $ evalAtom a1 st
                     b2 = display $ fst $ evalAtom a2 st

-- Knowncall Expression
evalExpression (FunctionCall f k as) st@(h,fv) 
    = evalExpression e1 st
    where Just (FUN xs e)  = M.lookup f h
          reps = zip xs as
          e1 = replaceMany reps [] e  

-- default if no rules apply just return the expression unchanged
evalExpression e st = (e,st)


evalAtom :: Atom -> State -> (Atom, State)
evalAtom (Literal x) st = (Literal x, st)
evalAtom (Variable x) st@(h,fv) = (Variable x', st')
                                where (obj, st') = evalObject (lookupHeap x h) st
                                      x' = display obj 

evalObject :: Object -> State -> (Object, State)
evalObject (CON c as) st = (CON c as', st)
                           where as' = [fst $ evalAtom a st | a <- as]
evalObject _ _ =  error "eval Non CON object"

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

getTHUNK :: Maybe Object -> Maybe Expression 
getTHUNK (Just (THUNK e)) = Just e
getTHUNK _ = Nothing 
    
isTHUNK :: Maybe Object -> Bool
isTHUNK (Just (THUNK _)) = True
isTHUNK _ = False   

matchAlt :: Constructor -> [Alternative] -> Maybe ([Variable], Expression)
matchAlt c1 ((Alt c2 xs e):alts) 
    = if c1 == c2 then Just (xs, e) else matchAlt c1 alts
matchAlt _ _ = Nothing

matchDefaultAlt :: [Alternative] ->  Maybe (Variable, Expression)
matchDefaultAlt ((Alt _ _ _ ):alts) = matchDefaultAlt alts
matchDefaultAlt ((DefaultAlt v e):alts) = Just (v,e)
matchDefaultAlt _ = Nothing

