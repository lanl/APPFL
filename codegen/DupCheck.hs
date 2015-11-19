module DupCheck (
 dupCheck
) where 

-- Need to check for duplicates in
-- Objects/expressions
-- - top-level object names
-- - object names in "let"
-- - variables in individual case patterns
-- - constructors across case alternatives
-- - FUN formal parameters
-- Datatype definitions
-- - tyCons
-- - constructors (globally, not just within a tyCon)

import AST
import ADT
import Data.List

dupCheck :: ([TyCon], [Obj ()]) -> ([TyCon], [Obj ()])
dupCheck = checkTopLevel 

--  ------------------------BEGIN TOP LEVEL FUNCTIONS---------------------------------------
-- checks top level functions
checkTopLevel :: ([TyCon], [Obj ()]) -> ([TyCon], [Obj ()])
checkTopLevel x | startGetDup (getList s) = x
                | otherwise = error "something went wrong"
                  where (f,s) = x

-- gets a list of names of top level functions
getList :: [Obj ()] -> [String]
getList [] = []
getList (x:xs) = case x of 
                      FUN _ _ _ name -> name:(getList xs)
                      PAP _ _ _ name -> name:(getList xs)
                      CON _ _ _ name -> name:(getList xs)
                      THUNK _ _ name -> name:(getList xs)
                      BLACKHOLE _ name -> name:(getList xs)

-- predicate to see if the list of names has no duplicates
noDups :: [String] -> Bool
noDups xs = length (nub xs) == length xs 

-- if there are duplicates in list of names, returns true. else starts making an error message
startGetDup :: [String] -> Bool
startGetDup xs | noDups xs = True
               | otherwise = startError xs

-- starts the creation of an error message
startError :: [String] -> Bool 
startError xs = printDups (group (sort xs)) []

-- creates and concatenates error message
printDups :: [[String]] -> String -> Bool 
printDups [] ys = error ("you have duplicated -- " ++ ys)
printDups (x:xs) ys | length x <= 1 = printDups xs ys
                    | otherwise = printDups xs (ys ++ (head x) ++ " " ++ (show (length x)) ++ " times -- ") 
--  ------------------------END TOP LEVEL FUNCTIONS---------------------------------------
--  ------------------------BEGIN ---------------------------------------


--  ------------------------END ---------------------------------------
