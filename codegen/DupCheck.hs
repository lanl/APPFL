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
dupCheck = checkTopLevel . checkFunArguments 

--  ------------------------BEGIN TOP LEVEL FUNCTIONS---------------------------------------
-- checks top level functions for duplicate names
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

--  ------------------------BEGIN FUNCTION ARGUMENTS---------------------------------------
-- checks function arguments for duplicate names
checkFunArguments :: ([TyCon], [Obj ()]) -> ([TyCon], [Obj ()])
checkFunArguments x | checkFunArgList (objFunArgList s) = x
                    | otherwise = error "something went wrong"
                      where (f,s) = x
--         -----CONSTRUCTING LIST OF LISTS OF FUNCTION ARGUMENTS-----
-- checks names of function arguments from objects for duplicates
objFunArgList :: [Obj ()] -> [[String]]
objFunArgList [] = {-[]-} error "here2"
objFunArgList (x:xs) = error "here1" {-case x of 
                            FUN _ vs e _ -> {-vs:(exprFunArgList (e:[]))-} error "reached here"
                            PAP _ _ as _ -> (exprFunArgList as)
                            CON _ _ as _ -> exprFunArgList as
                            THUNK _ e _ -> exprFunArgList (e:[])
                            BLACKHOLE _ _ -> []-}

-- checks names of function arguments from expressions for duplicates  
exprFunArgList :: [Expr ()] -> [[String]]
exprFunArgList [] = []
exprFunArgList (x:xs) = case x of 
                             EAtom _ _ -> []
                             EFCall _ _ eas -> exprFunArgList eas
                             EPrimop _ _ eas -> exprFunArgList eas
                             ELet _ edefs ee -> (objFunArgList edefs)++(exprFunArgList (ee:[]))
                             ECase _ ee ealts -> (exprFunArgList (ee:[]))++(altsFunArgList (ealts:[]))  

-- checks names of function arguments from alts for duplicates
altsFunArgList :: [Alts ()] -> [[String]]
altsFunArgList [] = []
altsFunArgList (x:xs) = case x of 
                             Alts _ alts _ -> altFunArgList alts

-- checks names of function arguments from alt for duplicates
altFunArgList :: [Alt ()] -> [[String]]
altFunArgList [] = []
altFunArgList (x:xs) = case x of 
                             ACon _ _ _ ae -> exprFunArgList (ae:[]) 
                             ADef _ _ ae -> exprFunArgList (ae:[])
--           -----END CONSTRUCTING LIST OF LISTS OF FUNCTION ARGUMENTS-----
--           -----CHECK LIST FOR DUPLICATE FUNCTION ARGUMENTS------
-- determines if the list of lists of function arguments contains duplicates
checkFunArgList :: [[String]] -> Bool
checkFunArgList xs = True

-- checks the list of list for duplicate names
checkEntireList :: [[String]] -> Bool
checkEntireList [] = True
checkEntireList (x:xs) = startGetDup x && checkEntireList xs

--  ------------------------END FUNCTION ARGUMENTS---------------------------------------

