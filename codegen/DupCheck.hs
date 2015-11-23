module DupCheck (
 dupCheck
) where 

-- [-] Need to check for duplicates in
--  [X] Objects/expressions
--   [X] top-level object names
--   [X] object names in "let"
--   [X] variables in individual case patterns
--   [X] constructors across case alternatives
--   [X] FUN formal parameters
--  [-] Datatype definitions
--   [-] tyCons
--   [-] constructors (globally, not just within a tyCon)

import AST
import ADT
import Data.List

-- -----------------------------------BEGIN RECURSIVE TRAVERSAL------------------------------------------------------------
-- Checks for duplicates in a variety of situations
dupCheck :: ([TyCon], [Obj ()]) -> ([TyCon], [Obj ()])
dupCheck (ts,os) = let odups = dupCheckObjs os ("top":[])
                       tdups = dupCheckTyCons ts
                   in case (odups ++ tdups) of 
                      [] -> (ts,os)
                      ys -> error ys 

-- Checks the list of TyCons
dupCheckTyCons :: [TyCon] -> String
dupCheckTyCons ts = []

-- Checks the list of Objects
dupCheckObjs :: [Obj ()] -> [String] -> String
dupCheckObjs os ys = dupCheckTopLevelObjects os ys ++ concatMapGen dupCheckObj os ys 

-- checks OBJ
dupCheckObj :: Obj () -> [String] -> String
dupCheckObj x ys = case x of 
                     PAP{} -> []
                     BLACKHOLE{} -> []
                     CON{} -> []
                     THUNK _ e oname -> dupCheckExpr e (ys ++ (oname:[]))
                     FUN _ vs e oname -> dupCheckVars vs (ys ++ (oname:[])) ++ dupCheckExpr e (ys ++ (oname:[]))

-- checks EXPR
dupCheckExpr :: Expr () -> [String] -> String
dupCheckExpr e ys = case e of 
                      EAtom{} -> []
                      EFCall{} -> []
                      EPrimop{} -> []
                      ELet _ os e -> dupCheckObjs os (ys ++ ("let":[])) ++ dupCheckExpr e (ys ++ ("let":[]))
                      ECase _ ee ealts -> dupCheckExpr ee (ys ++ ("case":[])) ++ dupCheckAlts ealts (ys ++ ("case":[]))

-- checks ALTS
dupCheckAlts :: Alts () -> [String] -> String
dupCheckAlts a ys = case a of 
                      Alts _ alts aname -> dupCheckCons alts (ys ++ (aname:[])) ++ concatMapGen dupCheckAlt alts (ys ++ (aname:[]))

-- Checks ALT
dupCheckAlt :: Alt () -> [String] -> String
dupCheckAlt a ys = case a of 
                     ACon _ ac avs ae -> dupCheckVars avs (ys ++ (ac:[])) ++ dupCheckExpr ae (ys ++ (ac:[]))
                     ADef _ _ ae  -> dupCheckExpr ae (ys ++ ("Default":[]))
-- -----------------------------------END RECURSIVE TRAVERSAL--------------------------------------------------------------


-- -----------------------------------BEGIN CHECKS-------------------------------------------------------------------------

--               --------------Begin Object Check-----------
-- checks objects at top level
dupCheckTopLevelObjects :: [Obj ()] -> [String] -> String
dupCheckTopLevelObjects xs ys = printFunNameDups (group (sort (getFunNameList xs))) [] ys

-- gets a list of names of top level objects
getFunNameList :: [Obj ()] -> [String]
getFunNameList [] = []
getFunNameList (x:xs) = case x of 
                          FUN _ _ _ name -> name:(getFunNameList xs)
                          PAP _ _ _ name -> name:(getFunNameList xs)
                          CON _ _ _ name -> name:(getFunNameList xs)
                          THUNK _ _ name -> name:(getFunNameList xs)
                          BLACKHOLE _ name -> name:(getFunNameList xs)

-- creates and concatenates error message for top level object names
printFunNameDups :: [[String]] -> String -> [String] -> String 
printFunNameDups [] ys zs = ys 
printFunNameDups (x:xs) ys zs | length x <= 1 = printFunNameDups xs ys zs
                              | otherwise = printFunNameDups xs (ys ++ " function name " ++ (head x) ++ " duplicated " ++ (show (length x)) ++ " times " ++ "in location: " ++ (locationToString zs) ++ " -- ") zs
--               --------------End Object Check-------------

--               --------------Begin Variable Check---------
-- checks for duplicates in list of variables     
dupCheckVars :: [Var] -> [String] -> String
dupCheckVars xs ys = printDupVars (group (sort xs)) [] ys

-- prints duplicates in list of variables
printDupVars :: [[Var]] -> String -> [String] -> String
printDupVars [] ys zs = ys 
printDupVars (x:xs) ys zs| length x <= 1 = printDupVars xs ys
                       zs| otherwise = printDupVars xs (ys ++ " variable " ++  (head x) ++ " duplicated " ++ (show (length x)) ++ " times " ++ "in location: " ++ (locationToString zs) ++ " -- ") zs
--               --------------End Variable Check-----------

--               --------------Begin Constructor Check------
-- checks for duplicats of cons in list of ALT
dupCheckCons :: [Alt ()] -> [String] -> String
dupCheckCons xs ys =  printDupCons (group (sort (dupGetCons xs))) [] ys

-- gets list of Cons from list of ALT
dupGetCons :: [Alt ()] -> [Con]
dupGetCons [] = []
dupGetCons (x:xs) = case x of 
                      ACon _ ac _ _ -> ac:(dupGetCons xs)  
                      ADef{} -> []

-- prints duplicates in list of constructors
printDupCons :: [[Con]] -> String -> [String] -> String
printDupCons [] ys zs = ys
printDupCons (x:xs) ys zs | length x <= 1 = printDupCons xs ys zs
                          | otherwise = printDupCons xs (ys ++ " constructor " ++ (head x) ++ " duplicated " ++ (show (length x)) ++ " times " ++ "in location: " ++ (locationToString zs) ++ " -- ") zs
--               --------------End Constructor Check--------

--               --------------Miscellaneous----------------
-- identical to concatMap function, except with another parameter for location
concatMapGen :: (a -> [String] -> String) -> [a] -> [String] -> String
concatMapGen _ [] _ = []
concatMapGen f (x:xs) ys = (f x ys) ++ (concatMapGen f xs ys)

-- converts location to a string for an error message
locationToString :: [String] -> String
locationToString [] = ""
locationToString (x:xs) = x ++ "." ++ (locationToString xs)
                 --------------End Miscellaneous------------

-- -----------------------------------END CHECKS --------------------------------------------------------------------------
