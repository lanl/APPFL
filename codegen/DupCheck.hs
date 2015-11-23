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

-- -----------------------------------BEGIN RECURSIVE TRAVERSAL------------------------------------------------------------------------
-- Checks for duplicates in a variety of situations
dupCheck :: ([TyCon], [Obj ()]) -> ([TyCon], [Obj ()])
dupCheck (ts,os) = let odups = dupCheckObjs os
                       tdups = dupCheckTyCons ts
                   in case (odups ++ tdups) of 
                      [] -> (ts,os)
                      ys -> error ys 

-- Checks the list of TyCons
dupCheckTyCons :: [TyCon] -> String
dupCheckTyCons ts = []

-- Checks the list of Objects
dupCheckObjs :: [Obj ()] -> String
dupCheckObjs os = dupCheckTopLevelObjects os ++ concatMap dupCheckObj os 

-- checks OBJ
dupCheckObj :: Obj () -> String
dupCheckObj x = case x of 
                  PAP{} -> []
                  BLACKHOLE{} -> []
                  CON{} -> []
                  THUNK _ e _ -> dupCheckExpr e
                  FUN _ vs e _ -> dupCheckVars vs ++ dupCheckExpr e

-- checks EXPR
dupCheckExpr :: Expr () -> String
dupCheckExpr e = case e of 
                   EAtom{} -> []
                   EFCall{} -> []
                   EPrimop{} -> []
                   ELet _ os e -> dupCheckObjs os ++ dupCheckExpr e 
                   ECase _ ee ealts -> dupCheckExpr ee ++ dupCheckAlts ealts

-- checks ALTS
dupCheckAlts :: Alts () -> String
dupCheckAlts a = case a of 
                   Alts _ alts _ -> dupCheckCons alts ++ concatMap dupCheckAlt alts

-- Checks ALT
dupCheckAlt :: Alt () -> String
dupCheckAlt a = case a of 
                  ACon _ _ avs ae -> dupCheckVars avs ++ dupCheckExpr ae
                  ADef _ _ ae  -> dupCheckExpr ae
-- -----------------------------------END RECURSIVE TRAVERSAL--------------------------------------------------------------------------


-- -----------------------------------BEGIN CHECKS-------------------------------------------------------------------------------------

--               --------------Begin Object Check-----------
-- checks objects at top level
dupCheckTopLevelObjects :: [Obj ()] -> String
dupCheckTopLevelObjects xs =  printFunNameDups (group (sort (getFunNameList xs))) []

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
printFunNameDups :: [[String]] -> String -> String 
printFunNameDups [] ys = ys 
printFunNameDups (x:xs) ys | length x <= 1 = printFunNameDups xs ys
                          | otherwise = printFunNameDups xs (ys ++ " function name " ++ (head x) ++ " duplicated " ++ (show (length x)) ++ " times -- ") 
--               --------------End Object Check-------------

--               --------------Begin Variable Check---------
-- checks for duplicates in list of variables     
dupCheckVars :: [Var] -> String
dupCheckVars xs = printDupVars (group (sort xs)) []

-- prints duplicates in list of variables
printDupVars :: [[Var]] -> String -> String
printDupVars [] ys = ys 
printDupVars (x:xs) ys | length x <= 1 = printDupVars xs ys
                       | otherwise = printDupVars xs (ys ++ " function variable " ++  (head x) ++ " duplicated " ++ (show (length x)) ++ " times -- ")
--               --------------End Variable Check-----------

--               --------------Begin Constructor Check------
-- checks for duplicats of cons in list of ALT
dupCheckCons :: [Alt ()] -> String
dupCheckCons xs =  printDupCons (group (sort (dupGetCons xs))) []

-- gets list of Cons from list of ALT
dupGetCons :: [Alt ()] -> [Con]
dupGetCons [] = []
dupGetCons (x:xs) = case x of 
                      ACon _ ac _ _ -> ac:(dupGetCons xs)  
                      ADef{} -> []

-- prints duplicates in list of constructors
printDupCons :: [[Con]] -> String -> String
printDupCons [] ys = ys
printDupCons (x:xs) ys | length x <= 1 = printDupCons xs ys
                       | otherwise = printDupCons xs (ys ++ " constructor " ++ (head x) ++ " duplicated " ++ (show (length x)) ++ " times -- ")
--               --------------End Constructor Check--------

-- -----------------------------------END CHECKS --------------------------------------------------------------------------------------
