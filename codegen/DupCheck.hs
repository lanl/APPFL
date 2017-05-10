{-# LANGUAGE NamedFieldPuns    #-}

module DupCheck (
 dupCheck
) where

-- [X] Need to check for duplicates in
--  [X] Objects/expressions
--   [X] top-level object names
--   [X] object names in "let"
--   [X] variables in individual case patterns
--   [X] constructors across case alternatives
--   [X] FUN formal parameters
--  [X] Datatype definitions
--   [X] tyCons
--   [X] constructors (globally, not just within a tyCon)

import           AST
import           ADT
import           Data.List
import qualified Data.Set as Set
import qualified Data.MultiSet as MultiSet

-- Checks for duplicates in list of TyCons and list of Objects
-- At the source level we only allow a single type signature per name, top-level
dupCheck :: ([TyCon], [Obj ()], [(Var, Monotype)]) -> ([TyCon], [Obj ()], Assumptions)
dupCheck (ts,os,ss) = let odups = dupCheckObjs os ["toplevel"]
                          tdups = dupCheckTyCons ts ++ dupCheckDataCons ts
                          sdups = dupCheckTypeSigs ss
                      in case odups ++ tdups ++ sdups of
                           [] -> (ts, os, Set.fromList ss)
                           ys -> error ys

--               --------------Begin OBJ/EXPR/ALTS/ALT Recursive Traversal------------
-- Checks the list of Objects
dupCheckObjs :: [Obj ()] -> [String] -> String
dupCheckObjs os ys = dupCheckTopLevelObjects os ys ++ concatMapGen dupCheckObj os ys

-- checks OBJ
dupCheckObj :: Obj () -> [String] -> String
dupCheckObj x ys = case x of
                     PAP{} -> []
--BH                     BLACKHOLE{} -> []
                     CON{} -> []
                     THUNK{e,oname} -> dupCheckExpr e (ys ++ [oname])
                     FUN{vs,e,oname} -> dupCheckVars vs (ys ++ [oname]) ++ dupCheckExpr e (ys ++ [oname])

-- checks EXPR
dupCheckExpr :: Expr () -> [String] -> String
dupCheckExpr expr ys = case expr of
                         EAtom{} -> []
                         EFCall{} -> []
                         EPrimOp{} -> []
                         ELet{edefs,ee} -> dupCheckObjs edefs (ys ++ ["let"])
                                        ++ dupCheckExpr ee (ys ++ ["let"])
                         ECase{ee,ealts} -> dupCheckExpr ee (ys ++ ["case"])
                                         ++ dupCheckAlts ealts (ys ++ ["case"])

-- checks ALTS
dupCheckAlts :: Alts () -> [String] -> String
dupCheckAlts a ys = case a of
                      Alts{alts,aname} -> dupCheckCons alts (ys ++ [aname])
                                       ++ concatMapGen dupCheckAlt alts (ys ++ [aname])

-- Checks ALT
dupCheckAlt :: Alt () -> [String] -> String
dupCheckAlt a ys = case a of
                     ACon{avs,ae,ac} -> dupCheckVars avs (ys ++ [ac]) ++ dupCheckExpr ae (ys ++ [ac])
                     ADef{ae} -> dupCheckExpr ae (ys ++ ["def"])
--               --------------End OBJ/EXPR/ALTS/ALT Recursive Traversal--------------


-- -----------------------------------BEGIN CHECKS-------------------------------------------------------------------------

dupCheckTypeSigs :: [(Var, a)] -> String
dupCheckTypeSigs ss = let names = map fst ss
                          msNames = MultiSet.fromList names
                          msUniqueNames = MultiSet.fromList $ MultiSet.distinctElems msNames
                          dupList = MultiSet.toList $ MultiSet.difference msNames msUniqueNames
                      in "duplicate type signatures for [" ++ intercalate "," dupList ++ "]"

--               --------------Begin Object Check-----------
-- checks objects at top level
dupCheckTopLevelObjects :: [Obj ()] -> [String] -> String
dupCheckTopLevelObjects xs = printFunNameDups (group (sort (map oname xs))) []

-- creates and concatenates error message for top level object names
printFunNameDups :: [[String]] -> String -> [String] -> String
printFunNameDups [] ys zs = ys
printFunNameDups (x:xs) ys zs | length x <= 1 = printFunNameDups xs ys zs
                              | otherwise = printFunNameDups xs (ys
                                            ++ " function name "
                                            ++ head x
                                            ++ " duplicated "
                                            ++ show (length x)
                                            ++ " times "
                                            ++ "in location: "
                                            ++ locationToString zs
                                            ++ " -- ") zs
--               --------------End Object Check-------------

--               --------------Begin Variable Check---------
-- checks for duplicates in list of variables
dupCheckVars :: [Var] -> [String] -> String
dupCheckVars xs = printDupVars (group (sort xs)) []

-- prints duplicates in list of variables
printDupVars :: [[Var]] -> String -> [String] -> String
printDupVars [] ys zs = ys
printDupVars (x:xs) ys zs | length x <= 1 = printDupVars xs ys zs
                          | otherwise = printDupVars xs (ys
                                        ++ " variable "
                                        ++  head x
                                        ++ " duplicated "
                                        ++ show (length x)
                                        ++ " times "
                                        ++ "in location: "
                                        ++ locationToString zs
                                        ++ " -- ") zs
--               --------------End Variable Check-----------

--               --------------Begin Constructor Check------
-- checks for duplicats of cons in list of ALT
dupCheckCons :: [Alt ()] -> [String] -> String
dupCheckCons xs =  printDupCons (group (sort (dupGetCons xs))) []

-- gets list of Cons from list of ALT
dupGetCons :: [Alt ()] -> [Con]
dupGetCons [] = []
dupGetCons (x:xs) = case x of
                      ACon{ac} -> ac:(dupGetCons xs)
                      ADef{} -> []

-- prints duplicates in list of constructors
printDupCons :: [[Con]] -> String -> [String] -> String
printDupCons [] ys zs = ys
printDupCons (x:xs) ys zs | length x <= 1 = printDupCons xs ys zs
                          | otherwise = printDupCons xs (ys
                                        ++ " constructor "
                                        ++ head x
                                        ++ " duplicated "
                                        ++ show (length x)
                                        ++ " times "
                                        ++ "in location: "
                                        ++ locationToString zs
                                        ++ " -- ") zs
--               --------------End Constructor Check--------

--               --------------Begin TyCon Check------------
-- checks for duplicates of Cons in list of TyCons
dupCheckTyCons :: [TyCon] -> String
dupCheckTyCons ts = printDupTyCons (group (sort (dupGetTyCons ts))) []

-- get list of Cons from list of TyCons
dupGetTyCons :: [TyCon] -> [Con]
dupGetTyCons [] = []
dupGetTyCons (x:xs) = case x of
                        TyCon _ con _ _  -> con:(dupGetTyCons xs)

-- prints duplicates in list of Cons from TyCons
printDupTyCons :: [[Con]] -> String ->  String
printDupTyCons [] ys = ys
printDupTyCons (x:xs) ys | length x <= 1 = printDupTyCons xs ys
                         | otherwise = printDupTyCons xs (ys
                                       ++ " data type "
                                       ++ head x
                                       ++ " duplicated "
                                       ++ show (length x)
                                       ++ " times "
                                       ++ " -- ")
--               --------------End TyCon Check--------------

--               --------------Begin DataCon Check----------
-- checks for duplicates of DataCons in list of TyCons
dupCheckDataCons :: [TyCon] -> String
dupCheckDataCons ts = printDupDataCons (group (sort a)) [] b
                      where (a,b) = invert (dupGetDataCons ts)

-- get List of DataCons from list of TyCons
dupGetDataCons :: [TyCon] -> [(Con,(Con,Con))]
dupGetDataCons [] = []
dupGetDataCons (x:xs) = case x of
                          TyCon _ con _ cons -> dupGetDataConsCon cons con
                                                ++ dupGetDataCons xs

-- getList of Cons from list of DataCons
dupGetDataConsCon :: [DataCon] -> Con -> [(Con,(Con,Con))]
dupGetDataConsCon [] _ = []
dupGetDataConsCon (x:xs) y = case x of
                               DataCon con _ -> (con,(y,con)):(dupGetDataConsCon xs y)

-- prints duplicates in list of DataCons from TyCons
printDupDataCons :: [[Con]] -> String -> [(Con,Con)] -> String
printDupDataCons [] ys zs = ys
printDupDataCons (x:xs) ys zs | length x <= 1 = printDupDataCons xs ys zs
                              | otherwise = printDupDataCons xs (ys
                                            ++ " data constructor "
                                            ++ head x
                                            ++ " duplicated "
                                            ++ show (length x)
                                            ++ " times in "
                                            ++ getConDupMessage (head x) zs
                                            ++ " -- ") zs

-- gets the error string for duplicated Cons
getConDupMessage _ [] = ""
getConDupMessage x (z:zs) | x == snd z = fst z ++ "," ++ getConDupMessage x zs
                          | otherwise = getConDupMessage x zs
--               --------------End DataCon Check------------

--               --------------Miscellaneous----------------
-- identical to concatMap function, except with another parameter for location
concatMapGen :: (a -> [String] -> String) -> [a] -> [String] -> String
concatMapGen _ [] _ = []
concatMapGen f (x:xs) ys = f x ys ++ concatMapGen f xs ys

-- converts location to a string for an error message
locationToString :: [String] -> String
locationToString [] = ""
locationToString (x:xs) = x ++ "." ++ locationToString xs

-- inverts list and pair
invert :: [(Con,(Con,Con))] -> ([Con],[(Con,Con)])
invert [] = ([],[])
invert (x:xs) = (((fst x):a),((snd x):b))
                where (a,b) = invert xs
                 --------------End Miscellaneous------------

-- -----------------------------------END CHECKS --------------------------------------------------------------------------
