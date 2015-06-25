{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}

module ADT (
  Def(..),
  TyCon(..),
  DataCon(..),
  TyVar,
  Polytype(..),
  Monotype(..),
  TyConParam(..),
  TyConMap,
  DataConParam(..),
  DataConMap,
  ConMaps,
  getTyConDefFromConstructor
) where

import AST

import Data.List(intercalate, (\\), find)
import Data.Maybe (fromJust)
--import Control.Monad.State
import qualified Data.Map as Map

{-
  Algebraic Datatypes:
    
  Ref:  Unboxed Values as First-Class Citizens
  
  data Def (pg 6):
  data \Chi \alpha_1 .. \alpha_t =
  c_1 \tau_11 .. \tau_1a_1 | ... | c_n \tau_n1 .. \tau_na_1
  
  \Chi -- type constructor
  \c_x -- data constructor
  
  and unboxed version on pg 26
  data unboxed \Chi# \alpha_1 .. \alpha_t =
  c_1# \tau_11 .. \tau_1a_1 | ... | c_n# \tau_n1 .. \tau_na_1
  
  Polytype      \sigma ::=  \forall \alpha . \sigma 
                        |   \tau

  Monotype      \tau   ::=  \pi | \nu

  Boxed type    \pi    ::=  \alpha                   Type variable
                        |   \tau -> \tau             Function type
                        |   \Chi \pi_1 ... \pi_n   Parameterized boxed data type

  Unboxed type  \nu     ::=  Int#
                        |   Double#
                        |   Bool#
                        |   \Chi# \pi_1 ... \pi_n   Parameterized boxed data type

-}


data Def a = ObjDef (Obj a)
           | DataDef TyCon
             deriving(Eq, Show)

-- Boxed: data \Chi \alpha_1 .. \alpha_t =
-- c_1 \tau_11 .. \tau_1a_1 | ... | c_n \tau_n1 .. \tau_na_1  
-- Unboxed: data unboxed \Chi# \alpha_1 .. \alpha_t =
-- c_1# \tau_11 .. \tau_1a_1 | ... | c_n# \tau_n1 .. \tau_na_1   
data TyCon = TyCon Bool Con [TyVar] [DataCon]
             deriving(Eq, Show)

                     
-- Boxed True: c_x \tau_x1 .. \tau_xa_1 
-- Boxed False: c_x# \tau_x1 .. \tau_xa_1  
data DataCon = DataCon Con [Monotype] -- Removed Bool field
               deriving(Eq, Show)


type TyVar = String

data Polytype = PPoly [TyVar] Monotype
              | PMono Monotype
                deriving(Eq,Ord)
                
data Monotype = MVar TyVar
              | MFun Monotype Monotype
              | MCon Con [Monotype] -- Removed Bool field
                deriving(Eq,Ord)

instance Show Polytype where
    show (PPoly [] m) = show m
    show (PPoly xs m) = "forall " ++ (intercalate "," xs) ++ "." ++ show m
    show (PMono m) = show m

instance Show Monotype where
    show (MVar v) = v
    show (MFun m1@(MFun _ _) m2) = "(" ++ show m1 ++ ") -> " ++ show m2      
    show (MFun m1 m2) = show m1 ++ " -> " ++ show m2 
    show (MCon con ms) = con ++ " " ++ intercalate " " (map show ms) -- modified (no boxed)



data TyConParam = TyConParam {tarity :: Int, 
                              ttag :: Int, 
                              tboxed :: Bool,
                              tdatacons :: [Con],
                              tycon :: TyCon}
                  deriving(Eq,Show)

type TyConMap = Map.Map String TyConParam

data DataConParam = DataConParam {darity :: Int, 
                                  dtag :: Int, 
                                  dboxed :: Bool, 
                                  dtycon :: Con, -- type constructor name
                                  datacon :: DataCon} 
                    deriving(Eq,Show)

type DataConMap = Map.Map String DataConParam

type ConMaps = (TyConMap, DataConMap)

-- given a TyConMap, DataConMap and a data constructor C,
-- return the data constructor definition

getTyConDefFromConstructor dconMap tconMap con = 
    let Just dataConParam = Map.lookup con dconMap :: Maybe DataConParam
        tyConName = dtycon dataConParam :: Con
        Just tyConParam = Map.lookup tyConName tconMap :: Maybe TyConParam
    in tycon tyConParam :: TyCon



----------------------------  Alternative interface to ConMap idea ---------------------------

type CMap = Map.Map Con TyCon

toCMap tycons =
  let tab = concatMap (\t-> zip (map dConName $ tycDCons t) (repeat t)) tycons
  in Map.fromList tab
      
      
conArity name conmap =
  let cons = luDCons name conmap
      (DataCon _ mtypes) = getDConInList name cons
  in length mtypes

getDConInList name cons = fromJust $ find ((==name).dConName) cons

dConName (DataCon n _) = n


-- Given a list of Cons, check if they exhaust all the DataCon constructors
-- for their associated TyCon.
-- The head of the list is used to lookup the TyCon, but otherwise, validity
-- of constructors is *not* checked. (yet)
-- i.e. if given ["A","B","C"] as Cons and a TyCon has been made from
-- data T = A | B,
-- consExhaust will return True
consExhaust :: [Con] -> CMap -> Bool
consExhaust [] _ = False
consExhaust cc@(c:cs) conmap =
  let cons = luDCons c conmap
  in  null $ map dConName cons \\ cc

-- Given a Con, get the list of DataCons associated with it
luDCons :: Con -> CMap -> [DataCon]
luDCons con conmap = tycDCons $ luTCon con conmap


-- Pull DataCons out of a TyCon
tycDCons :: TyCon -> [DataCons]
tycDCons (TyCon _ _ _ cons) = cons


-- Lookup a DataCon in the CMap by Con
luDCon :: Con -> CMap -> DataCon
luDCon name conmap = getDConInList name $ luDCons name conmap

-- lookup TyCon info by con in the CMap
-- info is a triple of the form
-- (TyCon name, TyCon vars, MonoTypes of the DataCon name given)
luTConInfo :: Con -> CMap -> (Con,[TyVar],[MonoType])
luTConInfo name conmap =
   let (TyCon _ tname vars cons) = luTCon name conmap
       (DataCon _ mTypes) = getDConInList name cons
   in (tname, vars, mTypes)

-- lookup a TyCon by Con in the CMap
luTCon :: Con -> CMap -> TyCon
luTCon name conmap
  | isBuiltInType name = getBuiltInType name
  | otherwise = case Map.lookup name conmap of
                 Nothing -> error "constructor not in conmap"
                (Just t) -> t


-- Pending
isBuildInType :: Con -> Bool
isBuiltInType = const False
getBuiltInType :: Con -> TyCon
getBuildInType = undefined -- TyCon False "Int#" [] [DataCon "Int# []]





         
 
 
