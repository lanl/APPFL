{-# LANGUAGE RecordWildCards #-}
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
  ppConMaps,
  getTyConDefFromConstructor,
  isBoxedMonotype
) where

import AST(Con,BuiltinType,Obj)

import Data.List(intercalate)
--import Data.Maybe
--import Control.Monad.State
import qualified Data.Map as Map
import Text.PrettyPrint

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
            deriving(Eq,Show)

-- Boxed: data \Chi \alpha_1 .. \alpha_t =
-- c_1 \tau_11 .. \tau_1a_1 | ... | c_n \tau_n1 .. \tau_na_1  
-- Unboxed: data unboxed \Chi# \alpha_1 .. \alpha_t =
-- c_1# \tau_11 .. \tau_1a_1 | ... | c_n# \tau_n1 .. \tau_na_1   
data TyCon = TyCon Bool Con [TyVar] [DataCon]
             deriving(Eq,Show)
             
-- Boxed True: c_x \tau_x1 .. \tau_xa_1 
-- Boxed False: c_x# \tau_x1 .. \tau_xa_1  
data DataCon = DataCon Bool Con [Monotype]
               deriving(Eq,Show)

type TyVar = String

data Polytype = PPoly [TyVar] Monotype
              | PMono Monotype
                deriving(Eq,Ord)
                
data Monotype = MVar TyVar
              | MFun Monotype Monotype
              | MCon Bool Con [Monotype]
              | MPrim BuiltinType
              | MPVar TyVar -- should be used only in BU.hs
              | MPhony
                deriving(Eq,Ord)


precalate s ss = concatMap (s++) ss

instance Show Polytype where
    show (PPoly [] m) = show m
    show (PPoly xs m) = "forall " ++ (intercalate "," xs) ++ "." ++ show m
    show (PMono m) = show m

instance Show Monotype where
    show (MVar v) = v
    show (MPVar v) = "p_" ++ v
    show (MFun m1@(MFun _ _) m2) = "(" ++ show m1 ++ ") -> " ++ show m2
    show (MFun m1 m2) = show m1 ++ " -> " ++ show m2
    show (MCon boxed con ms) = con ++ 
                               (if boxed then " [B] " else " [U] ") ++
                               "(" ++ intercalate ") (" (map show ms) ++ ")"
    show (MPrim p) = show p
    show MPhony = "phonyType"

isBoxedMonotype MVar{} = True
isBoxedMonotype MFun{} = True
isBoxedMonotype (MCon boxed _ _) = boxed

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
        tyConParam = case Map.lookup tyConName tconMap of
                       Just x -> x
                       Nothing -> error $ "getTyConDefFromConstructor: no such " 
                                          ++ tyConName
        -- Just tyConParam = Map.lookup tyConName tconMap :: Maybe TyConParam
    in tycon tyConParam :: TyCon

         
 
 

ppConMaps (tmap, dmap) = show $
                         text "TyConMap:" $+$
                         nest 2 (brackets $ ppTmap tmap) $+$
                         text "DataConMap" $+$
                         ppDmap dmap

ppTmap tmap =
  let assoc = Map.toList tmap
      ppEntry (s, tcParam) = case tcParam of
        TyConParam{..} ->
          text (s ++ "-->") $+$
          (nest 2 $ braces $
           text "tarity:" <+> int tarity $+$
           text "ttag:" <+> int ttag $+$
           text "tboxed:" <+> text (show tboxed) $+$
           text "tdatacons:" <+> brackets (hsep $ punctuate comma $ map text tdatacons) $+$
           text "tycon:" <+> text (show tycon))
  in vcat $ map ppEntry assoc

ppDmap dmap =
  let assoc = Map.toList dmap
      ppEntry (s, dcParam) = case dcParam of
        DataConParam{..} ->
          text (s ++ "-->") $+$
          (nest 2 $ braces $
           text "darity:" <+> int darity $+$
           text "dtag:" <+> int dtag $+$
           text "dboxed:" <+> text (show dboxed) $+$
           text "dtycon:" <+> text (show dtycon) $+$
           text "datacon:" <+> text (show datacon))
  in vcat $ map ppEntry assoc  
