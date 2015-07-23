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
  Con,
  dataConName,
  tyConName,
  getDataCons,
  makeIntTyCon,
  makeDoubleTyCon,
  boxMTypes,
  isBoxed
) where

import AST(Con,BuiltinType(..),Obj)

import Data.List(intercalate, (\\), find)
import Data.Maybe (fromJust)
import Data.Char (isNumber)
import qualified Data.Map as Map
import PPrint

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
             deriving(Eq,Show)
             
-- Boxed True: c_x \tau_x1 .. \tau_xa_1 
-- Boxed False: c_x# \tau_x1 .. \tau_xa_1  
data DataCon = DataCon Con [Monotype]
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

isBoxed :: Monotype -> Bool
isBoxed m = case m of
  MVar{}     -> True -- polymorphic
  MFun{}     -> True -- expr / obj :: MFun --> PAP created --> boxed
  MCon b _ _ -> b
  MPrim{}    -> False
  MPVar{}    -> True
  m          -> error $ "ADT.isBoxed called with " ++ show m
  

-- set Monotype boxity in TyCons (this should be done before CMaps are built
-- for InfoTabs
boxMTypes :: [TyCon] -> [TyCon]
boxMTypes tycons =
  let -- create assoc list for TyCon names -> TyCons
      tycons' = makeIntTyCon "0" : tycons
      tmap = zip (map tyConName tycons') tycons'
      
      -- functions below set MCon boxity in TyCons
      mapFunc (TyCon b c vs dcs) = TyCon b c vs $ map setDCtypes dcs
      setDCtypes (DataCon c mts) = DataCon c $ map setMtypes mts
      setMtypes m = case m of
                     MCon _ c mts ->
                       let (TyCon bxt _ _ _) = fromJust $ lookup c tmap
                       in MCon bxt c $ map setMtypes mts
                     MFun mts1 mts2 -> MFun (setMtypes mts1) (setMtypes mts2)
                     MVar{} -> m
                     MPrim{} -> m
                     _ -> error $ "CMap.cMapTyCons matching bad Monotype: " ++ show m
                     
  in map mapFunc tycons -- don't need built-ins in TyCon list (?) 

-- helpers to make TyCons for built-in types
-- this is a bit of a hack to fudge the fact that there are no explicit
-- data declarations for the built-ins
-- The string given (e.g. "0") may be useful, depending on the application
-- of the TyCon
makeIntTyCon :: Con -> TyCon
makeIntTyCon c = TyCon False "Int_h" [] [DataCon c []]

makeDoubleTyCon :: Con -> TyCon
makeDoubleTyCon c = TyCon False "Double_h" [] [DataCon c []]


-- helper field accessor functions --

dataConName :: DataCon -> Con
dataConName (DataCon n _) = n

tyConName :: TyCon -> Con
tyConName (TyCon _ n _ _) = n

getDataCons :: TyCon -> [DataCon]
getDataCons (TyCon _ _ _ cons) = cons



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
    show (MCon bxt con ms) = con ++
                             (if bxt
                              then "[B] "
                              else "[U] ") ++ intercalate " " (map show ms)
    show (MPrim p) = show p
    show MPhony = "MPhony"

--------------- ADT Pretty Printing -----------------------

instance PPrint Monotype where
  toDoc (MVar c) = text c
  toDoc (MFun m1@MFun{} m2) = parens (toDoc m1) <+> arw <+> toDoc m2
  toDoc (MFun m1 m2) = toDoc m1 <+> arw <+> toDoc m2
  toDoc (MCon _ c ms) = (if null ms then (empty <>) else parens)
                        (text c <+> hsep (map toDoc ms))
  toDoc (MPrim p) = case p of
    UBInt    -> text "Int#"
    UBDouble -> text "Double#"
  toDoc m = error $ "ADT.toDoc (Monotype) m=" ++ show m

  
instance PPrint DataCon where
  toDoc (DataCon con mTypes) = text con <+> hsep (map toDoc mTypes)

instance PPrint TyCon where
  toDoc (TyCon boxed name vars dCons) =
    let
      lh = 
        (if boxed then empty else text "unboxed") <+>
        text name <+> hsep (map text vars) <+> equals

      -- nest 2 has the somewhat strange effect of "dedenting" by 2 here
      -- not sure why
      rh = vcat $ prepunctuate bar $ map ((nest 2).toDoc) dCons

    in lh <+> rh

instance PPrint a => PPrint (Def a) where
  toDoc (DataDef t) = text "data" <+> toDoc t
  toDoc (ObjDef o) = toDoc o

instance PPrint a => PPrint [Def a] where
  toDoc xs = vcat $ punctuate semi $ map toDoc xs

