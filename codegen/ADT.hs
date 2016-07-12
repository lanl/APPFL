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
  biIntMCon,
  biLongMCon,
  biFloatMCon,
  biDoubleMCon,
  dataConName,
  tyConName,
  getDataCons,
  makeIntTyCon,
  makeDoubleTyCon,
  dataConTyVars,
  monoTypeVars,
  boxMTypes,
  isBoxed,
  unfoldr
) where

--import AST(Con,BuiltinType(..),Obj)
import AST(Con,Obj)

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
                         |   Long#
                         |   Float#
                         |   Double#
                         |   \Chi# \pi_1 ... \pi_n   Parameterized unboxed data type

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
             
--  c_x \tau_x1 .. \tau_xa_1
data DataCon = DataCon Con [Monotype]
               deriving(Eq,Show)

type TyVar = String

data Polytype = PPoly [TyVar] Monotype
              | PMono Monotype
                deriving(Eq,Ord)
                
data Monotype = MVar TyVar
              | MFun Monotype Monotype
              | MCon Bool Con [Monotype]
              | MPVar TyVar -- should be used only in BU.hs
              | MPhony
                deriving(Eq,Ord)

-- m is rightmost element
unfoldr (MFun m1 m2) = let (m,ms) = unfoldr m2 in (m, m1:ms)
unfoldr m = (m,[])

isBoxed :: Monotype -> Bool
isBoxed m = case m of
  MVar{}     -> True -- polymorphic
  MFun{}     -> True -- expr / obj :: MFun --> PAP created --> boxed
  MCon b _ _ -> b
  MPVar{}    -> True
  m          -> error $ "ADT.isBoxed called with " ++ show m
  

-- set Monotype boxity in TyCons (this should be done before CMaps are built
-- for InfoTabs
boxMTypes :: [TyCon] -> [TyCon]
boxMTypes tycons =
  let -- create assoc list for TyCon names -> TyCons
      tycons' = makeIntTyCon "0" :
                makeLongTyCon "0" :
                makeFloatTyCon "0" :
                makeDoubleTyCon "0" :
                tycons
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
                     _ -> error $ "CMap.cMapTyCons matching bad Monotype: " ++ show m
                     
  in map mapFunc tycons -- don't need built-ins in TyCon list (?)

-- helpers to make TyCons for built-in types
-- this is a bit of a hack to fudge the fact that there are no explicit
-- data declarations for the built-ins
-- The string given (e.g. "0") may be useful, depending on the application
-- of the TyCon
makeIntTyCon :: Con -> TyCon
makeIntTyCon c = TyCon False "Int_h" [] [DataCon c []]

makeLongTyCon :: Con -> TyCon
makeLongTyCon c = TyCon False "Long_h" [] [DataCon c []]

makeFloatTyCon :: Con -> TyCon
makeFloatTyCon c = TyCon False "Float_h" [] [DataCon c []]

makeDoubleTyCon :: Con -> TyCon
makeDoubleTyCon c = TyCon False "Double_h" [] [DataCon c []]

-- this is even more of a hack but at least it's localized
biIntMCon    = MCon False "Int_h" []
biLongMCon   = MCon False "Long_h" []
biFloatMCon  = MCon False "Float_h" []
biDoubleMCon = MCon False "Double_h" []

-- helper field accessor functions --

dataConName :: DataCon -> Con
dataConName (DataCon n _) = n

tyConName :: TyCon -> Con
tyConName (TyCon _ n _ _) = n

getDataCons :: TyCon -> [DataCon]
getDataCons (TyCon _ _ _ cons) = cons

dataConTyVars :: DataCon -> [TyVar]
dataConTyVars (DataCon _ mts) = concatMap monoTypeVars mts

monoTypeVars :: Monotype -> [TyVar]
monoTypeVars mt = go mt []
  where go (MVar v) vs      = v:vs
        go (MFun m1 m2) vs  = let vs' = go m1 vs
                              in go m2 vs'
        go (MCon _ _ ms) vs = concatMap (flip go []) ms ++ vs
        go m _ = error $ "ADT.monoTypeVars: " ++ show m

instance Show Polytype where
    show (PPoly [] m) = show m
    show (PPoly xs m) = "forall " ++ intercalate "," xs ++ "." ++ show m
    show (PMono m) = show m

instance Show Monotype where
    show (MVar v) = v
    show (MPVar v) = "p_" ++ v
    show (MFun m1@(MFun _ _) m2) = "(" ++ show m1 ++ ") -> " ++ show m2
    show (MFun m1 m2) = show m1 ++ " -> " ++ show m2
    show (MCon bxt con ms) = con ++
                             (if bxt
                              then "[B] "
                              else "[U] ") ++ unwords (map show ms)
    show MPhony = "MPhony"

--------------- ADT Pretty Printing -----------------------



instance Unparse Monotype where
  unparse (MVar c) = text c
  unparse (MFun m1@MFun{} m2) = parens (unparse m1) <+> arw <+> unparse m2
  unparse (MFun m1 m2) = unparse m1 <+> arw <+> unparse m2
  unparse (MCon b c ms) = (if null ms then (empty <>) else parens)
                          (text c <+> hsep (map unparse ms))
  unparse m = error $ "ADT.unparse (Monotype) m=" ++ show m

  
instance Unparse DataCon where
  unparse (DataCon con mTypes) = text con <+> hsep (map unparse mTypes)

instance PPrint DataCon where
  pprint = unparse

instance Unparse TyCon where
  unparse (TyCon boxed name vars dCons) =
    let
      (d:ds) = dCons
      lh =
        text "data" <+>
        (if boxed then empty else text "unboxed") <+>
        text name <+> hsep (map text vars) <+> equals

      sepr = bar <> text " "
      ind = length (show lh) + 1
      rh = nest ind
           (unparse d $$
            nest (-2) (vcat $ prepunctuate sepr $ map unparse ds))
    in lh $$ rh
       
instance Unparse [TyCon] where
  unparse tycons = vcat $ postpunctuate semi $ map unparse tycons

instance PPrint TyCon where
  pprint = unparse

instance Unparse a => Unparse (Def a) where
  unparse (DataDef t) =  unparse t
  unparse (ObjDef o) = unparse o

instance Unparse a => Unparse [Def a] where
  unparse defs = vcat $ postpunctuate semi $ map unparse defs

instance PPrint Monotype where
  pprint m = case m of
    MVar v -> text "MVar" <> braces (text v)
    MFun m1 m2 -> text "MFun" <> braces
                  (nest 2
                   (pprint m1 $+$
                    pprint m2))
    MCon b c ms -> text "MCon" <> braces
                   (text (if b then "boxed" else "unboxed") <+>
                    text c $+$
                    nest 2 (vcat $ map pprint ms))
    MPVar v -> text "MPVar" <> braces (text v)
    MPhony -> text "MPhony"
    
