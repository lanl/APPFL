{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}

module ADT (
  TyCon(..),
  DataCon(..),
  TyVar,
  Polytype(..),
  Monotype(..),
  PrimType(..),
  Con,
  primTypeName,
  primTypeNames,
  dataConName,
  tyConName,
  getDataCons,
  makePrimTyCon,
  primTypeStrId,
  dataConTyVars,
  monoTypeVars,
  boxMTypes,
  isBoxed,
  unfoldMTy,
  Assumption,
  Assumptions,
) where


import Data.List(intercalate)
import Data.Maybe (fromMaybe)
import PPrint
import Util
import qualified Data.Set as Set
-- import AST(Var) -- shouldn't depend on AST
type Var = String

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
                        |   \Chi \pi_1 ... \pi_n     Parameterized boxed type constructor

  Unboxed type  \nu     ::=  Int#
                         |   Double#
                         |   \Chi# \pi_1 ... \pi_n   Parameterized unboxed type constructor

-}



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
type Con = String

data Polytype = PPoly [TyVar] Monotype
              | PMono Monotype
                deriving(Eq,Ord)

data Monotype = MVar TyVar
              | MFun Monotype Monotype
              | MCon (Maybe Bool) Con [Monotype]
              | MPVar TyVar -- should be used only in BU.hs
              | MPrim PrimType
              | MPhony
                deriving(Eq,Ord)

data PrimType
  = PInt
  | PDouble
  | PString
  | PVoid   -- side-effecting code
  deriving (Eq, Ord, Enum, Bounded, Show)

type Assumption = (Var, Monotype)

-- Set, not Map, because a var may have multiple assumptions
-- alternatively, Map.Map Var (Set.Set Monotype)
type Assumptions = Set.Set Assumption


primTypeStrId :: PrimType -> String
primTypeStrId PInt    = "i"
primTypeStrId PDouble = "d"
primTypeStrId PString = "s"
primTypeStrId PVoid   = ""


primTypeName :: PrimType -> Con
primTypeName = (++ "#") . tail . show

primTypeNames :: [Con]
primTypeNames = map primTypeName [minBound :: PrimType .. ]

-- m is rightmost element
unfoldMTy (MFun m1 m2) = let (m,ms) = unfoldMTy m2 in (m, m1:ms)
unfoldMTy (MPrim PVoid) = (MVar "",[]) -- hack for raise
unfoldMTy m = (m,[])

isBoxed :: Monotype -> Bool
isBoxed m = case m of
  MVar{}     -> True -- polymorphic
  MFun{}     -> True -- expr / obj :: MFun --> PAP created --> boxed
  MCon (Just b) _ _ -> b
  MCon Nothing _ _ -> error "Boxity not set"
  MPVar{}    -> True
  MPrim{} -> False
  m          -> error $ "ADT.isBoxed called with " ++ show m

isVoid :: Monotype -> Bool
isVoid m = case m of 
  MPrim PVoid -> True
  m -> False

-- set Monotype boxity in TyCons (this should be done before CMaps are built
-- for InfoTabs)
boxMTypes :: [TyCon] -> [TyCon]
boxMTypes tycons =
  let -- create assoc list for TyCon names -> TyCons
      tycons' = map makePrimTyCon [minBound ..] ++
                tycons
      tmap = zip (map tyConName tycons') tycons'

      -- functions below set MCon boxity in TyCons
      mapFunc (TyCon b c vs dcs) = TyCon b c vs $ map setDCtypes dcs
      setDCtypes (DataCon c mts) = DataCon c $ map setMtypes mts
      setMtypes m = case m of
                     MCon _ c mts ->
                       let (TyCon bxt _ _ _) =
                             fromMaybe
                             (error $ "Couldn't find " ++ show c ++ " in tmap " ++ show tmap)
                             $ lookup c tmap
                       in MCon (Just bxt) c $ map setMtypes mts
                     MFun mts1 mts2 -> MFun (setMtypes mts1) (setMtypes mts2)
                     MVar{} -> m
                     MPrim{} -> m
                     _ -> error $ "CMap.cMapTyCons matching bad Monotype: " ++ show m

  in map mapFunc tycons -- don't need built-ins in TyCon list (?)

-- helpers to make TyCons for built-in types
-- this is a bit of a hack to fudge the fact that there are no explicit
-- data declarations for the built-ins
makePrimTyCon :: PrimType -> TyCon
makePrimTyCon pt = TyCon False (primTypeName pt) [] []


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
    show (MPrim pt) = show pt
    show (MCon (Just bxt) con ms) = con ++
                             (if bxt
                              then "[B] "
                              else "[U] ") ++ unwords (map show ms)
    show (MCon Nothing con ms) = con ++ "[?] "
                                     ++ unwords (map show ms)
    show MPhony = "MPhony"

--------------- ADT Pretty Printing -----------------------




instance Unparse Monotype where
  unparse (MVar c) = stgName c
  unparse (MFun m1@MFun{} m2) = parens (unparse m1) <+> arw <+> unparse m2
  unparse (MFun m1 m2) = unparse m1 <+> arw <+> unparse m2
  unparse (MCon b c ms) = (if null ms then (empty <>) else parens)
                            (stgName c <+> hsep (map unparse ms))
  unparse (MPrim ty) = pprint ty
  unparse m = error $ "ADT.unparse (Monotype) m=" ++ show m

unparseDCmono :: Monotype -> Doc
unparseDCmono m@(MFun m1 m2) = parens $ unparse m
unparseDCmono m = unparse m

instance Unparse DataCon where
  unparse (DataCon con mTypes) = stgName con <+> hsep (map unparseDCmono mTypes)

instance PPrint DataCon where
  pprint = unparse

instance Unparse TyCon where
  unparse (TyCon boxed name vars dCons) =
    let
      (d:ds) = dCons
      lh =
        text "data" <+>
        (if boxed then empty else text "unboxed") <+>
        stgName name <+> hsep (map stgName vars) <+> equals

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


instance PPrint Monotype where
  pprint m = case m of
    MVar v -> text "MVar" <> braces (text v)
    MFun m1 m2 -> text "MFun" <> braces
                  (nest 2
                   (pprint m1 $+$
                    pprint m2))
    MCon (Just b) c ms -> text "MCon" <> braces
                   (text (if b then "boxed" else "unboxed") <+>
                    text c $+$
                    nest 2 (vcat $ map pprint ms))
    MCon Nothing c ms -> text "MCon" <> braces
                   (text "?" <+>
                    text c $+$
                    nest 2 (vcat $ map pprint ms))
    MPVar v -> text "MPVar" <> braces (text v)
    MPrim pt -> text "MPrim" <> pprint pt
    MPhony -> text "MPhony"
