{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}

module ADT (
  Def(..),
  TyCon(..),
  DataCon(..),
  TyVar,
  Polytype(..),
  Monotype(..),
  Con
) where

import AST

import Data.List(intercalate, (\\), find)
import Data.Maybe (fromJust)
import Data.Char (isNumber)
import qualified Data.Map as Map
import PPrint
--import Control.Monad.State

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
              | MPrim BuiltinType
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
    show (MPrim p) = show p

--------------- ADT Pretty Printing -----------------------

instance PPrint Monotype where
  toDoc (MVar c) = text c
  toDoc (MFun m1@MFun{} m2) = parens (toDoc m1) <+> arw <+> toDoc m2
  toDoc (MFun m1 m2) = toDoc m1 <+> arw <+> toDoc m2
  toDoc (MCon c ms) = (if null ms then (empty <>) else parens)
                      (text c <+> hsep (map toDoc ms))
  toDoc (MPrim p) = case p of
    UBInt    -> text "Int#"
    UBDouble -> text "Double#"
    UBBool   -> text "Bool#"

  
instance PPrint DataCon where
  toDoc (DataCon con mTypes) = text con <+> hsep (map toDoc mTypes)

instance PPrint TyCon where
  toDoc (TyCon boxed name vars dCons) =
    let
      barify (x:xs) = x : (foldr ((:) . (bar<+>)) [] xs)

      lh = 
        (if boxed then empty else text "unboxed") <+>
        text name <+> hsep (map text vars) <+> equals

      -- nest 2 has the somewhat strange effect of "dedenting" by 2 here
      -- not sure why
      rh = vcat $ barify $ map ((nest 2).toDoc) dCons

    in lh <+> rh

instance PPrint a => PPrint (Def a) where
  toDoc (DataDef t) = text "data" <+> toDoc t
  toDoc (ObjDef o) = toDoc o

instance PPrint a => PPrint [Def a] where
  toDoc xs = vcat $ punctuate semi $ map toDoc xs

