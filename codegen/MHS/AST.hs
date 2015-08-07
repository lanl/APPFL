{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE TypeSynonymInstances     #-}

module MHS.AST
(
  Pattern (..),
  Defn (..),
  Constr (..),
  Exp (..),
  Atm (..),
  Clause,
  isODefn,
  isTDefn,
  isDDefn,
  partitionDefs,
  unfoldEAp,
  isDefaultPat,
  hidesPats,
  hidesPat,
  isEFn,
  isEAt,
  intCon,
  dblCon,
  isBoxedNum,
  module AST,
  module ADT
)
where

import AST
import ADT
import Data.List (nubBy, partition)
import Data.Char (isNumber)
import Data.Function (on)
import PPrint
import Util (mapSnd)


data Defn = ODefn -- object definition 
            {bnd :: Var,  -- variable bound to
             oexp :: Exp, -- expression being bound
             mmtype :: Maybe Monotype} -- Type, if signature present
            
          | DDefn -- datatype definition
            {mtyp :: Monotype,  -- MCon holding Type info (name, boxed, pvars)
             dcons :: [Constr]} -- List of constructors
            
          | TDefn -- type signature definition
            {bnd :: Var,  -- variable to give type information for
             mtyp :: Monotype} -- Type specified
          deriving (Show,Eq)

partitionDefs defs =
  let (ds, rs) = partition isDDefn defs
      (os, ts) = partition isODefn rs
  in (ds, os, ts)

isODefn d = case d of
  ODefn{} -> True
  _       -> False

isDDefn d = case d of
  DDefn{} -> True
  _       -> False

isTDefn d = case d of
  TDefn{} -> True
  _       -> False  


data Constr = DCon {dcon :: Con, mtyps :: [Monotype], cons :: [Con]}
            deriving (Show,Eq)

data Exp = EAp {fexp :: Exp, eexp :: Exp}
         | EFn {pats :: [Pattern], eexp :: Exp}
         | ECs {eexp :: Exp, cls :: [Clause]}
         | ELt {eexp :: Exp, defns :: [Defn]}
         | EAt {atm :: Atm}
           deriving (Show,Eq)

isEFn EFn{} = True
isEFn _ = False
isEAt EAt{} = True
isEAt _ = False

type Clause = (Pattern, Exp)

data Atm = LBInt Int
         | LBDbl Double
         | LUBInt Int
         | LUBDbl Double
         | AtmVar Var
         | AtmCon Con
         | AtmOp Primop
           deriving (Show, Eq)

data Pattern = Match {str :: String, npats :: [Pattern]}
             | Default {str :: String}
             deriving (Show,Eq)

isDefaultPat p = case p of
  Default{} -> True
  _ -> False

isMatchPat = not . isDefaultPat

hidesPats :: [Pattern] -> [Pattern] -> Bool
hidesPats ps qs = and $ zipWith hidesPat ps qs

hidesPat Default{} _ = True
hidesPat _ Default{} = False
hidesPat (Match a as) (Match b bs) = a == b && hidesPats as bs


-- unfold nestings of EAp into a tuple representing the "base" function
-- (which can be any of {EAt, ECs, ELt, EFn}) and the list of Exp that
-- the function is being applied to
unfoldEAp :: Exp -> (Exp, [Exp])
unfoldEAp e =
  let (f, exps) = uf e
  in (f, reverse exps) 
  where
    uf e = case e of
            EAp{fexp, eexp} ->
              let (f, exps) = uf fexp
              in (f, eexp:exps)
            _ -> (e,[]) -- NOT necessarily EAt!



-- These datatypes conform to the usage in GHC.Prim and GHC.Exts
-- this allows a minihaskell program to be compiled as Haskell
-- with creative usage of haskell blocks, the MagicHash extension
-- and the correct imports and aliases see prelude.mhs
intCon = DDefn { mtyp = MCon True "Int" [],
               dcons = [DCon {dcon = "I#",
                              mtyps = [MPrim UBInt],
                              cons = ["I#"]}] }
dblCon = DDefn { mtyp = MCon True "Double" [],
               dcons = [DCon {dcon = "D#",
                              mtyps = [MPrim UBDouble],
                              cons = ["D#"]}] }

isBoxedNum s = 
  let (as, bs) = break (== '.') $ init s
  in all isNumber (as ++ drop 1 bs) && last s == '#'


instance Unparse [Defn] where
  unparse = vcat . map unparse

instance Unparse (Defn) where
  unparse TDefn{bnd, mtyp} =
    text bnd <+> doubleColon <+> unparse mtyp

  unparse DDefn{mtyp, dcons} =
    case mtyp of
        MCon b c ms ->
          let lh = text "data" <+>
                   (if b then empty else text "unboxed") <+>
                   text c <+>
                   hsep (map unparse ms) <+> -- should only be MVar
                   equals
              (d:ds) = dcons -- should never have fewer than 1 constructor
              sepr = bar <> text " "
              ind = (length $ show lh) + 1
              rh = nest ind
                   (unparse d $$
                    nest (-2) (vcat $ prepunctuate sepr $ map unparse ds))
          in lh $$ rh
                    
        m -> error $ "DDefn holds non-MCon defining monotype: " ++ show m

  unparse ODefn{bnd, oexp} =
    text bnd <+>
    case oexp of
     EFn{pats, eexp} -> hsep (map unparse pats) <+>
                        equals <+>
                        unparse eexp
     _               -> equals <+> unparse oexp



instance Unparse (Constr) where
  unparse DCon{dcon, mtyps} =
    text dcon <+> hsep (map unparse mtyps)

instance Unparse (Exp) where
  unparse EAp{fexp, eexp} =
    let fd = case eexp of
              EAt{atm} -> unparse atm
              _ -> parens (unparse eexp)
    in unparse fexp <+> fd

  unparse EFn{pats, eexp} =
    parens $
    lambda <+> hsep (map unparse pats) <+>
    arw <+> unparse eexp

  unparse ECs{eexp, cls} =
    text "case" <+> unparse eexp <+> text "of" $+$
    nest 2 (vcat $ map unparse cls)

  unparse ELt{eexp, defns} =
    text "let" $+$
    nest 2 (vcat $ map unparse defns) $+$
    text "in" <+> unparse eexp

  unparse EAt{atm} = unparse atm

instance Unparse (Clause) where
  unparse (p,e) = unparse p <+> arw <+> unparse e

instance Unparse Pattern where
  unparse Match{str, npats} =
    case npats of
     [] -> text str
     _  -> parens (text str <+> hsep (map unparse npats))

  unparse Default{str} = text str

instance Unparse Atm where
  unparse atm = case atm of
    AtmVar v -> text v
    AtmCon c -> text c
    AtmOp o  -> unparse o
    LBInt i  -> int i
    LBDbl d  -> double d
    LUBInt i -> int i <> hash
    LUBDbl d -> double d <> hash


pprintObj :: String -> [(String, Doc)] -> Doc
pprintObj name tups =
  text name <> braces
  (nest 2
   (vcat $ punctuate comma $ map mproc tups))
  where
    mproc (n,d) = text n <> colon <+> d

instance PPrint Defn where
  pprint d = case d of
    TDefn{bnd, mtyp} ->
      pprintObj "TDefn"
      [("bnd", text bnd),
       ("mtyp", pprint mtyp)]

    ODefn{bnd, oexp, mmtype} ->
      let mdoc = case mmtype of
                  Nothing -> text "Nothing"
                  Just m  -> pprint m
      in
       pprintObj "ODefn"
       [("bnd", text bnd),
        ("oexp", pprint oexp),
        ("mmtyp", mdoc)]

    DDefn{mtyp, dcons} ->
      let mdoc = case mtyp of
                  MCon b c ms -> (if b then empty else text "unboxed") <+>
                                 text c <+> hsep (map mfun ms)
                  _ -> error $
                       "NewAST.pprint (Defn): DDefn holding bad Monotype: " ++
                       show (pprint mtyp)
          mfun m = case m of
                    MVar v -> text v
                    _ -> error $
                         "NewAST.pprint (Defn): DDefn MTyp holding non MVars " ++
                         show (pprint mtyp)
      in
       pprintObj "DDefn"
       [("mtyp", mdoc),
        ("dcons", pprint dcons)]

instance PPrint Exp where
  pprint e = case e of
    EAp{fexp, eexp} ->
      pprintObj "EAp"
      [("fexp", pprint fexp),
       ("eexp", pprint eexp)]
      
    EFn{pats, eexp} ->
      pprintObj "EFn"
      [("pats", pprint pats),
       ("eexp", pprint eexp)]
      
    ECs{eexp, cls} ->
      pprintObj "ECs"
      [("eexp", pprint eexp),
       ("cls", pprint cls)]
      
    ELt{eexp, defns} ->
      pprintObj "ELt"
      [("defns", pprint defns),
       ("eexp", pprint eexp)]
      
    EAt{atm} ->
      pprintObj "EAt"
      [("atm", pprint atm)]


instance PPrint Constr where
  pprint DCon{dcon, mtyps, cons} =
    pprintObj "DCon"
    [("dcon", text dcon),
     ("mtypes", pprint mtyps),
     ("cons", pprint cons)]
    where mcName (MCon _ c _) = c
          mcName _ = error "not an MCon"



instance PPrint Pattern where
  pprint p = case p of
    Match{str, npats} ->
      pprintObj "Match"
      [("str", text str),
       ("npats", pprint npats)]

    Default{str} ->
      pprintObj "Default" [("str", text str)]


instance PPrint Clause where
  pprint (pat, exp) =
    parens (pprint pat <> arw <+> pprint exp)

instance PPrint Atm where
  pprint atm = case atm of
    AtmVar v -> f "AtmVar" $ text v
    AtmCon c -> f "AtmCon" $ text c
    AtmOp o  -> f "AtmOp" $ pprint o
    LBInt i  -> f "LBInt" $ int i
    LBDbl d  -> f "LBDbl" $ double d
    LUBInt i -> f "LUBInt" $ int i
    LUBDbl d -> f "LUBDbl" $ double d
    where f n d = text n <> braces d
          
