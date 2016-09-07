
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecordWildCards   #-}

module AST (
  Var,
  Atom(..),
  Expr(..),
  Alt(..),
  Alts(..),
  Obj(..),
  Def(..),
  PrimType(..),
  PrimOpInfo(..),
  PrimOp(..),
  opArity,
  mkOpInfo,
  primOpTab,
  primTyTab,
  show,
  objListDoc,
  projectAtoms,
  scrtVarName,
  maybeTypeOfAtom,
  module ADT
) where

import PPrint
import Data.List (find, (\\))
import Data.Int as Int (Int64)
import ADT
import Util ((>|))

import qualified Language.C.Syntax as C
       ( Exp (BinOp, UnOp, Var, FnCall)
       , BinOp (..), UnOp(..)
       , Id (Id)
       )
--  See Parser.hs for grammar

type Var = String

data Atom = Var  Var
          | LitI Int64
          | LitD Double
          | LitC Con
          | LitStr String -- primitive string e.g. const char *str = "hello world";
            deriving(Eq)

instance Show Atom where
    show (Var v)    = v
    show (LitI i)   = show i
    show (LitD d)   = show d ++ "(d)"
    show (LitC c)   = c
    show (LitStr s) = s

data Def a = ObjDef (Obj a)
           | DataDef TyCon
             deriving(Eq, Show)


data Obj a
  = FUN
    { omd   :: a
    , vs    :: [Var]
    , e     :: Expr a
    , oname :: String}

  | PAP
    { omd   :: a
    , f     :: Var
    , as    :: [Expr a] -- invariant the as are EAtoms
    , oname :: String}

  | CON
    { omd   :: a
    , c     :: Con
    , as    :: [Expr a] -- invariant the as are EAtoms
    , oname :: String}

  | THUNK
    { omd   :: a
    , e     :: Expr a
    , oname :: String}

--BH | BLACKHOLE {omd :: a                           , oname :: String}
  deriving(Eq,Show)

data Expr a
  = EAtom
    { emd     :: a
    , ea      :: Atom}
  | EFCall
    { emd     :: a
    , ev      :: Var
    , eas     :: [Expr a]} -- invariant the eas are EAtoms
  | EPrimOp
    { emd     :: a
    , eprimOp :: PrimOp
    , eopInfo :: PrimOpInfo -- We know everything about an op at parse time, so we save it here
    , eas     :: [Expr a]} -- invariant the eas are EAtoms
  | ELet
    { emd     :: a
    , edefs   :: [Obj a]
    , ee      :: Expr a}
  | ECase
    { emd     :: a
    , ee      :: Expr a
    , ealts   :: Alts a}
  deriving(Eq,Show)

data Alts a = Alts { altsmd :: a
                   , alts   :: [Alt a]
                   , aname  :: String
                   , scrt   :: Expr a} -- invariant the scrt is an EAtom holding a Var
            deriving(Eq,Show)

-- Grab the variable name of the scrutinee binding, enforce the invariant above
scrtVarName (EAtom _ (Var v)) = v
scrtVarName _ = error "scrutinee is not an atomic variable"

data Alt a = ACon {amd :: a, ac :: Con, avs :: [Var], ae :: Expr a}
           | ADef {amd :: a,            av :: Var,    ae :: Expr a}
             deriving(Eq,Show)

projectAtoms [] = []
projectAtoms (EAtom{ea}:as) = ea:projectAtoms as
projectAtoms (a:as) = error "InfoTab.projectAtoms: non-EAtom"


primTys :: [PrimType]
primTys = [(minBound :: PrimType) ..]

primTyTab :: [(String, PrimType)]
primTyTab = zip (map ((++ "#") . tail . show)  primTys) primTys


-- when calculating free variables we need an enclosing environment that
-- includes the known primops.  This will allow proper scoping.  As such
-- we initially parse EPrimops as EFCalls, then transform and check saturation
-- here.  We only need to distinguish EPrimop from EFCall because special
-- code is generated for them.

-- Note: Order matters here, since the classifications of Ops takes advantage of the
-- generated Enum instance (polyOps, constrainedOps).  If you add new PrimOps, you'll
-- need to take this into account.
data PrimOp
  =
  -- :: (Num# a#) => a# -> a#
    Pneg

  -- :: (Num# a#) => a# -> a# -> a#
  | Padd | Psub | Pmul | Pdiv | Pmod | Pmax | Pmin


  -- :: (Num# a#) => a# -> a# -> Int#
  | Peq  | Pne  | Plt  | Ple  | Pgt  | Pge

  -- :: Int -> Int -> Int
  | Psll -- logical left shift
  | Psra -- arithmetic right shift
  | Psrl -- logical right shift

  -- :: String# -> Int# -> Int#
  | PidxChr -- get char at index

  -- :: Int# -> Int#
  | Pord | Pchr
  -- Both are No Ops: Char and Int are equivalent for us. These could both be
  -- some kind of PNoOp or PiIdent (they're only here to help with mapping GHC
  -- primitives) but differentiating between them may be useful at some point.

 -- :: String# -> a
  | Praise 

  deriving (Eq, Ord, Enum, Bounded, Show)


-- | Assoc List of primop names (in STG syntax) and primops.
primOpTab :: [(String, PrimOp)]
primOpTab =
  let polyPrepped = map (tail . show) polyOps
      constrPrepped = map (tail . show) constrainedOps
      addHash = map (++ "#")
      names = map ('i':) polyPrepped ++ map ('d':) polyPrepped ++ constrPrepped
      ops   =            polyOps     ++            polyOps     ++ constrainedOps
  in zip (addHash names) ops

-- The classification of PrimOps is dependent on the Enum instance. See note
-- above the PrimOp definition.  We're faking some kind of "polymorphism" in the
-- numeric PrimOps to keep the code clean and readable.  They can all be applied to
-- either Int# or Double#, and the PrimOpInfo (constructed at Parse-time) provides
-- all the necessary information for typechecking and codegen
allOps, polyOps, polyUnEndoOps, polyBiOps, polyBiEndoOps, compareOps, constrainedOps :: [PrimOp]
-- Poly ==> "Polymorphic" (not really)
-- Endo ==> arguments stay in the same category (or type, in this case)
--   Probably not the best term, but I don't know what else fits.
-- Bi   ==> binary op
-- Un   ==> unary op
allOps         = [minBound .. ]
polyOps        = polyUnEndoOps ++ polyBiOps
polyUnEndoOps  = [Pneg]
polyBiOps      = polyBiEndoOps ++ compareOps
polyBiEndoOps  = [Padd .. Pmin]
compareOps     = [Peq  .. Pge]
constrainedOps = [Psll .. Pchr]

data PrimOpInfo =
  POI { opType  :: Monotype -- invariant: MFun + MPrim only (nested)
      , pArgTys :: [PrimType]
      , pRetTy  :: PrimType

      -- This makes more sense in CodeGen, but it's simpler to keep all the Info
      -- together here.  Functions are implemented directly with C AST types to
      -- avoid another module dependent on TemplateHaskell
      , primCGFun  :: [C.Exp] -> C.Exp
      }

instance Eq PrimOpInfo where
  (POI mt1 args1 ret1 _ ) == (POI mt2 args2 ret2 _ )
    = mt1 == mt2 && args1 == args2 && ret1 == ret2

instance Show PrimOpInfo where
  showsPrec _ (POI ty args ret _) =
    showString $ show ty ++ show args ++ show ret

instance Ord PrimOpInfo where
  (POI mt1 args1 ret1 _ ) `compare` (POI mt2 args2 ret2 _ )
    = mt1   `compare` mt2   >|
      args1 `compare` args2 >|
      ret1  `compare` ret2

opArity :: PrimOpInfo -> Int
opArity info = length (pArgTys info)

-- | Make a C FnCall Exp given the name of the function and the list of
-- arguments (Exps).  In practice, this is used to make the codegen functions
-- for primops for which we have a runtime function/macro.
mkFunCall :: String -- | Name of the function
        -> ([C.Exp] -> C.Exp)
mkFunCall name exps = C.FnCall (C.Var (C.Id name mempty) mempty) exps mempty



-- | Make PrimOpInfo given the (properly ordered) list of PrimTypes the
-- associated PrimOp operates on, its PrimType return and a function to generate
-- an abstract C Expression
mkOpInfo' :: [PrimType] -- | Argument types
         -> PrimType   -- | Return type
         -> ([C.Exp] -> C.Exp) -- | Codegen function
         -> PrimOpInfo
mkOpInfo' argTys retTy cgFun =
  let otyp = foldr (MFun . MPrim) (MPrim retTy) argTys
  in POI { opType    = otyp
         , pArgTys   = argTys
         , pRetTy    = retTy
         , primCGFun = cgFun
         }

-- | Make PrimOp info for a PrimOp that maps to a unary operator in C.
-- This makes a codegen function that produces a C UnOp Exp.
mkUnOpInfo :: PrimType -- | The argument type to the associated PrimOp
           -> PrimType -- | The return type of the associated PrimOp
           -> C.UnOp   -- | The C unary operator to use
           -> PrimOpInfo
mkUnOpInfo ty rt cOP = mkOpInfo' [ty] rt $ \[e] -> C.UnOp cOP e  mempty

mkBinOpInfo     t1 t2 rt cOP   = mkOpInfo' [t1,t2] rt $ \[e1,e2] -> C.BinOp cOP e1 e2 mempty
mkBinFunInfo    t1 t2 rt fname = mkOpInfo' [t1,t2] rt (mkFunCall fname)
mkBinEndoOpInfo ty cOP         = mkBinOpInfo ty ty ty cOP

mkOpInfo :: Char -> PrimOp -> PrimOpInfo
mkOpInfo c op =
  let ty = case c of
             'i' -> PInt
             'd' -> PDouble
             's' -> PString
             _   -> error $ "mkOpInfo given bad char: " ++ [c]
  in
    case op of
      Padd    -> mkBinEndoOpInfo ty C.Add
      Psub    -> mkBinEndoOpInfo ty C.Sub
      Pmul    -> mkBinEndoOpInfo ty C.Mul
      Pdiv    -> mkBinEndoOpInfo ty C.Div
      Pmod    -> mkBinEndoOpInfo ty C.Mod

      Pmax    -> mkBinFunInfo ty ty ty (c:"max")
      Pmin    -> mkBinFunInfo ty ty ty (c:"min")

      Peq     -> mkBinOpInfo ty ty PInt C.Eq
      Pne     -> mkBinOpInfo ty ty PInt C.Ne
      Plt     -> mkBinOpInfo ty ty PInt C.Lt
      Ple     -> mkBinOpInfo ty ty PInt C.Le
      Pgt     -> mkBinOpInfo ty ty PInt C.Gt
      Pge     -> mkBinOpInfo ty ty PInt C.Ge

      Pneg    -> mkUnOpInfo ty ty C.Negate

      Psll    -> mkBinEndoOpInfo PInt C.Lsh
      Psra    -> mkBinEndoOpInfo PInt C.Rsh
      Psrl    -> mkBinFunInfo PInt PInt PInt "intLogicalRightShift"

      PidxChr -> mkOpInfo' [PString, PInt] PInt (mkFunCall "charAtIndex")

      -- ord and chr take one argument, so simply returning the head of the list
      -- of C.Exps accomplishes the NOP logic for Codegen
      Pord -> mkOpInfo' [PInt] PInt head
      Pchr -> mkOpInfo' [PInt] PInt head
      Praise -> mkOpInfo' [] PVoid (mkFunCall "raise")

maybeTypeOfAtom :: Atom -> Maybe Monotype
maybeTypeOfAtom at = case at of
  Var _    -> Nothing
  LitC c   -> Just $ MCon (Just False) c []
  LitI _   -> Just $ MPrim PInt
  LitD _   -> Just $ MPrim PDouble
  LitStr _ -> Just $ MPrim PString

instance Unparse Atom where
  unparse (Var v)  = stgName v
  unparse (LitI i) = int64 i
  unparse (LitD d) = text $ show d
  unparse (LitC c) = text c
  unparse (LitStr s) = text s

-- just the suffix of the op. Unparse EPrimop makes the prefix
instance Unparse PrimOp where
    unparse = (<> hash) . text . tail . show

instance Unparse a => Unparse (Alt a) where
  unparse ACon{amd, ac, avs, ae} =
    bcomment (unparse amd) $+$
    stgName ac <+> hsep (map stgName avs) <+> arw <+> unparse ae

  unparse ADef{amd, av, ae} =
    bcomment (unparse amd) $+$
    stgName av <+> arw <+> unparse ae

instance Unparse a => Unparse (Alts a) where
  unparse Alts{altsmd, alts, scrt} = -- Note aname field is *not* in use here
    unparse scrt <+> lbrace $+$
    nest 2
    (bcomment (unparse altsmd) $+$
     vcat (punctuate semi $ map unparse alts) <+> rbrace)

instance Unparse a => Unparse (Expr a) where
  unparse EAtom{emd, ea} =
    bcomment (unparse emd) $+$
    unparse ea

  unparse EFCall{emd, ev, eas} =
    bcomment (unparse emd) $+$
    stgName ev <+> hsep (map unparse eas)

  unparse EPrimOp{emd, eprimOp, eopInfo, eas} =
    let pfx | eprimOp `elem` polyOps = text (primTypeStrId (head $ pArgTys eopInfo))
            | otherwise = empty
        opDoc = pfx <> unparse eprimOp
    in bcomment (unparse emd) $+$ opDoc <+> hsep (map unparse eas)

  unparse ELet{emd, edefs, ee} =
    bcomment (unparse emd) $+$
    text "let" <+> lbrace $+$
    nest 2 (vcat $ punctuate semi $ map unparse edefs) <> rbrace $+$
    text "in" <+> unparse ee

  unparse ECase{emd, ee, ealts} =
    bcomment (unparse emd) $+$
    text "case" <+> unparse ee <+> text "of" $+$
    (nest 2 $ unparse ealts)


instance Unparse a => Unparse (Obj a) where
  unparse FUN{omd, vs, e, oname} =
    bcomment (unparse omd) $+$
    stgName oname <+> equals <+> text "FUN" <>
    parens (hsep (map stgName vs) <+> arw $+$ nest ident (unparse e))
    where ident = length vs + sum (map length vs) -- aligns expr with arrow

  unparse PAP{omd, f, as, oname} =
    bcomment (unparse omd) $+$
    stgName oname <+> equals <+> text "PAP" <>
    parens (stgName f <+> hsep (map unparse as))

  unparse CON{omd, c, as, oname} =
    bcomment (unparse omd) $+$
    stgName oname <+> equals <+> text "CON" <>
    parens (stgName c <+> hsep (map unparse as))

  unparse THUNK{omd, e, oname} =
    bcomment (unparse omd) $+$
    stgName oname <+> equals <+> text "THUNK" <>
    parens (unparse e)

--BH  unparse BLACKHOLE{omd, oname} =
--BH    bcomment (unparse omd) $+$
--BH    text oname <+> equals <+> text "ERROR"


instance Unparse a => Unparse [Obj a] where
  unparse objs = vcat $ postpunctuate semi $ map unparse objs


instance Unparse a => Unparse (Def a) where
  unparse (DataDef t) =  unparse t
  unparse (ObjDef o) = unparse o

instance Unparse a => Unparse [Def a] where
  unparse defs = vcat $ postpunctuate semi $ map unparse defs


objListDoc :: (PPrint a) => [Obj a] -> Doc
objListDoc objs = vcat $ map pprint objs

instance (PPrint a) => PPrint (Obj a) where
  pprint o = case o of
    FUN{..} -> braces
               (text "FUN:" $+$
                nest 2
                (text "name:" <+> text oname $+$
                 text "vars:" <+> listText vs $+$
                 text "metadata:" $+$
                 nest 2 (pprint omd) $+$
                 text "expr:" $+$
                 nest 2 (pprint e)
                )
               )
    PAP{..} -> braces
               (text "PAP:" $+$
                nest 2
                (text "name:" <+> text oname $+$
                 text "function:" <+> text f $+$
                 text "args:" <+> brackList (map pprint as) $+$
                 text "metadata:" $+$
                 nest 2 (pprint omd)
                )
               )
    CON{..} -> braces
               (text "CON:" $+$
                nest 2
                (text "name:" <+> text oname $+$
                 text "datacon:" <+> text c $+$
                 text "args:" <+> brackList (map pprint as) $+$
                 text "metadata:" $+$
                 nest 2 (pprint omd)
                )
               )
    THUNK{..} -> braces
                 (text "THUNK:" $+$
                  nest 2
                  (text "name:" <+> text oname $+$
                   text "metadata:" $+$
                   nest 2 (pprint omd) $+$
                   text "expression:" $+$
                   nest 2 (pprint e)
                  )
                 )
--BH    BLACKHOLE{..} -> braces
--BH                     (text "BLACKHOLE" $+$
--BH                      nest 2
--BH                      (text "name:" <+> text oname $+$
--BH                       text "metadata:" $+$
--BH                       nest 2 (pprint omd)
--BH                      )
--BH                     )
instance (PPrint a) => PPrint (Expr a) where
  pprint e = case e of
    EAtom{..} -> braces
                 (text "EAtom:" $+$
                  nest 2
                  (text "atom:" <+> pprint ea $+$
                   text "metadata:" $+$
                   nest 2 (pprint emd)
                  )
                 )
    EFCall{..} -> braces
                  (text "EFCall:" $+$
                   nest 2
                   (text "function:" <+> text ev $+$
                    -- 7.9 changed, grab atoms from eas (same in primop below)
                    text "args:" <+> brackets (vcat $ punctuate comma $ map pprint eas) $+$
                    text "metadata:" $+$
                    nest 2 (pprint emd)
                   )
                  )
    EPrimOp{..} -> braces
                  (text "EPrimOp:" $+$
                   nest 2
                   (text "primOp:" <+> pprint eprimOp $+$
                    text "primInfo:" <+> pprint eopInfo $+$
                    text "args:" <+> brackets (vcat $ punctuate comma $ map pprint eas) $+$
                    text "metadata" $+$
                    nest 2 (pprint emd)
                   )
                  )
    ELet{..} -> braces
                (text "ELet:" $+$
                 nest 2
                 (text "metadata:" $+$
                  nest 2 (pprint emd) $+$
                  text "definitions:" $+$
                  nest 2 (objListDoc edefs) $+$
                  text "expression:" $+$
                  nest 2 (pprint ee)
                 )
                )
    ECase{..} -> braces
                 (text "ECase:" $+$
                  nest 2
                  (text "metadata:" $+$
                   nest 2 (pprint emd) $+$
                   text "scrutinee:" $+$
                   nest 2 (pprint ee) $+$
                   text "alts:" $+$
                   nest 2 (pprint ealts)
                  )
                 )

instance (PPrint a) => PPrint (Alts a) where
  pprint Alts{..} = braces
                    (text "Alts:" $+$
                     nest 2
                     (text "name:" <+> text aname $+$
                      text "metadata:" $+$
                      nest 2 (pprint altsmd) $+$
                       text "scrutinee binding:" $+$
                       nest 2 (pprint scrt) $+$
                      text "alt defs:" $+$
                      nest 2 (vcat (map pprint alts))
                     )
                    )
instance (PPrint a) => PPrint (Alt a) where
  pprint a = case a of
    ACon{..} -> braces
                (text "ACon:" $+$
                 nest 2
                 (text "datacon:" <+> text ac $+$
                  text "vars:" <+> listText avs $+$
                  text "metadata:" $+$
                  nest 2 (pprint amd) $+$
                  text "expression:" $+$
                  nest 2 (pprint ae)
                 )
                )
    ADef{..} -> braces
                (text "ADef:" $+$
                 nest 2
                 (text "var:" <+> text av $+$
                  text "metadata:" $+$
                  nest 2 (pprint amd) $+$
                  text "expression:" $+$
                  nest 2 (pprint ae)
                 )
                )

instance PPrint Atom where
  pprint a = case a of
    Var v -> text "Var" <> braces (text v)
    LitI i -> text "LitI" <> braces (int64 i)
    LitD d -> text "LitD" <> braces (double d)
    LitC c -> text "LitC" <> braces (text c)
    LitStr s -> text "LitStr" <> braces (text s)

instance PPrint PrimOp where
  pprint = unparse

instance PPrint PrimOpInfo where
  pprint info = empty -- TODO: Something useful here
