{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecordWildCards   #-}

module AST (
  Var,
  Con,
  Atom(..),
  Expr(..),
  Alt(..),
  Alts(..),
  Obj(..),
  Primop(..),
  rmPrelude,
  primopTab,
  show,
  objListDoc,
  primArity,
  projectAtoms,
  scrtVarName
) where

import PPrint
import Data.List (find, (\\))
import Data.Int as Int (Int64)

--  See Parser.hs for grammar

type Var = String
type Con = String

data Atom = Var  Var
          | LitI Int
          | LitL Int.Int64
          | LitF Float
          | LitD Double
          | LitC Con
          | LitStr String -- primitive string e.g. const char *str = "hello world";
            deriving(Eq)

instance Show Atom where
    show (Var v)    = v
    show (LitI i)   = show i
    show (LitL l)   = show l
    show (LitF f)   = show f ++ "(f)"
    show (LitD d)   = show d ++ "(d)"
    show (LitC c)   = c
    show (LitStr s) = s

data Obj a = FUN   {omd :: a, vs :: [Var],   e :: Expr a   , oname :: String}
           | PAP   {omd :: a, f  :: Var,     as :: [Expr a], oname :: String} -- invariant the as are EAtoms
           | CON   {omd :: a, c  :: Con,     as :: [Expr a], oname :: String} -- invariant the as are EAtoms
           | THUNK {omd :: a, e  :: Expr a                 , oname :: String}
--BH           | BLACKHOLE {omd :: a                           , oname :: String}
             deriving(Eq,Show)

-- 7.9 EFCalls (and EPrimops, for consistency) changed to accept Expr args.
-- Parser (and other traversals) should enforce use of *only* EAtom as args
data Expr a
    = EAtom   {emd :: a,
               ea :: Atom}
    | EFCall  {emd :: a,
               ev :: Var,
               eas :: [Expr a]} -- invariant the eas are EAtoms
    | EPrimop {emd :: a,
               eprimop :: Primop,
               eas :: [Expr a]} -- invariant the eas are EAtoms
    | ELet    {emd :: a,
               edefs :: [Obj a],
               ee :: Expr a}
    | ECase   {emd :: a,
               ee :: Expr a,
               ealts :: Alts a}               
              deriving(Eq,Show)

data Alts a = Alts {altsmd :: a,
                    alts :: [Alt a],
                    aname :: String,
                    scrt :: Expr a} -- invariant the scrt is an EAtom holding a Var
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

-- when calculating free variables we need an enclosing environment that
-- includes the known primops.  This will allow proper scoping.  As such
-- we initially parse EPrimops as EFCalls, then transform and check saturation
-- here.  We only need to distinguish EPrimop from EFCall because special
-- code is generated for them.

data Primop = Piadd -- Int -> Int -> Int
            | Pisub
            | Pimul
            | Pidiv
            | Pimod
            | Pimax
            | Pimin

            | Pieq -- Int -> Int -> Bool
            | Pine
            | Pilt
            | Pile
            | Pigt
            | Pige

            | Pineg -- Int -> Int
            | Pinvalid
              deriving(Eq,Show, Ord)

primArity op = case op of
  Pineg -> 1
  _ -> 2

-- these are the C names, not STG names
primopTab =
    [("iplus_h",  Piadd),
     ("isub_h",   Pisub),
     ("imul_h",   Pimul),
     ("idiv_h",   Pidiv),
     ("imod_h",   Pimod),
    
     ("ieq_h",    Pieq),
     ("ine_h",    Pine),
     ("ilt_h",    Pilt),
     ("ile_h",    Pile),
     ("igt_h",    Pigt),
     ("ige_h",    Pige),

     ("ineg_h",   Pineg),

     ("imax_h",   Pimax),
     ("imin_h",   Pimin)
    ]


-- generate with
-- awk '/^[a-z_#].*=/ {if ($1 != "data") {printf "\42%s\42, ", $1}}' \
-- appfl/prelude/Prelude.stg | head -c-2 | fmt -w80

preludeObjNames =
    [
     "error", "unit", "nil", "zero", "one", "two", "three", "four", "five", "six",
     "seven", "eight", "nine", "ten", "false", "true", "blackhole", "_iplus",
     "_isub", "_imul", "_idiv", "_imod", "_ieq", "_ine", "_ilt", "_ile", "_igt",
     "_ige", "_imin", "_imax", "_ineg", "cons", "int", "tupl2", "fst", "snd",
     "tupl3", "eqInt", "multInt", "plusInt", "subInt", "modInt", "_intPrimop",
     "_intComp", "intLE", "minInt", "gcd#", "gcd", "append", "map", "head",
     "tail", "foldl", "foldr", "length", "_length", "forcelist", "take", "drop",
     "zipWith", "zip", "strictList", "null", "init", "filter", "all", "any",
     "sum", "const", "apply", "seq", "repeat", "replicate", "odd#", "even#",
     "odd", "even", "not", "compose", "divInt", "compareInt", "intLT", "intGE",
     "intGT", "switch", "move", "removeAtIndex", "insertAtIndex", "index",
     "eqList", "createNormArray", "cNArr", "createNormBackArray", "cNBArr",
     "createArray", "cArr", "createOddBackArray", "cOBArr", "createEvenArray",
     "cEArr", "createEvenBackArray", "cEBArr", "createOddArray", "cOArr"
    ]



-- functions for removing some or all standard Prelude objects from the
-- list of objects in a parsed STG program.  Useful for debugging.
rmPrelude :: [Obj a] -> [Obj a]
rmPrelude = rmPreludeLess []
rmPreludeLess :: [Var] -> [Obj a] -> [Obj a]
rmPreludeLess keeps =
  let objs = preludeObjNames \\ keeps
  in filter (not . (`elem` objs) . oname)

primID p =
  case find ((==p).snd) primopTab of
    Nothing -> error $ "primop lookup failed for " ++ show p
    Just x -> fst x

     
instance Unparse Atom where
  unparse (Var v)  = stgName v
  unparse (LitI i) = int i
  unparse (LitL l) = text $ show l
  unparse (LitF f) = float f
  unparse (LitD d) = text $ show d
  unparse (LitC c) = text c
  unparse (LitStr s) = text s

instance Unparse Primop where
  unparse = stgName . primID

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

  unparse EPrimop{emd, eprimop, eas} =
    bcomment (unparse emd) $+$
    unparse eprimop <+> hsep (map unparse eas)

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
         

--instance PPrint BuiltinType where
--  pprint b = case b of
--    UBInt -> text "UBInt"
--    UBDouble -> text "UBDouble"
    
 


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
    EPrimop{..} -> braces
                   (text "EPrimop:" $+$
                    nest 2
                    (text "primop:" <+> pprint eprimop $+$
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
    LitI i -> text "LitI" <> braces (int i)
    LitL l -> text "LitL" <> braces (text $ show l)
    LitD d -> text "LitD" <> braces (double d)
    LitC c -> text "LitC" <> braces (text c)
    a -> error $ "AST.pprint (Atom): not expecting Atom - " ++ show a

instance PPrint Primop where
  pprint = unparse
