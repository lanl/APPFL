{-# LANGUAGE NamedFieldPuns #-}

module AST (
  Var,
  Con,
  Atom(..),
  Expr(..),
  Alt(..),
  Alts(..),
  Obj(..),
  Primop(..),
  primopTab,
  show,
  BuiltinType(..)
) where

import PPrint
import Data.List (find)

{-  grammar

<var> :: C syntax, more or less

<con> :: start with uppercase

<lit> ::= int[eger]

<atom> ::= <lit> | <var>

<prog> ::= <def> (";" <def>)*

<obj> ::= "FUN" "(" <var>+ -> <expr> ")"
       |  "PAP" "(" <var> <atom>+ ")"
       |  "CON" "(" <con> <atom>* ")"
       |  "THUNK" <expr>
       |  "ERROR"  (aka BLACKHOLE)

<expr> ::= <atom>
       |  <var>"^"<arity> <atom>+
       |  <primop> <atom>+
       |  "let" "{" <defs> "}" "in" <expr>
       |  "case" <expr> "of" "{" <alts> "}"

<alts> ::= <alt> (";" <alt>)*

<alt> ::= <con> <var>* "->" <expr>
       |  <var> "->" <expr>

<arity> ::= <pos> | "_"

-}

-- not really the place for this, maybe need to factor
-- the common types into a module
data BuiltinType = UBInt
                 | UBDouble             
                 | UBBool             
                   deriving (Eq,Show,Ord)           

type Var = String
type Con = String

data Atom = Var  Var
          | LitI Int
          | LitB Bool
          | LitF Float
          | LitD Double
          | LitC Char
            deriving(Eq)

instance Show Atom where
    show (Var v) = v
    show (LitI i) = show i
    show (LitB False) = "false#"
    show (LitB True) = "true#"
    show (LitF f) = show f ++ "(f)"
    show (LitD d) = show d ++ "(d)"
    show (LitC c) = [c]

data Expr a = EAtom   {emd :: a,                    ea :: Atom} --,      ename::String}
            | EFCall  {emd :: a, ev :: Var,         eas :: [Atom]} --,   ename::String}
            | EPrimop {emd :: a, eprimop :: Primop, eas :: [Atom]} --,   ename::String}
            | ELet    {emd :: a, edefs :: [Obj a],  ee :: Expr a} --,    ename::String}
            | ECase   {emd :: a, ee :: Expr a,      ealts :: Alts a} --, ename::String}
              deriving(Eq,Show)

data Alt a = ACon {amd :: a, ac :: Con, avs :: [Var], ae :: Expr a}
           | ADef {amd :: a,            av :: Var,    ae :: Expr a}
             deriving(Eq,Show)

data Alts a = Alts {altsmd :: a, alts :: [Alt a], aname :: String}
              deriving(Eq,Show)

data Obj a = FUN   {omd :: a, vs :: [Var],   e :: (Expr a), oname :: String}
           | PAP   {omd :: a, f  :: Var,     as :: [Atom],  oname :: String}
           | CON   {omd :: a, c  :: Con,     as :: [Atom],  oname :: String}
           | THUNK {omd :: a, e  :: (Expr a)             ,  oname :: String}
           | BLACKHOLE {omd :: a                         ,  oname :: String}
             deriving(Eq,Show)

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

            -- the following are deprecated
            | PintToBool
              deriving(Eq,Show)

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

     ("imin_h",   Pimax),
     ("imax_h",   Pimin),

     -- the following are deprecated
     ("intToBool_h", PintToBool)
    ]

stgPrimName p =
  case find ((==p).snd) primopTab of
   Nothing -> error $ "primop lookup failed for " ++ show p
   Just x -> reverse ('#' : (drop 2 $ reverse $ fst x))
     
instance PPrint Atom where
  toDoc (Var v)  = text v
  toDoc (LitI i) = int i
  toDoc (LitF f) = float f
  toDoc x        = text $ show x -- not expecting other literals yet


instance PPrint Primop where
  toDoc = text.stgPrimName 

instance PPrint a => PPrint (Alt a) where
  toDoc ACon{amd, ac, avs, ae} =
    toDoc amd $+$
    text ac <+> (hsep $ map text avs) <+> arw <+> toDoc ae

  toDoc ADef{amd, av, ae} =
    toDoc amd $+$
    text av <+> arw <+> toDoc ae

instance PPrint a => PPrint (Alts a) where
  toDoc Alts{altsmd, alts} = -- Note aname field is *not* in use here
    toDoc altsmd $+$
    (vcat $ punctuate semi $ map toDoc alts)

instance PPrint a => PPrint (Expr a) where
  toDoc EAtom{emd, ea} =
    toDoc emd $+$
    toDoc ea

  toDoc EFCall{emd, ev, eas} =
    toDoc emd $+$
    text ev <+> (hsep $ map toDoc eas)

  toDoc EPrimop{emd, eprimop, eas} =
    toDoc emd $+$
    toDoc eprimop <+> (hsep $ map toDoc eas)

  toDoc ELet{emd, edefs, ee} =
    toDoc emd $+$
    text "let" <+> lbrace $+$
    (nest 2 $ vcat $ punctuate semi $ map toDoc edefs) <> rbrace $+$
    text "in" <+> toDoc ee

  toDoc ECase{emd, ee, ealts} =
    toDoc emd $+$
    text "case" <+> toDoc ee <+> text "of" <+> lbrace $+$
    (nest 2 $ toDoc ealts) <> rbrace


instance PPrint a => PPrint (Obj a) where
  toDoc FUN{omd, vs, e, oname} =
    toDoc omd $+$
    text oname <+> equals <+> text "FUN" <>
    parens ((hsep $ map text vs) <+> arw $+$ nest ident (toDoc e))
    where ident = length vs + (sum $ map length vs) -- aligns expr with arrow

  toDoc PAP{omd, f, as, oname} =
    toDoc omd $+$
    text oname <+> equals <+> text "PAP" <>
    parens (text f <+> (hsep $ map toDoc as))

  toDoc CON{omd, c, as, oname} =
    toDoc omd $+$
    text oname <+> equals <+> text "CON" <>
    parens (text c <+> (hsep $ map toDoc as))

  toDoc THUNK{omd, e, oname} =
    toDoc omd $+$
    text oname <+> equals <+> text "THUNK" <>
    parens (toDoc e)

  toDoc BLACKHOLE{omd, oname} =
    toDoc omd $+$
    text oname <+> equals <+> text "ERROR"


-- () metadata = empty document
-- empty is the identity Doc for the associative pretty printing operators
instance PPrint () where
  toDoc () = empty
