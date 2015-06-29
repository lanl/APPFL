{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

import DavidParser
import DataParser
import Tokenizer
import Text.PrettyPrint
import Data.List (groupBy, find)
import qualified Data.Map as Map
import Data.Monoid hiding ((<>))
import AST
import ADT



-- make it easy to group parsed and unparsed input together for later filtration
data Parsed a b = Parsed [a] | Unparsed [b] deriving (Show)

-- I may be abusing/misusing this given the dependence on constructor types
instance Monoid (Parsed a b) where
  mappend (Parsed as) (Parsed bs) = Parsed (as ++ bs)
  mappend (Unparsed as) (Unparsed bs) = Unparsed (as ++ bs)
  mappend _ _ = error "Can't mappend Parsed and Unparsed containers"
  mconcat = foldr1 mappend
  mempty = Parsed []
  
groupParsed =
  let
    aux (Parsed _) (Parsed _)     = True
    aux (Unparsed _) (Unparsed _) = True
    aux _ _                       = False
  in
   groupBy aux



testConMaps fl = do
  file <- readFile fl
  let
    ((parsed,_):_) = prog $ tokenize file
    maps = toConMaps $
           map fromDataDef $
           filter isDataDef $
           concatMap fromParsed parsed
  putStrLn $ show $ toDoc maps
  

fromParsed (Parsed a) = a
fromParsed _ = []

isDataDef (DataDef _) = True
isDataDef _ = False

fromDataDef (DataDef t) = t

testParse fl = do
  file <- readFile fl
  let ((parsed,_):_) = prog $ tokenize file
  mapM_ (putStrLn.show) $ map mconcat $ groupParsed parsed

testUnparse f1 = do
  file <- readFile f1
  let ((parsed,_):_) =  prog $ tokenize file
  mapM_ (putStrLn.show.toDoc) $ map mconcat $ groupParsed parsed

parseUnparse f1 f2 = do
  file <- readFile f1
  let
    ((parsed,_):unparsed) = prog $ tokenize file
    out = concatMap (show.toDoc.mconcat) $ groupParsed parsed
  writeFile f2 out

prog = many'
       (orExList [
           objDefSC `using` (Parsed . (:[]) . ObjDef),
           dataDefP `using` (Parsed . (:[])),
           notEOF `using` (Unparsed. (:[]))
           ]
       )
       `thenx` failTok eofP "EOF not found"
       
     
objDefP = varNameP `thenx` eqP `ordered` objP `using` (uncurry $ flip ($))
objDefSC = objDefP `thenx` scP

objP :: Parser Token (String -> Obj ())
objP = orExList [funP `using` (uncurry $ FUN ()),
                 papP `using` (uncurry $ PAP ()),
                 conP `using` (uncurry $ CON ()),
                 thunkP `using` (THUNK ()),
                 errorP `using` (const $ BLACKHOLE ())]

objPat s p = rsvP s `xthen` (inparensP p)

funP :: Parser Token ([Var],Expr ())
funP =
  let
    funDefP = some' varNameP `thenx` arrowP `ordered` exprP
  in objPat "FUN" funDefP

papP :: Parser Token (Var, [Atom])
papP = objPat "PAP" $ varNameP `ordered` (many' atomP)

conP :: Parser Token (Con, [Atom])
conP = objPat "CON" $ conNameP `ordered` (many' atomP)

thunkP :: Parser Token (Expr ())
thunkP = objPat "THUNK" $ exprP

errorP :: Parser Token Token
errorP = rsvP "ERROR"


--------------------- Expression parsers -------------------------

exprP :: Parser Token (Expr ())
exprP = orExList [
  fCallP `using` (uncurry $ EFCall ()), -- if this is not checked first, "f x" matches atomP first
  atomP `using` (EAtom ()),
  primopP `using` (uncurry $ EPrimop ()),
  letP `using` (uncurry $ ELet ()),
  caseP `using` (uncurry $ ECase ())
  -- inparensP exprP -- doesn't show up in ghc stg but may be useful?
  ]

atomP :: Parser Token Atom
atomP = orExList [
  varNameP `using` Var,
  intP `using` LitI,
  --boolP `using` LitB,
  fltP `using` LitF
  --dblP `using` LitD,
  --chrP `using` LitC
  ]

fCallP = varNameP `ordered` some' atomP

primopP = primP `ordered` some' atomP

letP = let inletP p = xthenx (rsvP "let") (inbraces p) (rsvP "in")
           scObjDef = scP `xthen` objDefP
           defs = objDefP `ordered` (many' scObjDef) `using` cons
       in inletP defs `ordered` exprP

caseP = let inCase p = xthenx (rsvP "case") p (rsvP "of")
        in inCase exprP `ordered` (inbraces altsP)


altsP =
  let name = error "this alts not given a name!"
      fun = (.) (flip $ Alts ()) name $ cons
  in altP `ordered` many' (scP `xthen` altP) `using` cons `using` ((flip $ Alts ()) name)

altP =
  let -- ACon can't match something like Pair 1 1 ? only Pair a b, then scrutinize in later case?
    altConP = conNameP `ordered` many' varNameP `using` (uncurry $ ACon ())
    altDefP = varNameP `using` (ADef ())
  in orExList [altConP, altDefP] `thenx` arrowP `ordered` exprP `using` (uncurry ($))



--------------------------- Pretty Printing -------------------------

class PPrint a where
  toDoc :: a -> Doc

bar = text "|"
arw = text "->"
lcomment d = text "--" <> d
bcomment d = text "{-" <+> (nest 3 d) $+$ text "-}"



--------------- ADT Pretty Printing -----------------------

instance PPrint Monotype where
  toDoc = text.show

instance PPrint Token where
  toDoc = text.show
  
instance PPrint DataCon where
  toDoc (DataCon con mTypes) = text con <+> hsep (map toDoc mTypes)

instance PPrint TyCon where
  toDoc (TyCon boxed name vars dCons) =
    let
      lh = 
        (if boxed then empty else text "unboxed") <+>
        text name <+> hsep (map text vars) <+> equals
      rh = nest (indent+1) $ vcat $ punctuate (text " |") $ map toDoc dCons
      indent = length $ show lh
    in lh <+> rh



---------------- AST Pretty Printing -----------------------
{-
Metadata instances for PPrint:
  Metadata is printed on its own line before any other information for all AST data
  constructors. The pretty print for the rest of the AST information is
  printed on the line following.
  Metadata will be indented exactly as much as the object it pertains to unless
  otherwise defined.
  To unindent the metadata wrap its document using "nest n" where n should some
  large negative number. 
  

-}

instance PPrint Atom where
  toDoc (Var v)  = text v
  toDoc (LitI i) = int i
  toDoc (LitF f) = float f
  toDoc x        = text $ show x -- not expecting other literals yet


instance PPrint Primop where
  toDoc x =
    let m = find ((== x).snd) primopTable
    in maybe (error $ "primop lookup failed for " ++ show x) (text.fst) m

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


instance PPrint a => PPrint (Def a) where
  toDoc (DataDef t) = text "data" <+> toDoc t
  toDoc (ObjDef o) = toDoc o


-- () metadata = empty document
-- empty is the identity Doc for the associative pretty printing operators
instance PPrint () where
  toDoc () = empty

  

instance (PPrint a, PPrint b) => PPrint (Parsed a b) where
  toDoc (Parsed defs) =
    (lcomment $ text "PARSED:") $+$ (vcat $ punctuate semi $ map toDoc defs) <> semi
  toDoc (Unparsed toks) =
    bcomment $ text "UNPARSED:" $+$ (vcat $ map toDoc toks)


----------------------- ConMaps Pretty Printing -------------------------
{-
ttag and dtag are ignored. not sure what they're used for yet
TyCon and DataCon fields are only used for associated names: their respective
pretty printing definitions are ignored
-}

instance PPrint DataConParam where
  toDoc DataConParam{darity, dboxed, dtycon, datacon} = case datacon of
    (DataCon n _) ->
      text "DataCon:" <+> text n <+> lbrace $+$ nest 2 (
        text "arity:" <+> int darity $+$
        text "boxed:" <+> text (if dboxed then "yes" else "no") $+$
        text "TyCon:" <+> text dtycon <> rbrace)

instance PPrint TyConParam where
  toDoc TyConParam{tarity, tboxed, tdatacons, tycon} = case tycon of
    (TyCon _ n _ _) ->
      text "TyCon:" <+> text n <+> lbrace $+$ nest 2 (
        text "arity:" <+> int tarity $+$
        text "boxed:" <+> text (if tboxed then "yes" else "no") $+$
        text "DataCons:" <+> brackets (hsep $ punctuate comma $ map text tdatacons) <> rbrace)
                                                                      
instance PPrint ConMaps  where
  toDoc (tcm, dcm) =
    let f (str, tcp) = text str <+> arw $+$ nest 2 (toDoc tcp)
    in (vcat $ map f $ Map.toList tcm) $+$
       (vcat $ map f $ Map.toList dcm)
