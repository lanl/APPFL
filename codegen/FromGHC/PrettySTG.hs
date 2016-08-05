{-# LANGUAGE UndecidableInstances, FlexibleInstances, PatternGuards #-}



module FromGHC.PrettySTG where

import           FromGHC.Naming
import           State hiding (liftM)
import           Util
import qualified PPrint as P

import           Control.Monad
import qualified Data.Map as Map

import           StgSyn
                 ( StgBinding, GenStgBinding (..)
                 , StgExpr, GenStgExpr (..), StgOp (..)
                 , StgRhs, GenStgRhs (..)
                 , StgArg, GenStgArg (..)
                 , StgAlt, AltType (..))
import           CoreSyn (AltCon (..))
import           DataCon (dataConWrapId)
import           Literal (Literal (..))
import           PrimOp ( PrimCall (..), primOpOcc )
import           ForeignCall
                 ( ForeignCall (..), CCallSpec (..)
                 , CCallTarget (..) )
import           Class (Class (..), classAllSelIds)
import           Var (Var, idDetails, isId, varType)
import           IdInfo (IdDetails (..))
import           TypeRep (Type(..), TyLit(..))


-- OccName == Occurence Name (I think). How identifiers appear in code,
-- essentially.  We only need this to name PrimOps.
import           OccName ( occNameString )

import           Name (NamedThing (..), getOccString)                 
import           FastString (unpackFS)
------------------------------------------------------------------------------
-- Pretty Printing


-- Custom Pretty Printer for better inspection of STG tree.
-- This will make it easier to figure out what Haskell values
-- correspond to what STG code (the ddump-stg option is not fantastic)
type DocS = State PprState P.Doc

class OutputSyn a where
  pprSyn :: a -> DocS

instance UniqueNameState (State PprState) where
  getNamer   = liftM namer get
  putNamer n = modify $ \ps -> ps{namer = n}

instance Show (State PprState P.Doc) where
  show = showDocS

data PprState = PST { namer   :: UniqueNamer
                    , pprOpts :: PprOpts
                    }


-- Taking a page from the SDoc type from GHC: They maintain an SDocContext in a
-- stateful pretty printer and, as necessary, pull information from it to add
-- color or unicode symbols where appropriate.  For now, the only context we might
-- want is a verbosity level.
data PprOpts  = POpts { vlevel :: VLevel }
data VLevel = V0 | V1 | V2 | V3  deriving (Eq, Ord)                

defaultState :: PprState
defaultState = PST{ namer   = (Map.empty, 0)
                  , pprOpts = defaultOpts }

defaultOpts :: PprOpts
defaultOpts = POpts V2

  
pprGhcSyn :: [StgBinding] -> P.Doc
pprGhcSyn binds = mkDocWithState binds defaultState

mkDocWithNamer :: OutputSyn a => a -> UniqueNamer -> P.Doc
mkDocWithNamer thing nmr = mkDocWithState thing defaultState{namer = nmr}

mkDocWithOpts :: OutputSyn a =>  a -> PprOpts-> P.Doc
mkDocWithOpts thing opts = mkDocWithState thing defaultState{pprOpts = opts}


mkDocWithState :: OutputSyn a =>  a -> PprState -> P.Doc
mkDocWithState thing st = fst (runState (pprSyn thing) st)

showDocS :: DocS -> String
showDocS doc = show $ fst $ runState doc defaultState


-- Lifting the PrettyPrinter
underscore, comma, equals, empty, arrow :: DocS
underscore = pure P.underscore
comma      = pure P.comma
equals     = pure P.equals
empty      = pure P.empty
arrow      = pure P.arw
period     = char '.'

text :: String -> DocS
text = return . P.text

integer  :: Integer -> DocS
char     :: Char -> DocS
rational :: Rational -> DocS
integer  = return . P.integer
char     = return . P.char
rational = return . P.rational

vcat, hsep, brackList :: [DocS] -> DocS
vcat = liftM P.vcat . sequence
hsep = liftM P.hsep . sequence
brackList = brackets . hsep . punctuate comma

hang :: DocS -> Int -> DocS -> DocS
hang d1 n d2 = liftM2 (\a b -> P.hang a n b) d1 d2

punctuate :: DocS -> [DocS] -> [DocS]
punctuate _ []  = []
punctuate _ [a] = [a]
punctuate p (x:xs) = x: map (p <>) xs

brackets, parens :: DocS -> DocS
brackets d = pure P.lbrack <> d <> pure P.rbrack
parens d   = pure P.lparen <> d <> pure P.rparen
quotes = liftM P.quotes

(<>), (<+>), ($+$) :: DocS -> DocS -> DocS
(<>)  = liftM2 (P.<>)
(<+>) = liftM2 (P.<+>)
($+$) = liftM2 (P.$+$)


getVLevel :: State PprState VLevel
getVLevel = liftM (vlevel . pprOpts) get

aboveVLevel :: VLevel -> DocS -> DocS
aboveVLevel lvl doc = do
  verb <- getVLevel
  if verb > lvl
    then doc
    else empty


-- Add a prefix if printing verbosely
prefix :: String -> DocS
prefix = aboveVLevel V0 . parens . text
       

pprSynList :: OutputSyn a => [a] -> DocS
pprSynList = brackets . hsep . punctuate comma . map pprSyn

pprStgName :: (AppflNameable a) => a -> DocS
pprStgName t = text =<< nameGhcThing t


-- | Make a string representation of a foreign call
nameForeignCall :: ForeignCall -> String
nameForeignCall (CCall (CCallSpec target _ _))
  = case target of
      StaticTarget fstring _ _ -> unpackFS fstring
      DynamicTarget            -> "Dynamic:Unnamed"


-- Default to the NamedThing instance if no specific (overlapping) instance
-- is defined
instance {-# OVERLAPPABLE #-} AppflNameable a => OutputSyn a where
  pprSyn = pprStgName

instance {-# OVERLAPPABLE #-} OutputSyn a => OutputSyn [a] where
  pprSyn ls = vcat (map pprSyn ls)

instance {-# OVERLAPPING #-} OutputSyn Var where
  pprSyn v = pfx <+> pprStgName v
    where pfx | isId v
            = case idDetails v of
                  dets@DFunId{} ->
                    case last $ unroll $ varType v of
                     TyConApp tc typs ->
                       pprSyn dets <+> aboveVLevel V1
                       (text "tycon:" <+> pprSyn tc <+> text "typs:" <+>
                        brackList (map pprSyn typs))
                       
                     typ -> pprStgName v $+$ pprSyn typ >>= error . show
                       
                  dets -> pprSyn dets
                             
              | otherwise = empty

          unroll ty = case ty of
            FunTy t1 t2 -> t1 : unroll t2
            ForAllTy v t -> unroll t
            t -> [t]
          

instance OutputSyn IdDetails where
  pprSyn dets = case dets of
    VanillaId       -> prefix "VanillaId"
    RecSelId{}      -> prefix "RecSelId"
    DataConWorkId{} -> prefix "DCWorkerId"
    DataConWrapId{} -> prefix "DCWrapperId"
    PrimOpId{}      -> prefix "PrimOpId"
    FCallId{}       -> prefix "FCallId"
    TickBoxOpId{}   -> prefix "TickId"
    DFunId{}        -> prefix "DictFunId"
    ClassOpId clas  -> prefix "ClassOpId" <+>
                       aboveVLevel V1
                       (pprSyn clas)

instance OutputSyn Class where
  pprSyn clas = text "Selectors:" <+>
                brackList (map (text . getOccString) (classAllSelIds clas))

instance OutputSyn StgArg where
  pprSyn (StgVarArg id)  = prefix "VarArg" <+> pprSyn id
  pprSyn (StgLitArg lit) = prefix "LitArg" <+> pprSyn lit


instance OutputSyn Literal where
  pprSyn lit = uncurry ((<>) . prefix) $
    case lit of
      LitInteger i typ
        | Just int  <- toInt i   -> ("LitInteger-int", integer i)
        | Just long <- toInt64 i -> ("LitInteger-int64", integer i)
        | otherwise              -> ("LitInteger-big", integer i)
      MachChar chr               -> ("MachChar", char chr)
      MachInt i                  -> ("MachInt", integer i)
      MachInt64 i                -> ("MachInt64", integer i)
      MachWord i                 -> ("Machword", integer i)
      MachWord64 i               -> ("MachWord64", integer i)
      MachFloat r                -> ("MachFloat", rational r)
      MachDouble r               -> ("MachDouble", rational r)
      MachStr bs                 -> ("MachStr", text (show bs))
      MachNullAddr               -> ("MachNull", text "NUL")
      MachLabel fs mi fORd       -> ("MachLabel", text (unpackFS fs))


instance OutputSyn StgBinding where
  pprSyn bind = 
    case bind of
      StgNonRec id rhs ->
        hang (prefix "StgNonRec") 2 (pprDef id rhs)
              
      StgRec pairs ->
        hang (prefix "StgRec") 2 (vcat $ map (uncurry pprDef) pairs)
        
    where pprDef id rhs =
            pprSyn id <+> equals $+$
            pprSyn rhs


instance OutputSyn StgRhs where
  pprSyn rhs =
    case rhs of
      StgRhsClosure ccs bindInfo fvs updFlag srt args expr
        | null args -> hangIt "THUNK" empty
        | otherwise -> hangIt "FUN" (pprSynList args)
               
        where hangIt objStr argDoc =
                hang (prefix objStr $+$ argDoc) 2
                (pprSyn expr)

      StgRhsCon ccs datacon args
        -> prefix "CONish" <+> pprSyn datacon <+> pprSynList args $+$
           text "Worker/Wrapper:" <+> pprSyn (dataConWrapId datacon)


instance OutputSyn Type where
  pprSyn typ = case typ of
    TyVarTy var -> pprSyn var
    AppTy t1 t2 -> pprSyn t1 <+> pprSyn t2
    TyConApp tc tys -> pprSyn tc <+> hsep (map pprSyn tys)
    FunTy t1 t2 -> pprSyn t1 <+> arrow <+> pprSyn t2
    ForAllTy v t -> text "forall" <+> text (getOccString v) <+> period <+> pprSyn t
    LitTy (NumTyLit i)  -> integer i
    LitTy (StrTyLit fs) -> quotes (text $ unpackFS fs)
    

instance OutputSyn StgExpr where
  pprSyn e =
    case e of
      StgApp id args
        -> prefix "App" <+> pprSyn id <+> hsep (map pprSyn args)
      StgLit lit
        -> prefix "Lit" <+> pprSyn lit
      StgConApp datacon args
        -- Any StgConApp is a 'real' constructor. Use of the wrapper function
        -- shows up as an StgApp.
        -> prefix "ConApp" <+> pprSyn datacon <+> pprSynList args
      StgOpApp op args resT 
        -> prefix "Op" <+> pprSyn op <+> pprSynList args
      StgLam args body 
        -> prefix "Lam" <+> pprSynList args $+$ pprSyn body
      StgCase scrut _ _ bind _ altT alts 
        -> hang (prefix "Case" <+> pprSyn scrut <+> equals <+> pprSyn bind) 2
           (hang (pprSyn altT) 2 (vcat (map pprSyn alts)))
      StgLet bindings body 
        -> pprSynLet "Let" bindings body
      StgLetNoEscape _ _ bindings body 
        -> pprSynLet "LetNE" bindings body
      StgTick _ realExpr
        -> prefix "Tick" <+> pprSyn realExpr


-- | PrettyPrint the bindings and body of the two varieties of Let
-- expressions.
pprSynLet :: String -> StgBinding -> StgExpr -> DocS
pprSynLet pfxStr binds body = hang (prefix pfxStr <+> text "let") 2
                              (pprSyn binds) $+$
                              text "in" <+> pprSyn body


instance OutputSyn StgAlt where
  pprSyn (acon, params, useMask, rhs) =
    case acon of
      DataAlt datacon
        -> makeAlt "DataAlt" (pprStgName datacon <+> hsep (map pprSyn params))
      LitAlt lit
        -> makeAlt "LitAlt" (pprSyn lit)
      DEFAULT
        -> makeAlt "DEFAULT" underscore

    where makeAlt pfx pat = hang (prefix pfx <+> pat) 2 (arrow <+> pprSyn rhs)


instance OutputSyn AltType where
  pprSyn at = case at of
    PolyAlt     -> prefix "PolyAlt"
    UbxTupAlt i -> prefix $ "UbxTupAlt" ++ show i
    AlgAlt _    -> prefix "AlgAlt"
    PrimAlt _   -> prefix "PrimAlt"
    

instance OutputSyn StgOp where
  pprSyn op =
    case op of
      StgPrimOp primop
        -> text $ "(Prim) " ++ occNameString (primOpOcc primop)
      StgPrimCallOp (PrimCall fstring _)
        -> text $ "(PrimCall) " ++ unpackFS fstring
      StgFCallOp fcall _
        -> text $ "(Foreign) " ++ nameForeignCall fcall

