{-# LANGUAGE
NamedFieldPuns, CPP, FlexibleInstances,
LiberalTypeSynonyms, TypeOperators, BangPatterns, MagicHash #-}

module FromGHC
where

-- This may be bad practice, or an indicator of bad design.
-- It is, however, more convenient than manually typing a message
-- for every non-provably exhaustive pattern match
#define _unreachable (error ("Should not be reached: " ++ __FILE__ ++ ":" ++ show __LINE__))
#define _TODO (error ("Definition incomplete at" ++ __FILE__ ++ ":" ++ show __LINE__))

import           ADT as A hiding (unfoldr)
import           AST as A
import           State hiding (liftM)
import qualified Data.Map as Map

-- Let's be explicit about what comes from where, and what it's related to.
-- It's hard enough dealing with the massive GHC codebase
import           GHC.Paths (libdir)
import           GHC
                 -- These categorizations are ripped directly from GHC.hs

                 -- Initialization
                 ( defaultErrorHandler

                 -- GHC Monad
                 , GhcMonad (..), runGhc

                 -- DynFlags and settings
                 , getSessionDynFlags, setSessionDynFlags
                 , DynFlags (..), HscTarget (..), GhcLink (..)

                 -- Targets
                 , guessTarget, addTarget

                 -- Loading/compiling
                 , load, LoadHowMuch (..)
                 , depanal
                 , parseModule, typecheckModule, desugarModule, coreModule

                 -- Inspecting the module structure of the program
                 , ModuleGraph, ModSummary(..), Module
                 , ModLocation (..), ModuleName
                 , getModSummary, getModuleGraph
                 , moduleName, moduleNameString
                 )                 

import           GhcMonad
                 ( withSession, liftIO )   
import           DynFlags
                 ( ExtensionFlag (..)
                 , defaultFatalMessager
                 , defaultFlushOut
                 , xopt_set, xopt_unset -- for LANGUAGE Extensions
                 )

import           HscTypes
                 ( HscEnv (..), CgGuts (..)
                 , isBootSummary
                 )

import           HscMain
                 ( hscSimplify )
import           TidyPgm
                 ( tidyProgram )
import           CorePrep
                 ( corePrepPgm )
import           CoreToStg
                 ( coreToStg )
import           CoreSyn
                 ( CoreProgram, AltCon (..) )
import           DataCon as G
                 ( DataCon
                 , isVanillaDataCon, dataConTag
                 , dataConTyCon, dataConRepType
                 )
import           TypeRep (Type (..))                 
import           SimplStg
                 ( stg2stg )

import           StgSyn
                 ( StgBinding, GenStgBinding (..)
                 , StgExpr, GenStgExpr (..), StgOp (..)
                 , StgRhs, GenStgRhs (..)
                 , StgArg, GenStgArg (..)
                 , StgAlt, GenStgAlt, AltType (..)
                 , StgLiveVars, GenStgLiveVars
                 , UpdateFlag (..), SRT (..), StgBinderInfo
                 , pprStgBindings
                 )

import           Name
                 ( Name, NamedThing (..)
                 , getOccString, nameModule_maybe, nameUnique
                 , nameModule )
                 
import           OccName ( occNameString )
import           FastTypes
                 ( FastInt
                 , shiftRLFastInt, bitAndFastInt
                 , iBox, cBox, fastChr
                 )
import           Unique ( Unique, getKey )

import           GHC.Exts
                 ( Int(..), indexCharOffAddr# )

import           Id ( Id, idName, idUnique )

import           TyCon as G
                 ( TyCon, isDataTyCon )

import           Literal ( Literal (..) )
import           PrimOp
                 ( PrimOp (..), PrimCall (..)
                 , primOpOcc )
import           ForeignCall
                 ( ForeignCall (..), CCallSpec (..)
                 , CCallTarget (..) )

import           CostCentre
                 ( CollectedCCs )

import           Outputable
import           FastString ( unpackFS )
import           Control.Monad
                 (liftM, mapAndUnzipM, zipWithM, when
                 , (>=>), (<=<), (=<<)
                 )
import           Control.Exception ( assert )
import           Data.List as List (nubBy, unfoldr, insertBy) 
import           Data.Function (on)


testDir = "../test/haskell/"
target f = testDir ++ f

-- For in-progress tests.
testPreludeDir = "../prelude/"



putStgSyn file = compileAndThen stgSynString testPreludeDir file >>= putStrLn . snd
writeStgSyn infile outfile =
  compileAndThen stgSynString testPreludeDir infile >>= writeFile outfile . snd
  

stgSynString stg = do
  dynflags <- getSessionDynFlags
  let sdoc = vcat (map pprSyn stg)
      text = showSDoc dynflags sdoc
  return text

compileAndThen stgFun preludeDir file = do

  -- Install an error handler with functions for printing errors and flushing
  -- the stdout stream
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do

    -- Enter the GHC monad, giving it the directory "where GHC's library files
    -- reside."  See [Note topdir] in compiler/main/SysTools.hs in the GHC repo
    runGhc (Just libdir) $ do

      -- Need to modify the default DynFlags before doing anything.
      -- Default flags are defined as a function of a Settings object in
      -- compiler/main/DynFlags.hs
      -- The initialization process creates this Settings object in
      -- compiler/main/SysTools.hs:initSysTools
      
      oldFlags <- getSessionDynFlags
      let dflags = makeAppflFlags preludeDir oldFlags


      setSessionDynFlags dflags
      -- construct (and add) a Target from the given file name.  The
      -- file's extension (and potentially some of the DynFlags)
      -- determine how the file is to be processed, i.e. which Phase
      -- of the compilation to start at.  For APPFL, this will
      -- probably always be *.hs files, so we may want to error on
      -- other inputs (as opposed to letting GHC try to do something
      -- with it)
      guessTarget file Nothing >>= addTarget

      -- GHC.load normally initiates most of the heavy lifting for the
      -- compiler.  When the hscTarget is HscNothing (as it is here),
      -- this only incurs module dependency analysis, parsing,
      -- typechecking and the construction of an interface (ModIface).
      -- With -fwrite-interface, *.hi file(s) may be written to
      -- disk. We could turn that off explicitly if necessary.
      load LoadAllTargets
      -- It's not (yet) clear if it's strictly necessary to use the
      -- 'load' function.  There's a lot going on in there.
      -- If GHC.compileCore is any indicator, it *is* important:
      --  it calls 'load' and then parses and typechecks again before
      --  producing a CoreModule (not exactly what we want)


      modGraph <- getModuleGraph

      
      -- pull the ModGuts out of the parsed -> typechecked ->
      -- desugared modules. The ModGuts objects should have a
      -- CoreProgram (and other data relevant to a program's Core
      -- representation)
      modGuts <- mapM (liftM coreModule .
                      -- Left-fish operator (as I call it) composes
                      -- monadic operations right to left (as in (.) )
                      desugarModule <=< typecheckModule <=< parseModule)
              -- I don't think we'll see .hs-boot files, but let's be
              -- safe and filter them out
              . filter (not . isBootSummary) $ modGraph


      -- Now we simplify the core.  This is where transformations and
      -- optimizations happen, so we definitely want this.
      hscEnv <- getSession

      -- CgGuts are a reduced form of ModGuts. They hold information necessary
      -- for STG translation
      -- Not sure if ModDetails are needed for anything at the moment
      (cgGuts, modDetails) <-
        mapAndUnzipM (liftIO . tidyProgram hscEnv <=< liftIO . hscSimplify hscEnv) modGuts

      (nestedStg, ccs) <- mapAndUnzipM gutsToSTG cgGuts
      dflags     <- getSessionDynFlags
      let stg = concat nestedStg
      res <- stgFun stg
      return $ (stg, res)

makeAppflFlags :: String -> DynFlags -> DynFlags
makeAppflFlags preludeDir
  flags@DynFlags
  { extensions  = oldExts
  , importPaths = oldIPaths }
  =
  foldr xoptModify newFlags appflExts
 
  where    
    xoptModify :: (ExtensionFlag, Bool) -> DynFlags -> DynFlags
    xoptModify (xflag, turnOn) flags
      | turnOn    = xopt_set flags xflag
      | otherwise = xopt_unset flags xflag

    newFlags =
      flags {
      -- hscTarget determines what form the backend takes (Native,
      -- LLVM, etc.)  We don't want any code generation but our own,
      -- so HscNothing is appropriate
      hscTarget      = HscNothing
    
      -- When using HscNothing, Linking appears to only happen
      -- if GHC is being used as an interface to ld (e.g. if a
      -- .o file is given as an argument) or if explicitly
      -- requested with flags.  Linking generally means writing
      -- files to disk, so it's turned off for now.
      , ghcLink        = NoLink

      -- Make sure our Prelude and Base libs are visible
      , importPaths    = oldIPaths ++ [preludeDir, preludeDir ++ "/APPFL/" ]
      
      -- Should parameterize this (maybe with Options.h?)
      , verbosity      = 0
      }

-- List of language extensions we want to require of Appfl-haskell
-- source files.    
appflExts :: [(ExtensionFlag, Bool)]
appflExts =
  [
    -- We want to enforce the use of our Prelude to have a better
    -- chance of getting full STG programs from GHC
    (Opt_ImplicitPrelude  , False)

    -- We want to escape as much of the built-in stuff as possible,
    -- so we'll use our own "fromInteger", "ifthenelse" etc.
    -- This actually implies NoImplicitPrelude; I'm just being extra explicit 
    -- see the GHC Syntactic Extensions docs or the definitions in APPFL.Base
  , (Opt_RebindableSyntax , True)
  ]


-- | In the GhcMonad, given a CgGuts object, pull out the
--   CoreProgram and convert it to STG
gutsToSTG :: GhcMonad m => CgGuts -> m ([StgBinding], CollectedCCs)      
gutsToSTG CgGuts { cg_module -- :: Module
                 , cg_binds  -- :: CoreProgram
                 , cg_tycons -- :: [TyCon]
                 }
  =
  do
    ModSummary{ms_location} <- getModSummary $ moduleName cg_module
      -- As far as I know, it's entirely possible that the session
      -- and its DynFlags have been modified since they were bound
      -- above.  Grab them again here for safety.
    hscEnv <- getSession
    dflags <- getSessionDynFlags
    let datacons = filter G.isDataTyCon cg_tycons
          
    liftIO $ toStg cg_module cg_binds ms_location datacons hscEnv dflags


-- | Convert a CoreProgram to STG (includes the stg2stg pass,
--   which is where profiling information is added and any IO
--   may occur. 
toStg :: Module             -- this module (being compiled)
      -> CoreProgram        -- its Core bindings
      -> ModLocation        -- Where is it? (I think)
      -> [G.TyCon]          -- Its data constructors
      -> HscEnv             -- The compiler session
      -> DynFlags           -- Dynamic flags
      -> IO ([StgBinding],  -- The STG program
              CollectedCCs) -- Cost centres (don't care about these)
toStg mod core modLoc datacons hscEnv dflags = 
  do
    prepped   <- corePrepPgm hscEnv modLoc core datacons

    --    core2stg, by inspection, does not produce IO, despite its type
    stg_binds <- coreToStg dflags mod prepped

    -- Not clear if this is necessary.
    -- It may only do cost center analysis, which we don't use for now
    stg2stg dflags mod stg_binds


      
-- g2a prefix => GHC to APPFL


-- A TyMap holds the information required to build TyCons (TyCon name
-- -> (GHC)DataCon).  We only see one data constructor at a time in
-- the STG syntax, so we add them to the Map as we see them. Once
-- accumulated, the DataCons can be translate to APPFL DataCon.  This
-- translation does not happen immediately because we want to preserve
-- the ordering of constructors in the data definition (based on the
-- ConTag field of the GHC DataCons).  This tag is occasionally used by
-- GHC (in deriving Enum instances, in particular) and we don't want
-- to break that logic.
type TyMap = Map.Map A.Con [G.DataCon]

-- We want a "stateful" traversal, since, once a TyCon is in the TySet,
-- any new DataCon associated with it should be inserted into its
-- constructor list.  If we don't order the traversal, we'd have to merge
-- the TySets intelligently; not impossible, but extra work.
-- Sidenote: I had no idea type variables could be higher-kinded

type TyST stg = State TyMap (stg ())


-- This is a littly silly: composition of types. The LiberalTypeSynonyms
-- extension is absolutely required for this to be partially applied
-- (for example, for use in the 'stg' parameter of TyST above)
type (:.:) a b c = a (b c)


instance Ord A.TyCon where
  -- Type constructors names must be unique,
  -- so this is a reasonable instance for the Map requirement.
  -- Further, we'll be inserting DataCons into the Tycons as we
  -- encounter them, and we don't want the list-in-progress to change
  -- how the TyCons are interpreted by the Set operations
  compare (A.TyCon _ c1 _  _) (A.TyCon _ c2 _  _)
    = compare c1 c2




-- putDataCon :: G.DataCon -> ?
putDataCon dc =
  do
    let tcname  = qualifyName (dataConTyCon dc)        
    tymap <- get
    when (not $ dcPresent dc tymap) $ do
      put $ Map.insertWith insertDC tcname [dc] tymap
      

  where
    insertDC [new] dcs = List.insertBy (compare `on` dataConTag) new dcs
    insertDC _ _       = _unreachable
    
dcPresent :: G.DataCon -> TyMap -> Bool
dcPresent dc tmap = any (any (== dc)) tmap
  
-- | Merge two incomplete TyCons.  TyCons are incomplete
-- because their data constructors are added as they are encountered
-- in the STG tree.  The merging is a simple union of the TyCon TyVars and
-- DataCons. Note: the tyvar union might have to be tweaked, depending on how
-- GHC maintains the types of *its* DataCons    

g2aDataCon :: G.DataCon -> A.DataCon
g2aDataCon dc = DataCon con mtypes
  where con    = qualifyName dc
        mtypes = go (g2aMonoType $ dataConRepType dc)
        go (MFun m1 m2) = m1 : go m2
        go m            = [m]

-- | Convert a GHC Type to an Appfl Monotype
--   for use in DataCons
g2aMonoType :: Type -> Monotype
g2aMonoType t =
  case t of
    -- Simplest case, a type variable
    TyVarTy v        -> MVar (getOccString v)

    -- AppTy will never be an application of a TyConnApp to
    -- some other type.  The first param is always another AppTy or
    -- a TyVarTy.  This seems like it would show up in contexts like
    -- f :: Monad m => m a -> m b.  In Haskell, this is fine, but there are
    -- no typeclasses or synonyms at the STG level, so this type may have
    -- been excised/simplified by the time we start touching the STG tree
    AppTy t1 t2      ->
      case g2aMonoType t1 of
        MVar v        -> MCon True v [g2aMonoType t2]
        MCon b c args -> MCon b c (args ++ [g2aMonoType t2])
        -- Anything else would be strange, but let's be sure
        _           -> panicType

    -- e.g. List a, Maybe Int, etc.
    TyConApp tc args -> MCon True -- Assuming everything boxed for now
                        (qualifyName tc) (map g2aMonoType args)

    -- e.g a -> a
    -- This assumes right-associativity is expressed just as in our MFun,
    -- which is probably reasonable. Anything else would be counterintuitive
    FunTy t1 t2      -> MFun (g2aMonoType t1) (g2aMonoType t2)

    -- If this is nested in a type (as in higher-rank types) just ignoring the
    -- universal quantifier may be a problem.  We'll see...
    ForAllTy var ty  -> g2aMonoType ty

    -- Type-level Literals are definitely beyond what we support.
    -- They *should* only appear in programs with -XDataKinds,
    -- so they could be rejected early, in theory.
    LitTy _          -> panicType

  where panicType = panic $ showSDocUnsafe $
                    text "Having trouble converting Type to MonoType:" <+> ppr t



-- TODO: Sanitize names for use in C codegen
makeStgName :: Id -> String
makeStgName = qualifyName


g2a :: [StgBinding] -> [Def ()]
g2a binds =
  let (objs, tycons) = runState (mapM g2aObj binds) Map.empty
  in _TODO

-- | Translate bindings (let/letrec, including top level) into APPFL Obj types.
g2aObj :: StgBinding -> TyST ([] :.: Obj) -- this is why I wanted type composition
g2aObj bind =
  case bind of
    -- We don't distinguish between recursive bindings and otherwise
    -- at the type level, so no need to do anything special here.
    -- occNames passed to procRhs are probably not what we want, but
    -- serve for now
    StgNonRec id rhs -> procRhs rhs id >>= return . (:[])
    StgRec pairs -> mapM (\(id,rhs) -> procRhs rhs id) pairs

  where
    procRhs :: StgRhs -> Id -> TyST Obj
    procRhs rhs id =
      case rhs of
        -- FUN or THUNK
        StgRhsClosure ccs bindInfo fvs updFlag srt args expr

          -- it's a THUNK
          | null args
            -> do
              e <- g2aExpr expr
              return $ THUNK () e (makeStgName id)

          -- it's a FUN(ish) thing.  Data constructors are functions too
          -- but they are not given a distinguished definition as in APPFL
          -- STG or full Haskell. This is fine; we can leave function wrappers
          -- for the CONs, and only use them when constructors are 
          | otherwise
            -> do
              e <- g2aExpr expr
              return $ FUN () (map makeStgName args) e (makeStgName id)
          
        -- It's either a top-level empty constructor (a la Nil/Nothing) or it's
        -- being used in a let binding. Either way, we make sure it's accumulated
        StgRhsCon _ dataCon args

          -- DataCon does not have a "fancy" type.
          -- "No existentials, no coercions, nothing" (basicTypes/DataCon.hs)
          | isVanillaDataCon dataCon
            -> do
              putDataCon dataCon
              return _TODO
              
              

          -- Something fancy going on with the DataCon, fail hard until we figure
          -- out how to handle it.
          | otherwise         ->
              panic $ showSDocUnsafe $
              (text "Not sure how to handle non-vanilla DataCons like:" $+$ ppr dataCon)


g2aExpr :: StgExpr -> TyST Expr
g2aExpr = _TODO



-- Custom Pretty Printer for better inspection of STG tree.
-- This will make it easier to figure out what Haskell values
-- correspond to what STG code (the ddump-stg option is not fantastic)
class OutputSyn a where
  pprSyn :: a -> SDoc


-- Modify this to make the printing a little less text-y
verbosePrint = True

prefix :: String -> SDoc
prefix | verbosePrint = parens . text
       | otherwise    = const empty

-- | PrettyPrint a list of things in OutputSyn class
pprSynList :: OutputSyn a => [a] -> SDoc
pprSynList = brackets . sep . punctuate comma . map pprSyn

-- | Produce a qualified name for a NamedThing as a String. (e.g. Module.Submodule.idname)
--   Does not add package information, but does add characters to express the underlying
--   Unique for Names that don't have a Module. (Internal/System names)
qualifyName :: NamedThing a => a -> String
qualifyName thing = qualify idname
  where name           = getName thing
        qualify n      = case nameModule_maybe name of
                              Just m  -> makePrefix m ++ n
                              Nothing -> n ++ '!':showUnique (nameUnique name)
        modPrefix      = maybe "" makePrefix (nameModule_maybe name)
        makePrefix mod = moduleNameString (moduleName mod) ++ "."
        idname         = getOccString name


-- This is based on the (uglier, imo) base62 encoding from basicTypes/Unique.hs
-- Because this is a base64 encoding (with '-' and '_'), it needs to be sanitized
-- for C codegen.  There's other sanitization to be done on non-uniquified names,
-- so it's deferred for now.
showUnique :: Unique -> String
showUnique u = unfoldr op (getKey u)
  where chr64 n    = cBox ( indexCharOffAddr# chars64 n)
        !chars64   = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"#
        op (I# 0#) = Nothing
        op (I# i#) = case i# `shiftRLFastInt` 6# {- 2^6 = 64 -} of
                       shifted# ->
                         case i# `bitAndFastInt` 63# of
                           low5# -> Just ( chr64 low5#, iBox shifted# )                
          


-- | PrettyPrint a qualified name for NamedThing
pprSynName :: NamedThing a => a -> SDoc
pprSynName = text . qualifyName . getName


-- | Make a string representation of a foreign call
nameForeignCall :: ForeignCall -> String
nameForeignCall (CCall (CCallSpec target _ _))
  = case target of
      StaticTarget fstring _ _ -> unpackFS fstring
      DynamicTarget            -> "Dynamic:Unnamed"



instance OutputSyn Id where
  pprSyn = pprSynName


instance OutputSyn StgArg where
  pprSyn (StgVarArg id)  = pprSyn id
  pprSyn (StgLitArg lit) = pprSyn lit


instance OutputSyn Literal where
  pprSyn = ppr


instance OutputSyn StgBinding where
  pprSyn bind = 
    case bind of
      StgNonRec id rhs ->
        hang (prefix "StgNonRec") 2 (pprPair id rhs)
              
      StgRec pairs ->
        hang (prefix "StgRec") 2 (vcat [pprPair id rhs | (id, rhs) <- pairs])
        
    where pprPair id rhs =
            pprSynName id <+> equals $+$
            pprSyn rhs


instance OutputSyn StgRhs where
  pprSyn rhs =
    case rhs of
      StgRhsClosure ccs bindInfo fvs updFlag srt args expr
        | null args -> hangIt "THUNK" empty
        | otherwise -> hangIt "FUN" (pprSynList args)
               
        where hangIt objStr argDoc =
                hang (prefix objStr <+> text "Upd:" <+> ppr updFlag $+$ argDoc) 2
                (pprSyn expr)

      StgRhsCon ccs datacon args
        -> prefix "CONish" <+> pprSynName datacon <+> pprSynList args
                

instance OutputSyn StgExpr where
  pprSyn e =
    case e of
      StgApp id args
        -> prefix "App" <+> pprSynName id <+> hsep (map pprSyn args)
      StgLit lit
        -> prefix "Lit" <+> pprSyn lit
      StgConApp datacon args
        -> prefix "ConApp" <+> pprSynName datacon <+> pprSynList args
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


-- Prettyprint the bindings and body of the two varieties of Let expressions
pprSynLet :: String -> StgBinding -> StgExpr -> SDoc
pprSynLet pfxStr binds body = hang (prefix pfxStr <+> text "let") 2
                              (pprSyn binds) $+$
                              text "in" <+> pprSyn body



instance OutputSyn StgAlt where
  pprSyn (acon, params, useMask, rhs) =
    case acon of
      DataAlt datacon
        -> makeAlt "DataAlt" (pprSynName datacon <+> hsep (map pprSyn params))
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

      