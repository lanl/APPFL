{-# LANGUAGE
NamedFieldPuns, CPP, FlexibleInstances,
LiberalTypeSynonyms, TypeOperators #-}

module FromGHC
  ()
where

-- This may be bad practice, or an indicator of bad design.
-- It is, however, more convenient than manually typing a message
-- for every non-provably exhaustive pattern match
#define _unreachable (error ("Should not be reached: " ++ __FILE__ ++ ":" ++ show __LINE__))

import           ADT
import           AST
import           State hiding (liftM)
import qualified Data.Set as Set

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

                 -- Identifiers/Name
                 , Id -- Var synonym: basicTypes/Var.hs
                 , Name, NamedThing (..)
                 , nameModule
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
import           DataCon
                 ( DataCon
                 , isVanillaDataCon
                 )
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

import           Name ( getOccString, nameModule_maybe )
import           OccName ( occNameString )

import           Var
                 ( Var (..) )

import qualified TyCon as GHC
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
                 (liftM, mapAndUnzipM, zipWithM
                 , (>=>), (<=<), (=<<)
                 )
import           Data.List (nubBy)
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
  foldr xopt_modify newFlags appflExts
 
  where
    
    xopt_modify (xflag, turnOn) dfs
      | turnOn    = xopt_set dfs xflag
      | otherwise = xopt_unset dfs xflag
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
      
      --Should parameterize this
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
    let datacons = filter GHC.isDataTyCon cg_tycons
          
    liftIO $ toStg cg_module cg_binds ms_location datacons hscEnv dflags


-- | Convert a CoreProgram to STG (includes the stg2stg pass,
--   which is where profiling information is added and any IO
--   may occur. 
toStg :: Module             -- this module (being compiled)
      -> CoreProgram        -- its Core bindings
      -> ModLocation        -- Where is it? (I think)
      -> [GHC.TyCon]        -- Its data constructors
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
--   Does not add package information
qualifyName :: NamedThing a => a -> String
qualifyName thing = modPrefix ++ idname
  where name           = getName thing
        modPrefix      = maybe "" makePrefix (nameModule_maybe name)
        makePrefix mod = moduleNameString (moduleName mod) ++ "."
        idname         = getOccString name

-- | PrettyPrint a qualified name for NamedThing
pprSynName :: NamedThing a => a -> SDoc
pprSynName = text . qualifyName . getName

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

-- Handle the bindings and body of the two varieties of Let expressions
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

      
      
-- g2a prefix => GHC to APPFL


-- A TySet holds the set of TyCons-In-Progress.  We only see one
-- data constructor at a time in the STG syntax, so we add them to the
-- Set as we see them, creating a new TyCon if necessary and appending the
-- DataCon to the list of constructors for the TyCon
type TySet = Set.Set TyCon

-- We want a "stateful" traversal, since, once a TyCon is in the TySet,
-- any new DataCon associated with it should be inserted into its
-- constructor list.  If we don't order the traversal, we'd have to merge
-- the TySets intelligently; not impossible, but extra work.
-- Sidenote: I had no idea type variables could be higher-kinded

type TyST stg = State TySet (stg ())

-- This is a littly silly: composition of types. The LiberalTypeSynonyms
-- extension is absolutely required for this
type (:.:) a b c= a (b c)

instance Ord TyCon where
  -- Type constructors names must be unique,
  -- so this is a reasonable instance
  compare (TyCon _ c1 _  _) (TyCon _ c2 _  _)
    = compare c1 c2




g2a :: [StgBinding] -> [Def ()]
g2a binds =
  let (objs, tycons) = runState (mapM g2aObj binds) Set.empty
  in undefined

-- | Translate bindings (let/letrec, including top level) into APPFL Obj types.
g2aObj :: StgBinding -> TyST ([] :.: Obj) -- this is why I wanted type composition
g2aObj bind =
  case bind of
    -- We don't distinguish between recursive bindings and otherwise
    -- at the type level, so no need to do anything special here.
    -- occNames passed to procRhs are probably not what we want, but
    -- serve for now
    StgNonRec id rhs -> procRhs rhs (getOccString id) >>= return . (:[])
    StgRec pairs -> mapM (\(id,rhs) -> procRhs rhs $ getOccString id) pairs

  where
    procRhs :: StgRhs -> String -> TyST Obj
    procRhs rhs name =
      case rhs of
        -- FUN or THUNK
        StgRhsClosure ccs bindInfo fvs updFlag srt args expr

          -- it's a THUNK
          | null args -> do
              e <- g2aExpr expr
              return $ THUNK () e name
          
          -- it's a FUN 
          | otherwise -> do
              e <- g2aExpr expr
              return $ FUN () (map getOccString fvs) e name
          
        -- it's a DataCon definition
        -- Given a datatype: data T <type vars> = T1 <types> | T2 ..
        -- There are top level bindings to ModuleName.{T1,T2...} with
        -- definitions as either:
        -- functions that construct the object: M.T1 = \a b .. -> M.T1 a b ..
        -- or
        -- constants: M.T1 = M.T1
        -- We'll need to chase all these down to build our full TyCons
        StgRhsCon _ dataCon args

          -- DataCon does not have a "fancy" type.
          -- "No existentials, no coercions, nothing" (basicTypes/DataCon.hs)
          | isVanillaDataCon dataCon -> undefined

          -- Something fancy going on with the DataCon, fail hard
          | otherwise         ->
              panic $ showSDocUnsafe $
              (text "Not sure how to handle non-vanilla DataCons like:" $+$ ppr dataCon)
            
            

g2aExpr :: StgExpr -> TyST Expr
g2aExpr = undefined
