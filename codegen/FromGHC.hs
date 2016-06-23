{-# LANGUAGE
NamedFieldPuns #-}

module FromGHC
  ()
where


import           ADT
import           AST


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
                 , ModLocation (..)
                 , getModSummary, getModuleGraph
                 , moduleName
                 
                 )
                 

import           GhcMonad (withSession, liftIO)                   
import           DynFlags (defaultFatalMessager
                          ,defaultFlushOut)

import           HscTypes ( HscEnv (..), CgGuts (..)
                          , isBootSummary
                          )

import           HscMain (hscSimplify)
import           TidyPgm (tidyProgram)
import           CorePrep (corePrepPgm)
import           CoreToStg (coreToStg)
import           CoreSyn (CoreProgram)
import           SimplStg (stg2stg)

import           StgSyn ( StgBinding, GenStgBinding (..)
                        , StgExpr, GenStgExpr (..), StgOp (..)
                        , StgRhs, GenStgRhs (..)
                        , StgAlt, GenStgAlt, AltType (..)
                        , StgLiveVars, GenStgLiveVars (..)
                        , UpdateFlag (..), SRT (..), StgBinderInfo
                        , pprStgBindings)
import           Name ( NamedThing (..)
                      , getOccString)
import           Var (Var (..))

import qualified TyCon as HsTyCon (isDataTyCon, TyCon)

import           CostCentre (CollectedCCs)
import           Outputable
import           Control.Monad (liftM, mapAndUnzipM
                               , (>=>), (<=<), (=<<))


testDir = "../test/haskell/"
target f = testDir ++ f

main = compileTarget $ target "Fibo.hs"


compileTarget fileName = do

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
      let dflags = oldFlags
            -- hscTarget determines what form the backend takes (Native,
            -- LLVM, etc.)  We don't want any code generation but our own,
            -- so HscNothing is appropriate
            { hscTarget = HscNothing
            -- When using HscNothing, Linking appears to only happen
            -- if GHC is being used as an interface to ld (e.g. if a
            -- .o file is given as an argument) or if explicitly
            -- requested with flags.  Linking generally means writing
            -- files to disk, so it's turned off for now.
            , ghcLink = NoLink
            --Should parameterize this
            , verbosity = 0}

      setSessionDynFlags dflags
      -- construct (and add) a Target from the given file name.  The
      -- file's extension (and potentially some of the DynFlags)
      -- determine how the file is to be processed, i.e. which Phase
      -- of the compilation to start at.  For APPFL, this will
      -- probably always be *.hs files, so we may want to error on
      -- other inputs (as opposed to letting GHC try to do something
      -- with it)
      guessTarget fileName Nothing >>= addTarget

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

      let
        gutsToSTG CgGuts{ cg_module -- :: Module
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
            let datacons = filter HsTyCon.isDataTyCon cg_tycons
            liftIO $ toStg cg_module cg_binds ms_location datacons hscEnv dflags

        
        toStg :: Module -- this module
              -> CoreProgram -- its Core bindings
              -> ModLocation -- Where is it? (I think)
              -> [HsTyCon.TyCon] -- Its data constructors
              -> HscEnv -- The compiler session
              -> DynFlags -- Dynamic flags
              -> IO ([StgBinding], CollectedCCs)
        toStg mod core modLoc datacons hscEnv dflags = 
          do
            prepped <- corePrepPgm hscEnv modLoc core datacons
            stg_binds <- coreToStg dflags mod prepped

            -- Not clear if this is necessary.
            -- It may only do cost center analysis, which we don't use for now
            stg2stg dflags mod stg_binds

      (stg, ccs) <- mapAndUnzipM gutsToSTG cgGuts
      dflags <- getSessionDynFlags
      return $ showSDoc dflags (pprStgBindings (concat stg))
  
      

-- | Translate bindings (let/letrec, including top level) into APPFL Obj types.
g2aObj :: StgBinding -> [Obj ()]
g2aObj bind =
  case bind of
    -- We don't distinguish between recursive bindings and otherwise
    -- at the data level, so no need to do anything special here.
    -- occNames passed to procRhs are probably not what we want, but
    -- serve for now
    StgNonRec id rhs -> [procRhs rhs $ getOccString id]
    StgRec pairs -> [procRhs rhs $ getOccString id | (id,rhs) <- pairs]

  where
    procRhs rhs name =
      case rhs of
        -- FUN or THUNK
        StgRhsClosure ccs bindInfo fvs updFlag srt args expr
          -- it's a THUNK
          | null args -> THUNK () (g2aExpr expr) name
          
          -- it's a FUN 
          | otherwise -> FUN () (map getOccString fvs) (g2aExpr expr) name
          
        -- CON type, I think
        StgRhsCon ccs dataCon args
          -> undefined


g2aExpr = undefined


