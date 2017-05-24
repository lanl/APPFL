{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE CPP #-}
#include "../options.h"

module Driver (
  CodegenInput (..),
  tokenizer,
  parser,
  renamer,
  defaultcaser,
  freevarer,
  infotaber,
  conmaper,
  typechecker,
  knowncaller,
  heapchecker,
  tctest,
  tester,
  printObjsVerbose,
  codegener,
  pprinter,
  mhsTokenizer,
  mhsParser,
  mhsPup,
  mhsSigSetter,
  mhsRenamer,
  mhsGenFunctioner,
  mhsExpSimpler,
  mhsPatSimpler,
  mhsNumBoxer,
  mhsSTGer
) where

import qualified MHS.AST
import qualified MHS.Parser
import qualified MHS.Transform
import qualified MHS.Tokenizer
import qualified MHS.ToSTG

import           Tokenizer
import           Parser
import           DupCheck
import           Rename
import           ADT
import           AST
import           SCC
import           CodeGen
import           CMap
import           Analysis
import           PPrint
import           InfoTab
import           HeapObj
import           SetFVs
import           BU
import           HMStg
import           OrderFVsArgs
import           Data.List
import qualified Data.Set as Set
import           System.IO
import Language.C.Quote.GCC
import Language.C.Syntax (Definition)
import qualified Text.PrettyPrint.Mainland as PP



type ParsedPrgm   = ([TyCon], [Obj ()], [(Var, Monotype)])
type StgPrgm a    = ([TyCon], [Obj a], Assumptions)
type TypedPrgm    = ([TyCon], [Obj InfoTab])
type CGReadyPrgm  = ([TyCon], [Obj InfoTab])
type FVMeta       = ([Var], [Var])                  

data CodegenInput
  = StgSource String
  | MhsSource String
--  | StgParsed ([TyCon], [Obj ()])
  | StgParsed ParsedPrgm
--  | MhsTransformed ([TyCon], [Obj ()], Assumptions)
  | MhsTransformed (StgPrgm ())
  | GhcTransformed ([TyCon], [Obj ()])



tester :: (String -> a) -> (a -> String) -> [FilePath] -> IO ()
tester tfun sfun infiles =
 do
   ihandles <- mapM (flip openFile ReadMode) infiles
   _tester tfun sfun ihandles stdout

_tester :: (String -> a) -> (a -> String) -> [Handle] -> Handle -> IO ()
_tester testfun showfun ifds ofd =
  do
    inp <- mapM hGetContents ifds
    let res = testfun (concat inp)
        out = showfun res
    hPutStrLn ofd out
    return ()
    
header :: Definition
header = [cedecl| $esc:("#include \"stgc.h\"")|]

footer :: Bool -> [Definition]
footer v  = [cgStart, cgMain v]

-- nameDefs
--  :: [([Char], Obj)] ->
--     [([Char], [Char])] ->
--     State [[Char]] [([Char], Obj)]


-- need a better way, such as a built-ins prelude for par
stgRTSGlobals :: [String]
stgRTSGlobals = [ "par",                      -- source
                  "stg_case_not_exhaustive",  -- before type checking
                  "stg_case_not_exhaustiveP", -- during codegen
                  "stg_case_not_exhaustiveN"  -- during codegen
                ] ++ map fst primOpTab -- from AST.hs


-- Tokenizes input, stripping comments and handling the layout rule
-- (roughly)
mhsTokenizer :: String -> [MHS.Tokenizer.Token]
mhsTokenizer inp = MHS.Tokenizer.tokenize inp

-- parse tokenized input
-- checks for valid syntax
mhsParser :: String -> [MHS.AST.Defn]
mhsParser inp = let toks = mhsTokenizer inp
             in MHS.AST.intCon :
                MHS.AST.dblCon :
                MHS.Parser.parse toks


-- parse unparse parse
mhsPup :: String -> [MHS.AST.Defn]
mhsPup = mhsParser . show . unparse . mhsParser


-- Associate type signatures with objects
mhsSigSetter :: String -> [MHS.AST.Defn]
mhsSigSetter inp = let defs = mhsParser inp
                    in MHS.Transform.setTypeSignatures defs



-- Rename all user objects uniquely
mhsRenamer :: String -> [MHS.AST.Defn]
mhsRenamer inp = let defs = mhsSigSetter inp
                 in MHS.Transform.renameIDs defs


-- Generate functions in place of Constructors and Primops
mhsGenFunctioner :: String -> [MHS.AST.Defn]
mhsGenFunctioner inp = let defs = mhsRenamer inp
                       in MHS.Transform.genFunctions defs


-- simplify expression application with let bindings
-- All application must be atomic after this
mhsExpSimpler :: String -> [MHS.AST.Defn]
mhsExpSimpler inp = let defs = mhsGenFunctioner inp
                   in MHS.Transform.simplifyExps defs


-- Apply built-in Int constructor to all literal Ints in
-- expressions and patterns
mhsNumBoxer :: String -> [MHS.AST.Defn]
mhsNumBoxer inp = let defs = mhsExpSimpler inp
                  in MHS.Transform.boxNumLiterals defs

-- Make all pattern matching in functions and case expressions
-- simple (non-nested) case expressions.
mhsPatSimpler :: String -> [MHS.AST.Defn]
mhsPatSimpler inp = let defs = mhsNumBoxer inp
                    in MHS.Transform.simplifyPats defs

mhsSTGer :: String -> ([TyCon], [Obj ()], Assumptions)
mhsSTGer inp = let defs = mhsPatSimpler inp
               in MHS.ToSTG.makeSTG defs

mhsPupSTG inp = let (ts, os, as) = mhsSTGer inp
                    code = show (unparse ts $+$ unparse os)
                in parser code


-- strip comments, tokenize input
-- includes basic checking for valid characters and character strings
tokenizer :: String -> [Token]
tokenizer = tokenize

-- parse tokenized input
-- checks for valid syntax
-- parser :: String -> ([TyCon], [Obj ()], [(Var, Monotype)])
parser :: String -> ParsedPrgm
parser = parse . tokenizer

--checks for duplicates
-- dupChecker ::  String -> ([TyCon], [Obj ()], [(Var, Monotype)])
dupChecker ::  String -> StgPrgm ()
dupChecker = dupCheck . parser

-- set boxity in Monotypes of TyCon DataCons
boxer :: String -> StgPrgm ()
boxer inp = let (tycons, objs, typesigs) = dupChecker inp
                (tycons', typesigs') = boxMTypes (tycons, typesigs)
            in (tycons', objs, typesigs')


renamer :: String -> StgPrgm ()
renamer inp = let (tycons, objs, typesigs) = boxer inp
              in (tycons, renameObjs objs, typesigs)


-- generate default cases in Alts blocks that need them
-- The ordering here is important to allow the freevarer and
-- infotaber steps to properly handle the newly generated ADef
-- objects
-- might be worth starting to pass CMap around starting here
-- (currently generated again when setting CMaps in InfoTabs)
defaultcaser :: Bool -> String -> ([TyCon], [Obj ()],  Assumptions)
defaultcaser mhs inp = if mhs
                       then mhsSTGer inp
                       else let (tycons, objs, typesigs) = renamer inp
                            in (tycons, exhaustCases (toCMap tycons) objs, typesigs)

freevarer :: Bool -> String -> ([TyCon], [Obj ([Var],[Var])], Assumptions)
freevarer mhs inp = let (tycons, objs, typesigs) = defaultcaser mhs inp
                in (tycons, setFVsObjs stgRTSGlobals objs, typesigs)

infotaber :: Bool -> String -> ([TyCon], [Obj InfoTab], Assumptions)
infotaber mhs inp = let (tycons, objs, typesigs) = freevarer mhs inp
                in (tycons, setITs objs :: [Obj InfoTab], typesigs)

conmaper :: Bool -> String -> ([TyCon], [Obj InfoTab], Assumptions)
conmaper mhs inp = let (tycons, objs, typesigs) = infotaber mhs inp
                       (tycons', objs') = setCMaps tycons objs
                    in (tycons', objs', typesigs)

typechecker :: Bool -> String -> ([TyCon], [Obj InfoTab])
typechecker mhs inp = let (tycons, objs, typesigs) = conmaper mhs inp
                      in (tycons, if mhs then hmstgAssums objs typesigs else
                                              hmstgAssums objs typesigs)

orderfvsargser :: Bool -> String -> ([TyCon], [Obj InfoTab])
orderfvsargser mhs inp = let (tycons, objs) = typechecker mhs inp
                         in (tycons, orderFVsArgs objs)

knowncaller ::  Bool -> String -> ([TyCon], [Obj InfoTab])
knowncaller mhs inp  = let (tycons, objs) = orderfvsargser mhs inp
                       in (tycons, propKnownCalls objs)

heapchecker :: Bool -> String -> ([TyCon], [Obj InfoTab])
heapchecker mhs inp  = let (tycons, objs) = knowncaller mhs inp
                       in (tycons, setHeapAllocs objs)

printObjsVerbose :: ([TyCon], [Obj InfoTab]) -> IO ()
printObjsVerbose (tycons, objs) = print $ objListDoc objs



mhsTCTest filepath =
  do
    src <- readFile filepath
    let (ts,os,as) = mhsSTGer src
        os1 = setITs $
              setFVsObjs stgRTSGlobals $
              exhaustCases (toCMap ts) os
        (tsF, os2) = setCMaps ts os1
    hmstgAssumsdebug os2 as


tctest mhs arg =
  do
    ifd <- openFile arg ReadMode
    source <- hGetContents ifd
    let (tycons, objs, typesigs) = conmaper mhs source
    hmstgdebug objs


codegener :: CodegenInput -> Bool -> [Definition]
codegener inp v =
  let (tycons, objs) = case inp of
        StgSource src         -> fromStgSource src
        StgParsed parsed      -> fromParsed parsed
        MhsSource src         -> fromMhsSource src
        MhsTransformed mhsStg -> fromMhsTransform mhsStg
        GhcTransformed ghcStg -> let (ts,os) = ghcStg in fromGhcTransform (ts,os,[])
                           
      typeEnums = showTypeEnums tycons
      infotab = showITs objs
      (shoForward, shoDef, shoTable) = showSHOs objs
      (funForwards, funDefs) = cgObjs objs stgRTSGlobals
      -- (stgStatObjCount, stgStatObjTable) = shos objs
      defs =  header : funForwards ++
              typeEnums ++
              infotab ++
              shoForward ++
              shoDef ++
              -- [ stgStatObjCount, stgStatObjTable ] ++
              shoTable ++
              concat funDefs ++
              footer v
  in [cunit|$edecls:defs |]

pprinter :: [Definition] -> String
pprinter = PP.pretty 80 . PP.ppr


-- parse minihaskell in a file, add a block comment at the end
-- showing the unparsed STG code
addSTGComment :: FilePath -> IO ()
addSTGComment filename =
  do
    prld <- readFile "../prelude/Prelude.mhs"
    src <- readFile filename
    let (ts,os) = heapchecker True (src ++ prld)
        stg = show $ bcomment (unparse ts $+$ unparse os)
    length src `seq` writeFile filename (src ++ stg)

--------------------------------------------------------------------------------
--  Composable versions of some of the above stages
-- 
--   These are handy when you want to plug in at a later stage, as is the
--   case with the STG from GHC

{-
type ParsedPrgm   = ([TyCon], [Obj ()], [(Var, Monotype)])
type StgPrgm a    = ([TyCon], [Obj a], Assumptions)
type TypedPrgm    = ([TyCon], [Obj InfoTab])
type CGReadyPrgm  = ([TyCon], [Obj InfoTab])
type FVMeta       = ([Var], [Var])                  
-}

tokC                        :: String          -> [Token]
parseC                      :: [Token]         -> ParsedPrgm
dupCheckC                   :: ParsedPrgm      -> StgPrgm ()
boxC, renameC, exhaustCaseC :: StgPrgm ()      -> StgPrgm ()
freevarC                    :: StgPrgm ()      -> StgPrgm FVMeta
infotabC                    :: StgPrgm FVMeta  -> StgPrgm InfoTab
conmapC                     :: StgPrgm InfoTab -> StgPrgm InfoTab
typecheckC                  :: StgPrgm InfoTab -> TypedPrgm
orderfvsC, knowncallC       :: TypedPrgm       -> TypedPrgm
heapcheckC                  :: TypedPrgm       -> CGReadyPrgm

tokC                      = tokenize
parseC                    = parse
-- dupCheckC                 = uncurry ( , , Set.empty) . dupCheck
dupCheckC                 = dupCheck
boxC         (ts, os, as) = let (ts', as') = boxMTypes (ts, as)
                            in (ts', os, as')
renameC      (ts, os, as) = (ts, renameObjs os, as)
exhaustCaseC (ts, os, as) = (ts, exhaustCases (toCMap ts) os, as)
freevarC     (ts, os, as) = (ts, setFVsObjs stgRTSGlobals os, as)
infotabC     (ts, os, as) = (ts, setITs os, as)
conmapC      (ts, os, as) = uncurry ( , , as) $ setCMaps ts os
typecheckC   (ts, os, as) = (ts, hmstgAssums os as)
orderfvsC    (ts, os)     = (ts, orderFVsArgs os)
knowncallC   (ts, os)     = (ts, propKnownCalls os)
heapcheckC   (ts, os)     = (ts, setHeapAllocs os)


-- Useful plugin points below.
--
--  The naming convention fromX indicates that
--  the function should be given something equivalent to X.
--
--   e.g. fromGhcTransform uses fromParsed because the STG that the
--     GHC Transform produces is equivalent to simple parsed STG


-- Input STG should have CMaps set. Performs typecheck to heapcheck (CG ready)
fromCMapped :: StgPrgm InfoTab -> CGReadyPrgm
fromCMapped = heapcheckC . knowncallC . orderfvsC . typecheckC

-- Input should have provided default Alts for non-exhaustive Case expressions.
-- Performs free var enumeration to heapcheck (CG ready)
fromCaseExhausted :: StgPrgm () -> CGReadyPrgm
fromCaseExhausted = fromCMapped . conmapC . infotabC . freevarC 

-- Input need only be parsed into the STG AST.
-- Performs all AST passes from dup check to heapcheck
fromParsed :: ParsedPrgm -> CGReadyPrgm
fromParsed = fromCaseExhausted . exhaustCaseC . renameC . boxC . dupCheckC

-- Input should be a String of STG source code.
-- Performs full parse to heapcheck
fromStgSource :: String -> CGReadyPrgm
fromStgSource = fromParsed . parseC . tokC

-- Input should be a String of MHS source code.
-- Performs full parse to heapcheck
fromMhsSource :: String -> CGReadyPrgm
fromMhsSource = fromMhsTransform . mhsSTGer

-- Input should come from MHS frontend.
-- Performs free var enumeration to heapcheck
fromMhsTransform :: StgPrgm () -> CGReadyPrgm
fromMhsTransform = fromCaseExhausted

-- Input should come from GHC frontend.
-- Performs all AST passes from dup check to heapcheck
fromGhcTransform :: ParsedPrgm -> CGReadyPrgm
fromGhcTransform = fromParsed
