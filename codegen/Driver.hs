{-# LANGUAGE CPP #-}

#include "../options.h"

module Driver (
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

header :: String
header = "#include \"stgc.h\"\n"

footer :: Bool -> String
footer v = cgStart ++ cgMain v

-- nameDefs
--  :: [([Char], Obj)] ->
--     [([Char], [Char])] ->
--     State [[Char]] [([Char], Obj)]


-- need a better way, like reading from a .h file
stgRTSGlobals :: [String]
stgRTSGlobals = [ "stg_case_not_exhaustive", -- before type checking
                  "stg_case_not_exhaustiveP", -- during codegen
                  "stg_case_not_exhaustiveN"  -- during codegen
                ] ++ map fst primopTab -- from AST.hs

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
parser :: String -> ([TyCon], [Obj ()])
parser = parse . tokenizer

--checks for duplicates
dupChecker ::  String -> ([TyCon], [Obj ()])
dupChecker = dupCheck . parser

-- set boxity in Monotypes of TyCon DataCons
boxer :: String -> ([TyCon], [Obj ()])
boxer inp = let (tycons, objs) = dupChecker inp
            in (boxMTypes tycons, objs)


renamer :: String -> ([TyCon], [Obj ()])
renamer inp = let (tycons, objs) = boxer inp
              in (tycons, renameObjs objs)


-- generate default cases in Alts blocks that need them
-- The ordering here is important to allow the freevarer and
-- infotaber steps to properly handle the newly generated ADef
-- objects
-- might be worth starting to pass CMap around starting here
-- (currently generated again when setting CMaps in InfoTabs)
defaultcaser :: Bool -> String -> ([TyCon], [Obj ()],  Assumptions)
defaultcaser mhs inp = if mhs
                       then mhsSTGer inp
                       else let (tycons, objs) = renamer inp
                            in (tycons, exhaustCases (toCMap tycons) objs, Set.empty)

freevarer :: Bool -> String -> ([TyCon], [Obj ([Var],[Var])], Assumptions)
freevarer mhs inp = let (tycons, objs, assums) = defaultcaser mhs inp
                in (tycons, setFVsObjs stgRTSGlobals objs, assums)

infotaber :: Bool -> String -> ([TyCon], [Obj InfoTab], Assumptions)
infotaber mhs inp = let (tycons, objs, assums) = freevarer mhs inp
                in (tycons, setITs objs :: [Obj InfoTab], assums)

conmaper :: Bool -> String -> ([TyCon], [Obj InfoTab], Assumptions)
conmaper mhs inp = let (tycons, objs, assums) = infotaber mhs inp
                       (tycons', objs') = setCMaps tycons objs
                    in (tycons', objs', assums)

typechecker :: Bool -> String -> ([TyCon], [Obj InfoTab])
typechecker mhs inp = let (tycons, objs, assums) = conmaper mhs inp
                      in (tycons, if mhs then hmstgAssums objs assums else hmstg objs)

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

tester :: (String -> a) -> (a -> String) -> FilePath -> IO ()
tester tfun sfun infile =
 do
   ihandle <- openFile infile ReadMode
   _tester tfun sfun ihandle stdout

_tester :: (String -> a) -> (a -> String) -> Handle -> Handle -> IO ()
_tester testfun showfun ifd ofd =
  do
    inp <- hGetContents ifd
    let res = testfun inp
        out = showfun res
    hPutStrLn ofd out
    return ()

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
    let (tycons, objs, assums) = conmaper mhs source
    hmstgdebug objs


codegener :: String -> Bool -> Bool -> String
codegener inp v mhs = let (tycons, objs) = heapchecker mhs inp
                          typeEnums = showTypeEnums tycons
                          infotab = showITs objs
                          (shoForward, shoDef) = showSHOs objs
                          (funForwards, funDefs) = cgObjs objs stgRTSGlobals

                 in header ++ "\n" ++
                    intercalate "\n" funForwards ++ "\n\n" ++
                    typeEnums ++ "\n" ++
                    infotab ++ "\n" ++
                    shoForward ++ "\n" ++
                    shoDef ++ "\n" ++
                    intercalate "\n\n" funDefs ++
                    footer v

pprinter :: String -> String
pprinter = id

-- parse minihaskell in a file, add a block comment at the end
-- showing the unparsed STG code
addSTGComment :: FilePath -> IO ()
addSTGComment filename =
  do
    prld <- readFile "Prelude.mhs"
    src <- readFile filename
    let (ts,os) = heapchecker True (src++prld)
        stg = show $ bcomment (unparse ts $+$ unparse os)
    (length src) `seq` (writeFile filename (src ++ stg))
