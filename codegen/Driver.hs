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
  mhsTokenizer,
  mhsParser,
  mhsPup,
  mhsSigSetter,
  mhsRenamer,
  mhsGenFunctioner,
  mhsExpSimpler,
  mhsPatSimpler,
  mhsNumBoxer,
  mhsSTGer,
  mhsCheckAll
  
) where

import qualified MHS.AST
import qualified MHS.Parser
import qualified MHS.Transform
import qualified MHS.Tokenizer          
import qualified MHS.ToSTG

import           Tokenizer
import           Parser
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
import           System.IO

header :: String
header = "#include \"stgc.h\"\n" ++
         "#include \"stgApply.h\"\n"
        
footer :: Bool -> String
footer v = cgStart ++ cgMain v

-- nameDefs
--  :: [([Char], Obj)] ->
--     [([Char], [Char])] ->
--     State [[Char]] [([Char], Obj)]


-- need a better way, like reading from a .h file
stgRTSGlobals :: [String]
stgRTSGlobals = [ "stg_case_not_exhaustive" ]
                ++ map fst primopTab -- from AST.hs

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


-- set boxity in Monotypes of TyCon DataCons
boxer :: String -> ([TyCon], [Obj ()])
boxer inp = let (tycons, objs) = parser inp
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
defaultcaser :: String -> ([TyCon], [Obj ()])
defaultcaser inp = let (tycons, objs) = renamer inp
                 in (tycons, exhaustCases (toCMap tycons) objs)

freevarer :: String -> ([TyCon], [Obj ([Var],[Var])])
freevarer inp = let (tycons, objs) = defaultcaser inp
                in (tycons, setFVsObjs stgRTSGlobals objs)

infotaber :: String -> ([TyCon], [Obj InfoTab])
infotaber inp = let (tycons, objs) = freevarer inp
                in (tycons, setITs objs :: [Obj InfoTab])

conmaper :: String -> ([TyCon], [Obj InfoTab])
conmaper inp = let (tycons, objs) = infotaber inp
               in setCMaps tycons objs

typechecker :: String -> ([TyCon], [Obj InfoTab])
typechecker inp = let (tycons, objs) = conmaper inp
                  in (tycons, hmstg objs)

orderfvsargs inp = let (tycons, objs) = typechecker inp
                   in (tycons, orderFVsArgs objs)

knowncaller inp  = let (tycons, objs) = orderfvsargs inp --typechecker inp
                   in (tycons, propKnownCalls objs)

heapchecker :: String -> ([TyCon], [Obj InfoTab])
heapchecker inp = let (tycons, objs) = knowncaller inp
                  in (tycons, setHeapAllocs objs)


-- use MHS front end, pass through all MHS and STG transforms
-- and checks
mhsCheckAll :: String -> ([TyCon],[Obj InfoTab])
mhsCheckAll inp = let (ts0,os0,assums) = mhsSTGer inp
                      os1 = setITs $
                            setFVsObjs stgRTSGlobals $
                            exhaustCases (toCMap ts0) os0
                      (tsF, os2) = setCMaps ts0 os1
                      osF = setHeapAllocs $
                            propKnownCalls $
                            hmstgAssums os2 assums
                  in (tsF, osF)

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


tctest arg =
  do
    ifd <- openFile arg ReadMode
    source <- hGetContents ifd
    let (tycons, objs) = conmaper source
    hmstgdebug objs



codegener :: String -> Bool -> Bool -> String
codegener inp v mhs = let (tycons, objs) =
                            if mhs
                            then mhsCheckAll inp
                            else heapchecker inp
                                 
                          infotab = showITs objs
                          (shoForward, shoDef) = showSHOs objs
                          (funForwards, funDefs) = cgObjs objs stgRTSGlobals

                 in header ++
                    intercalate "\n" funForwards ++ "\n" ++
                    infotab ++ "\n" ++
                    shoForward ++ "\n" ++
                    shoDef ++ "\n" ++
                    intercalate "\n\n" funDefs ++
                    footer v


-- parse minihaskell in a file, add a block comment at the end
-- showing the unparsed STG code
addSTGComment :: FilePath -> IO ()
addSTGComment filename =
  do
    prld <- readFile "Prelude.mhs"
    src <- readFile filename
    let (ts,os) = mhsCheckAll (src++prld)
        stg = show $ bcomment (unparse ts $+$ unparse os)
    (length src) `seq` (writeFile filename (src ++ stg))
