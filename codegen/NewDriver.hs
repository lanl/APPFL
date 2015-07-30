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
  codegener
) where

import           NewAST
import           NewParser
import           Transform
import           NewTokenizer

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
import           HMStg
import           Data.List
import           System.IO

header :: String
header = "#include \"stgc.h\"\n"
        
footer :: Bool -> String
footer v = 
  let top = "\nDEFUN0(start) {\n" ++
            "  registerSHOs();\n" ++
            "  stgPushCont(showResultCont);\n" ++
            "  STGEVAL(((PtrOrLiteral){.argType = HEAPOBJ, .op = &sho_main}));\n" ++
            "  STGRETURN0();\n" ++
            "  ENDFUN;\n" ++
            "}\n\n" ++
            "int main (int argc, char **argv) {\n" ++
            "  initStg();\n" ++
            "  initCmm();\n" ++
            "  initGc();\n" ++
            "  CALL0_0(start);\n"
      bot = "  return 0;\n" ++ "}\n\n"
  in if v then top ++ "  showStgHeap();\n" ++ bot else top ++ bot
           

-- nameDefs
--  :: [([Char], Obj)] ->
--     [([Char], [Char])] ->
--     State [[Char]] [([Char], Obj)]


-- need a better way, like reading from a .h file
stgRTSGlobals :: [String]
stgRTSGlobals = [ "stg_case_not_exhaustive",
                  "true",  -- sho_True
                  "false",  -- sho_False

                  "True#",  
                  "False#" 
                ] ++ map fst primopTab -- from AST.hs


-- strip comments, tokenize input
-- includes basic checking for valid characters and character strings
tokenizer :: String -> [Token]
tokenizer = tokenize


-- parse tokenized input
-- checks for valid syntax
parser :: String -> [Defn]
parser = parse . tokenizer


testparser = tester parser (show.unparse)


typeSigSetter inp = let defs = parser inp
                    in setTypeSignatures defs

testSigSetter = tester typeSigSetter (show.pprint)


renamer inp = let defs = typeSigSetter inp
               in renameIDs defs

testRenamer = tester renamer (show.unparse)


genConFunctioner inp = let defs = renamer inp
                       in genConFunctions defs

testGenConFunctioner = tester genConFunctioner (show.unparse)


desugarApser inp = let defs = genConFunctioner inp
                   in desugarExpAps defs

testDesugarApser = tester desugarApser (show.unparse)                      

numBoxer inp = let defs = desugarApser inp
               in boxNumLiterals defs

testNumBoxer = tester numBoxer (show.unparse)                  

boxer = undefined



renamer' :: String -> ([TyCon], [Obj ()])
renamer' inp = let (tycons, objs) = boxer inp
              in (tycons, renameObjs objs)


-- generate default cases in Alts blocks that need them
-- The ordering here is important to allow the freevarer and
-- infotaber steps to properly handle the newly generated ADef
-- objects
-- might be worth starting to pass CMap around starting here
-- (currently generated again when setting CMaps in InfoTabs)
defaultcaser :: String -> ([TyCon], [Obj ()])
defaultcaser inp = let (tycons, objs) = renamer' inp
                 in (tycons, exhaustCases (toCMap tycons) objs)

freevarer :: String -> ([TyCon], [Obj ([Var],[Var])])
freevarer inp = let (tycons, objs) = defaultcaser inp
                in (tycons, setFVsObjs stgRTSGlobals objs)

infotaber :: String -> ([TyCon], [Obj InfoTab])
infotaber inp = let (tycons, objs) = freevarer inp
                in (tycons, setITs objs :: [Obj InfoTab])

conmaper :: String -> ([TyCon], [Obj InfoTab])
conmaper inp = let (tycons, objs) = infotaber inp
               in setCMaps (tycons, objs)

typechecker inp = let (tycons, objs) = conmaper inp
                  in (tycons, hmstg objs)


knowncaller inp  = let (tycons, objs) = typechecker inp
                   in (tycons, propKnownCalls objs)

heapchecker inp = let (tycons, objs) = knowncaller inp
                  in (tycons, setHeapAllocs objs)

               
printObjsVerbose (tycons, objs) = print $ objListDoc objs



tester tfun sfun infile =
 do
   ihandle <- openFile infile ReadMode
   _tester tfun sfun ihandle stdout
  
_tester testfun showfun ifd ofd =
  do
    inp <- hGetContents ifd
    let res = testfun inp
        out = showfun res
    hPutStrLn ofd out
    return ()


tctest arg =
  do
    ifd <- openFile arg ReadMode
    source <- hGetContents ifd
    let (tycons, objs) = conmaper source
    hmstgdebug objs


codegener :: String -> Bool -> String
codegener inp v = let (tycons, objs) =  heapchecker inp
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

