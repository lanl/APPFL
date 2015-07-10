module Driver (
  tokenizer,
  parser,
  renamer,
  normalizer,
  freevarer,
  infotaber,
  conmaper,
  typechecker,
  knowncaller,
  codegener
) where

import           ADT
import           AST
import           Analysis
import           SCC
import           CodeGen
-- MODIFIED 6.30 - David ----------------------------------------
import           CMap
import           DAnalysis
import           PPrint
--import           ConMaps
import           InfoTab
import           HeapObj
import           Tokenizer
import           Parser
import           Rename
import           SetFVs
import           HMStg
import           Data.List
import System.IO
import qualified Data.Map as Map

header :: String
header = "#include \"stg_header.h\"\n"
        
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

tokenizer :: String -> [Token]
tokenizer = tokenize

parser :: String -> ([TyCon], [Obj ()])
parser = parse . tokenizer

renamer :: String -> ([TyCon], [Obj ()])
renamer inp = let (tyCons, objs) = parser inp
              in (tyCons, renameObjs objs)

-- a branch for testing
--hm :: String -> [Obj Monotype]
--hm = let (tyCons, objs) = renamer inp
--     in (tyCons, hmstg tycons objs)

normalizer :: String -> ([TyCon], [Obj ()])
normalizer inp = let (tyCons, objs) = renamer inp
                 in (tyCons, exhaustCases (toCMap tyCons) objs)

freevarer :: String -> ([TyCon], [Obj ([Var],[Var])])
freevarer inp = let (tycons, objs) = normalizer inp
                in (tycons, setFVsObjs stgRTSGlobals objs)

infotaber :: String -> ([TyCon], [Obj InfoTab])
infotaber inp = let (tycons, objs) = freevarer inp
                in (tycons, setITs objs :: [Obj InfoTab])

-- MODIFIED 7.1 - David ----------------------------------------
-- assumption is that this typer is not actually useful now
-- real type inference would be after conmaper
--typer :: String -> ([TyCon], [Obj InfoTab])
--typer inp = let (tyCons, objs) = infotaber inp
--            in (tyCons, setTypes objs)

conmaper :: String -> ([TyCon], [Obj InfoTab])
-- MODIFIED 6.30 - David ----------------------------------------
conmaper = setCMaps . infotaber
--conmaper = setConmaps . typer


typechecker inp = let (tycons, objs) = conmaper inp
                  in (tycons, hmstg objs)


knowncaller inp  = let (tycons, objs) =  typechecker inp
                   in (tycons, propKnownCalls objs)

heapchecker inp = let (tycons, objs) = typechecker inp
                  in (tycons, setHeapAllocs objs $ toCMap tycons)

               

--printObjsVerbose :: ([TyCon], [Obj a]) -> IO () 
printObjsVerbose (tycons, objs) = print $ objListDoc objs

unparse (tycons, objs) =
  print $ toDoc $ (map DataDef tycons) ++ (map ObjDef objs)

tester ftest fprint file =
  do
    inp <- readFile file
    let tup = ftest inp
    fprint tup
    return ()


tctest arg =
  do
    ifd <- openFile arg ReadMode
    source <- hGetContents ifd
    let (tycons, objs) = conmaper source
    hmstgdebug objs


codegener :: String -> Bool -> String
codegener inp v = let (tycons, objs) = heapchecker inp
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
                  
