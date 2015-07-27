module Driver (
  renamer,
  normalizer,
  freevarer,
  infotaber,
  conmaper,
  typer,
  codegener,
  tctest,
  conmaptest
) where

import           ADT
import           Analysis
import           AST
import           SCC
import           CodeGen
import           ConMaps
import           InfoTab
import           HeapObj
import           Parser
import           Rename
import           SetFVs
import           HMStg
import           Data.List
import System.IO

header :: String
header = "#include \"stgc.h\"\n"
        
footer :: Bool -> String
footer v = 
  let top = "\n\nDEFUN0(start) {\n" ++
            "  registerSHOs();\n" ++
            "  Obj *showResultCont = stgAllocCallCont2(&it_stgShowResultCont, 0);\n" ++
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
stgRTSGlobals = [ "stg_case_not_exhaustive" ]
                ++ map fst primopTab -- from AST.hs

parser :: String -> ([TyCon], [Obj ()])
parser = parse

renamer :: String -> ([TyCon], [Obj ()])
renamer inp = let (tyCons, objs) = parser inp
              in (tyCons, renameObjs objs)

-- a branch for testing
--hm :: String -> [Obj Monotype]
--hm = let (tyCons, objs) = renamer inp
--     in (tyCons, hmstg tycons objs)

normalizer :: String -> ([TyCon], [Obj ()])
normalizer inp = let (tyCons, objs) = renamer inp
                 in (tyCons, normalize objs)

freevarer :: String -> ([TyCon], [Obj ([Var],[Var])])
freevarer inp = let (tyCons, objs) = normalizer inp
                in (tyCons, setFVsObjs stgRTSGlobals objs)

infotaber :: String -> ([TyCon], [Obj InfoTab])
infotaber inp = let (tyCons, objs) = freevarer inp
                in (tyCons, setITs objs :: [Obj InfoTab])

-- real type inference would be after conmaper
typer :: String -> ([TyCon], [Obj InfoTab])
typer inp = let (tyCons, objs) = infotaber inp
            in (tyCons, setTypes objs)

conmaper :: String -> ([TyCon], [Obj InfoTab])
conmaper = setConmaps . infotaber

typechecker inp = let (tycons, objs) = conmaper inp
                  in (tycons, hmstg objs)

tctest arg =
  do
    ifd <- openFile arg ReadMode
    source <- hGetContents ifd
    let (tycons, objs) = conmaper source
    hmstgdebug objs

conmaptest arg =
  do
   f <- readFile arg
   let (_, os) = typechecker f
   putStrLn $ maybe "no map found in objs" ppConMaps (getmap os)


   

codegener :: String -> Bool -> String
codegener inp v = let (tycons, objs) = typechecker inp
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
                  



