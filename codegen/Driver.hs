module Driver (
  renamer,
  normalizer,
  freevarer,
  infotaber,
  conmaper,
  codegener
) where

import           ADT
import           Analysis
import           AST
import           CodeGen
import           ConMaps2IT
import           InfoTab
import           HeapObj
import           Parser
import           Rename
import           SetFVs

import           Data.List
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

parser :: String -> ([TyCon], [Obj ()])
parser = parse

renamer :: String -> ([TyCon], [Obj ()])
renamer inp = let (tyCons, objs) = parser inp
              in (tyCons, renameObjs objs)

normalizer :: String -> ([TyCon], [Obj ()])
normalizer inp = let (tyCons, objs) = renamer inp
                 in (tyCons, normalize objs)

freevarer :: String -> ([TyCon], [Obj [Var]])
freevarer inp = let (tyCons, objs) = normalizer inp
                in (tyCons, setFVsObjs stgRTSGlobals objs)

infotaber :: String -> ([TyCon], [Obj InfoTab])
infotaber inp = let (tyCons, objs) = freevarer inp
                in (tyCons, setITs objs :: [Obj InfoTab])

conmaper :: String -> [Def InfoTab]
conmaper = conmaps2IT . unsplitDefs . infotaber

-- typer :: String -> [Def InfoTab]
-- typer = setTypes . conmaper

codegener :: String -> Bool -> String
codegener inp v = let defs = conmaper inp
                      objs = getObjs defs
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
                  
