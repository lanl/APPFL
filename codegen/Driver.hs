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

import           ADT
import           AST
import           SCC
import           CodeGen
import           CMap
import           Analysis
import           PPrint
import           InfoTab
import           HeapObj
import           Tokenizer
import           Parser
import           Rename
import           SetFVs
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
               in setCMaps (tycons, objs)

typechecker inp = let (tycons, objs) = conmaper inp
                  in (tycons, hmstg objs)

orderfvsargs inp = let (tycons, objs) = typechecker inp
                   in (tycons, orderFVsArgs objs)

knowncaller inp  = let (tycons, objs) = orderfvsargs inp --typechecker inp
                   in (tycons, propKnownCalls objs)

heapchecker inp = let (tycons, objs) = knowncaller inp
                  in (tycons, setHeapAllocs objs)

               
printObjsVerbose (tycons, objs) = print $ objListDoc objs


-- this is currently not a perfect unparse.
unparse (tycons, objs) =
  print $ pprint $ (map DataDef tycons) ++ (map ObjDef objs)

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

