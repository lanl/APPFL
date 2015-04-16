-- stgc "tests/Prelude.stg"

import AST(Primop)
import Prelude
import Parser
import Rename
import Analysis
import ConMap
import SetFVs
import InfoTab
import HeapObj
import ConMap2
import CodeGen

import Data.List
import System.Environment
import System.IO

-- nameDefs
--  :: [([Char], Obj)] ->
--     [([Char], [Char])] ->
--     State [[Char]] [([Char], Obj)]

-- need a better way, like reading from a .h file
stgRTSGlobals = [ "stg_case_not_exhaustive" ]

doit prog = 
    let defs0 = parser prog
        defs1 = renameObjs defs0
        defs1_1 = normalize defs1
        defs2 = setFVsDefs defs1_1 stgRTSGlobals
        defs3 = setITs defs2 :: [Obj InfoTab]
        defs4 = setConMap defs3 
        infotab = showITs defs4
        sho = showSHOs defs4
        (forwards, fundefs) = cgObjs defs4 stgRTSGlobals
    in intercalate "\n" forwards ++ "\n\n" ++
       infotab ++ "\n" ++
       sho ++ "\n" ++
       intercalate "\n\n" fundefs
           
--    in showDefs defs2 ++
--    in intercalate "\n\n" (map showIT infoTabs)

stgc arg =
  do
    ifd <- openFile arg ReadMode
    source <- hGetContents ifd
    let prog = doit source
    putStrLn prog
    hClose ifd
    writeFile (arg++".c") prog

main = 
    do
      args <- getArgs
      stgc $ head args

