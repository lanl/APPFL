-- stgc "tests/Prelude.stg"

import AST(Primop)
import Prelude
import Parser
import Rename
import ConMap
import SetFVs
import InfoTab
import ConMap2
import CodeGen

import Data.List
import System.Environment
import System.IO

-- nameDefs
--  :: [([Char], Obj)] ->
--     [([Char], [Char])] ->
--     State [[Char]] [([Char], Obj)]

doit prog = 
    let defs0 = parser prog
        defs1 = renameObjs defs0
        defs2 = setFVsDefs defs1
        defs3 = setITs defs2 :: [Obj InfoTab]
        defs4 = setConMap defs3 
        infotab = showITs defs4
        (forwards, fundefs) = cgObjs defs4
    in intercalate "\n" forwards ++ 
       infotab ++
       intercalate "\n\n" fundefs
           
--    in showDefs defs2 ++
--    in intercalate "\n\n" (map showIT infoTabs)

stgc arg =
  do
    ifd <- openFile arg ReadMode
    source <- hGetContents ifd
    putStrLn $ doit source
    hClose ifd

main = 
    do
      args <- getArgs
      stgc $ head args

