import Lexer(Primop)
import Prelude
import Parser
import Rename
import InfoTab

import Data.List
import System.Environment
import System.IO

-- nameDefs
--  :: [([Char], Obj)] ->
--     [([Char], [Char])] ->
--     State [[Char]] [([Char], Obj)]

doit prog = 
    let defs0 = parser prog
        defs1 = renameDefs defs0
        defs2 = setFVsDefs defs1
        infoTabs = makeITs defs2
--    in showDefs defs2
    in intercalate "\n\n" (map showIT infoTabs)

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

