-- stgc "tests/Prelude.stg"
import Driver

import Data.List
import System.Environment
import System.IO

stgc arg =
  do
    ifd <- openFile arg ReadMode
    source <- hGetContents ifd
    let prog = codegener source
    putStrLn prog
    hClose ifd
    writeFile (arg++".c") prog
    writeFile "../runtime/userprog.c" prog

main = 
    do
      args <- getArgs
      stgc $ head args

