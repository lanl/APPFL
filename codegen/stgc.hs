-- stgc "tests/Prelude.stg"
import Driver

import Data.List
import System.Environment
import System.IO

stgc :: String -> IO ()
stgc arg =
  do
    ifd <- openFile arg ReadMode
    source <- hGetContents ifd
    let prog = codegener source
    putStrLn prog
    hClose ifd
    writeFile "../runtime/userprog.c" prog

stgcout :: String -> String -> IO ()    
stgcout infile outfile =
  do
    ifd <- openFile infile ReadMode
    source <- hGetContents ifd
    let prog = codegener source
    if outfile == "-" then
      writeFile (infile++".c") prog
      else writeFile outfile prog

main :: IO ()
main = 
    do
      args <- getArgs
      name <- getProgName
      case length args of
        0 -> error ("usage: " ++ name ++ " infile [outfile]")
        1 -> stgc $ head args
        _ -> stgcout (head args)  (args !! 1)
      

