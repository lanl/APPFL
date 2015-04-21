-- stgc "tests/Prelude.stg"
import Parser
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
    case outfile of
      "-"            -> writeFile (infile++".c") (codegener source)
      "-dump-parse"  -> writeFile (infile++".dump") (show $ parser source)
      _              -> writeFile outfile (codegener source)

main :: IO ()
main = 
    do
      args <- getArgs
      name <- getProgName
      case length args of
        0 -> error ("usage: " ++ name ++ " infile [outfile]")
        1 -> stgc $ head args
        _ -> stgcout (head args)  (args !! 1)
      

