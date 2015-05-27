{-# LANGUAGE NamedFieldPuns    #-}

import Parser
import Driver

import Data.List
import System.Console.GetOpt
import System.Environment
import System.IO
import System.Process
import Data.Maybe

data Options = Options
    { optVerbose     :: Bool
    , optDumpParse   :: Bool
    , optNoPrelude   :: Bool
    , optOutput      :: Maybe FilePath
    , optInput       :: Maybe FilePath
    } deriving Show

defaultOptions       = Options
    { optVerbose     = False
    , optDumpParse   = False
    , optNoPrelude   = False
    , optInput       = Nothing
    , optOutput      = Just "a.out"
    }

--options :: [OptDescr (Options -> Options)]
options =
    [ Option ['v']     ["verbose"]
        (NoArg (\ opts -> opts { optVerbose = True }))
        "debug output on stderr"
    , Option ['d'] ["dump-parse"]
        (NoArg (\ opts -> opts { optDumpParse = True }))
        "parser output only"
    , Option ['p'] ["no-prelude"]
        (NoArg (\ opts -> opts { optNoPrelude = True }))
        "do not include prelude"
    , Option ['o']     ["output"]
        (ReqArg ((\ f opts -> opts { optOutput = Just f })) "FILE")
        "output FILE"
    , Option ['c']     []
        (ReqArg ((\ f opts -> opts { optInput = Just f })) "FILE")
        "input FILE"
    ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
      case getOpt Permute options argv of
         (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
         (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage: stgc [OPTION...] files..."

checkOpts :: Options -> Options
checkOpts (Options {optInput}) = error ""

stgcc :: String -> IO()
stgcc input = let update x = x {optInput = Just input}
              in compile (update defaultOptions) "../driver" "../runtime"

compile :: Options -> String -> String -> IO ()
compile  (Options {optVerbose, optDumpParse, optNoPrelude, optOutput, optInput}) preludeDir runtimeDir = 
  do 
    let input = fromJust optInput
    ifd <- openFile input ReadMode
    src <- hGetContents ifd
    pfd <- openFile (preludeDir ++ "/Prelude.stg") ReadMode
    prelude <- hGetContents pfd
    let source = if optNoPrelude then src 
                 else prelude ++ src 
                 
    case optDumpParse of
      True  -> writeFile (input ++ ".dump") (show $ parse source)
      False -> do 
                 let coutput = input ++ ".c"
                 let flags = " -std=gnu99 -L" ++ runtimeDir ++ " -I" ++ runtimeDir ++ " -lruntime"
                 writeFile coutput (codegener source optVerbose)
                 system ("gcc " ++ coutput ++ " -o " ++ (fromJust optOutput) ++ flags)
                 return ()   
              
stgc :: String -> IO ()
stgc arg =
  do
    ifd <- openFile arg ReadMode
    source <- hGetContents ifd
    let prog = codegener source True
    putStrLn prog
    hClose ifd
    writeFile "../runtime/userprog.c" prog

stgcout :: String -> String -> IO ()    
stgcout infile outfile =
  do
    ifd <- openFile infile ReadMode
    source <- hGetContents ifd
    case outfile of
      "-"            -> writeFile (infile++".c") (codegener source False)
      "-dump-parse"  -> writeFile (infile++".dump") (show $ parse source)
      _              -> writeFile outfile (codegener source False)

main :: IO ()
main = 
    do
      args <- getArgs
      name <- getProgName
      case length args of
        0 -> error ("usage: " ++ name ++ " infile [outfile]")
        1 -> stgc $ head args
        _ -> stgcout (head args)  (args !! 1)
      

