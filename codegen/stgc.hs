{-# LANGUAGE NamedFieldPuns #-}

import           Driver
import           CMap
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           System.Process
import           PPrint
import           Control.Monad (when)

-- build a.out from stg/mhs and run it
_eval :: String -> Bool -> IO()
_eval input showerr = do
  build input
  let erStr = if showerr then "" else " &2>/dev/null"
  system("./a.out" ++ erStr)
  putStrLn ""
  return ()

eval :: String -> IO()
eval i = _eval i True

evalNoErr i = _eval i False

-- build a.out from stg/mhs
build :: String -> IO()
build input = buildit input True

-- generate c code from stg/mhs
toc :: String -> IO()
toc input = buildit input False

mhs2stg :: String -> IO()
mhs2stg input =  let update x = x {optInput = Just input, optDumpSTG = True}
                      -- assumes we are in codegen dir
                     buildDir = "../build"
                 in compile (update defaultOptions) (buildDir ++ "/etc") (buildDir ++ "/lib") (buildDir ++ "/include") False


buildit :: String -> Bool -> IO()
buildit input gcc = let update x = x {optInput = Just input}
                        -- assumes we are in codegen dir
                        buildDir = "../build"
                    in compile (update defaultOptions) (buildDir ++ "/etc") (buildDir ++ "/lib") (buildDir ++ "/include") gcc

-- generate c code from stg (no prelude)
stgc :: String -> IO ()
stgc arg =
  do
    ifd <- openFile arg ReadMode
    source <- hGetContents ifd
    let prog = codegener source True False
    putStrLn prog
    hClose ifd
    writeFile "../runtime/userprog.c" prog

-- generate c code from mhs (no prelude)
mhsc :: String -> IO ()
mhsc arg =
  do
    ifd <- openFile arg ReadMode
    source <- hGetContents ifd
    let prog = codegener source True True
    putStrLn prog
    hClose ifd
    writeFile "../runtime/userprog.c" prog

data Options = Options
    { optVerbose   :: Bool
    , optHelp      :: Bool
    , optDumpParse :: Bool
    , optDumpSTG   :: Bool
    , optNoPrelude :: Bool
    , optOutput    :: Maybe FilePath
    , optInput     :: Maybe FilePath
    } deriving Show

defaultOptions       = Options
    { optVerbose     = False
    , optHelp        = False
    , optDumpParse   = False
    , optDumpSTG     = False
    , optNoPrelude   = False
    , optInput       = Nothing
    , optOutput      = Just "a.out"
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['v']     ["verbose"]
        (NoArg (\ opts -> opts { optVerbose = True }))
        "debug output on stderr"
    , Option ['?', 'h'] ["help"]
        (NoArg (\ opts -> opts { optHelp = True }))
        "show help"
    , Option ['d'] ["dump-parse"]
        (NoArg (\ opts -> opts { optDumpParse = True }))
        "parser output only"
    , Option ['g'] ["dump-stg"]
        (NoArg (\ opts -> opts {optDumpSTG = True}))
        "dump parsed STG code"
    , Option ['p'] ["no-prelude"]
        (NoArg (\ opts -> opts { optNoPrelude = True }))
        "do not include prelude"
    , Option ['o']     ["output"]
        (ReqArg ((\ f opts -> opts { optOutput = Just f })) "FILE")
        "output FILE"
    ]

header = "Usage: stgc [OPTION...] inputfile"

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
      case getOpt Permute options argv of
         (o,inp,[]) -> return (foldl (flip id) defaultOptions o, inp)
         (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

checkOpts :: Options ->  [String] -> IO ()
checkOpts (Options {optHelp}) optInputs =
  do
    case optHelp of
      True -> ioError (userError (usageInfo header options))
      False -> do
                 case length optInputs of
                   0 -> ioError (userError ("No input files\n" ++ usageInfo header options))
                   1 -> return ()
                   _ ->  ioError (userError ("bad input\n" ++ usageInfo header options)) 

compile :: Options -> String -> String -> String -> Bool -> IO ()
compile  (Options {optVerbose, optDumpParse, optNoPrelude, optInput, optOutput, optDumpSTG}) preludeDir rtLibDir rtIncDir gcc =
  do
    let input = fromJust optInput
        minihs = ".mhs" `isSuffixOf` input
        preludeFN = (preludeDir ++ "/Prelude" ++ (if minihs then ".mhs" else ".stg"))
    ifd <- openFile input ReadMode
    src <- hGetContents ifd
    pfd <- openFile preludeFN ReadMode
    prelude <- hGetContents pfd
    let source = if optNoPrelude then src
                 else prelude ++ src
        (ts,os) = let (t,o,_) =  mhsSTGer source             
                 in if minihs then (t,o) else parser source

    when optDumpSTG $
      writeFile (input ++ ".dump.stg") (show $ unparse ts $+$ unparse os)
        
    case optDumpParse of
      True  -> do 
                 let stgtext = (show $ toCMap $ ts) ++ (show os)                     
                 writeFile (input ++ ".dump") stgtext
                 
      False -> do
                 let coutput = input ++ ".c"
                 let flags = " -std=gnu99 -Wl,-rpath " ++ rtIncDir ++ " -L" ++ rtLibDir ++ " -I" ++ rtIncDir ++ " -lruntime"
                 writeFile coutput (codegener source optVerbose minihs)
                 if gcc 
                   then system ("gcc " ++ coutput ++ " -o " ++ (fromJust optOutput) ++ flags)
                   else return ExitSuccess
                 return ()

main :: IO ()
main =
    do
      binaryPath <- getExecutablePath
      let binaryDir = intercalate "/" $ init $ splitOn "/" binaryPath
      let update x input = x {optInput = Just input}
      args <- getArgs
      (opts, args') <- compilerOpts args
      checkOpts opts args'
      compile (update opts (head args')) (binaryDir ++ "/../etc") (binaryDir ++ "/../lib") (binaryDir ++ "/../include") True
