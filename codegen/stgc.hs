{-# LANGUAGE NamedFieldPuns #-}

import           ConMaps
import           Driver
import           Parser

import           Data.List
import           Data.List.Split
import           Data.Maybe
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           System.Process

-- build a.out from stg and run it
eval :: String -> IO()
eval input = do
               build input
               system("./a.out")
               return ()

-- build a.out from stg
build :: String -> IO()
build input = buildit input True

-- generate c code from stg
stg2c :: String -> IO()
stg2c input = buildit input False

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
    let prog = codegener source True
    putStrLn prog
    hClose ifd
    writeFile "../runtime/userprog.c" prog

data Options = Options
    { optVerbose   :: Bool
    , optHelp      :: Bool
    , optDumpParse :: Bool
    , optNoPrelude :: Bool
    , optOutput    :: Maybe FilePath
    , optInput     :: Maybe FilePath
    } deriving Show

defaultOptions       = Options
    { optVerbose     = False
    , optHelp        = False
    , optDumpParse   = False
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

header = "Usage: stgc [OPTION...] files..."

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
      case getOpt Permute options argv of
         (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
         (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

checkOpts :: Options -> IO ()
checkOpts (Options {optHelp, optInput}) =
  do
    case optHelp of
      True -> ioError (userError (usageInfo header options))
      False -> do
                 case optInput of
                   Nothing -> ioError (userError ("No input files\n" ++ usageInfo header options))
                   _ -> return ()

compile :: Options -> String -> String -> String -> Bool -> IO ()
compile  (Options {optVerbose, optDumpParse, optNoPrelude, optOutput, optInput}) preludeDir rtLibDir rtIncDir gcc =
  do
    let input = fromJust optInput
    ifd <- openFile input ReadMode
    src <- hGetContents ifd
    pfd <- openFile (preludeDir ++ "/Prelude.stg") ReadMode
    prelude <- hGetContents pfd
    let source = if optNoPrelude then src
                 else prelude ++ src

    case optDumpParse of
      True  -> do 
                 let (tycons, objs) = parse source
                 writeFile (input ++ ".dump") ((show $ buildConmaps $ tycons) ++ (show objs))
      False -> do
                 let coutput = input ++ ".c"
                 let flags = " -std=gnu99 -L" ++ rtLibDir ++ " -I" ++ rtIncDir ++ " -lruntime"
                 writeFile coutput (codegener source optVerbose)
                 if gcc then system ("gcc " ++ coutput ++ " -o " ++ (fromJust optOutput) ++ flags) else return ExitSuccess
                 return ()

main :: IO ()
main =
    do
      binaryPath <- getExecutablePath
      let binaryDir = intercalate "/" $ init $ splitOn "/" binaryPath
      args <- getArgs
      (opts, args') <- compilerOpts args
      checkOpts opts
      -- weird path stuff is because cabal puts binary in dist/build/stgc/stgc
      compile opts (binaryDir ++ "/../etc") (binaryDir ++ "/../lib") (binaryDir ++ "/../include")True

