{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}

import           Driver
import           CMap
import           AST (Obj)
import           ADT (TyCon)
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Char (toLower)
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           System.Process
import           PPrint
import           Control.Monad (when)
import           Control.Applicative ((<|>))
-- Just a <|> Nothing == Just a <|> Just b == Just a
import           FromGHC.FromGHC 

-- build a.out from stg/mhs and run it
_eval :: String -> Bool -> IO()
_eval input showerr = do
  system "cd .. && make runtime"
  system "rm -f a.out"
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
                 in compile (update defaultOptions)
                            (buildDir ++ "/etc")
                            (buildDir ++ "/lib")
                            (buildDir ++ "/include")
                            False


buildit :: String -> Bool -> IO()
buildit input ccompile = let update x = x {optInput = Just input}
                        -- assumes we are in codegen dir
                             buildDir = "../build"
                         in compile (update defaultOptions)
                                    (buildDir ++ "/etc")
                                    (buildDir ++ "/lib")
                                    (buildDir ++ "/include")
                                    ccompile

-- generate c code from stg (no prelude)
stgc :: String -> IO ()
stgc filename = genc filename STG

-- generate c code from mhs (no prelude)
mhsc :: String -> IO ()
mhsc filename = genc filename MHS

genc :: String -> Frontend -> IO ()
genc filename fe =
  do
    cginp <- case fe of
               STG -> readFile filename >>= return . StgSource
               MHS -> readFile filename >>= return . MhsSource
               GHC -> ghc2stg "../prelude" filename >>= return . GhcTransformed
    let prog = pprinter $ codegener cginp True
    putStrLn prog
    writeFile "../runtime/userprog.c" prog


  
getParsedSTG :: CodegenInput -> (CodegenInput, [TyCon], [Obj ()])
getParsedSTG i = case i of
  StgSource src -> let (ts,os,as) = parser src in (StgParsed (ts,os,as), ts, os)
  MhsSource src -> let (ts,os,as) = mhsSTGer src in (MhsTransformed (ts,os,as), ts, os)
  StgParsed (ts,os,as) -> (i, ts, os)
  MhsTransformed (ts,os,_) -> (i, ts, os)
  GhcTransformed (ts,os) -> (i, ts, os)

data Frontend = STG | MHS | GHC deriving (Show, Eq)

frontends = [STG, MHS, GHC]
frontendStrings = map (map toLower . show) frontends
frontendAssoc = zip frontendStrings frontends

parseFrontArg :: String -> Maybe Frontend
parseFrontArg userArg = case [x | (s, x) <- frontendAssoc, arg `isPrefixOf` s] of
                          []  -> frontendOptErr userArg
                          x:_ -> Just x
  where arg = map toLower userArg
  
inferFrontend filename = listToMaybe [fe | (s,fe) <- feFileAssoc, s `isSuffixOf` filename]
  where feFileAssoc = [("mhs", MHS)
                      ,("stg", STG)
                      ,("hs" , GHC)]


frontendOptErr :: String -> a
frontendOptErr s = error $ "Frontend flag given but " ++ show s ++ " is not a valid frontend"

warnIfFrontendStrange Nothing Nothing =
  putStrLn "Warning: No frontend specified or inferred from file extension, using STG"
warnIfFrontendStrange u i
  | Just usr <- u, Just inf <- i, usr /= inf
  = putStrLn $ unlines
    ["Warning: Specified frontend (" ++ show usr ++
      ") does not match inferred (" ++ show inf ++ ").",
      "  Using the former."]

  | otherwise = return ()

  
                                                         
                                
data Options = Options
    { optVerbose   :: Bool
    , optHelp      :: Bool
    , optDumpParse :: Bool
    , optDumpSTG   :: Bool
    , optNoPrelude :: Bool
    , optOutput    :: Maybe FilePath
    , optInput     :: Maybe FilePath
    , optFrontend  :: Maybe Frontend
    } deriving Show

defaultOptions       = Options
    { optVerbose     = False
    , optHelp        = False
    , optDumpParse   = False
    , optDumpSTG     = False
    , optNoPrelude   = False
    , optInput       = Nothing
    , optOutput      = Just "a.out"
    , optFrontend    = Nothing -- Want to know if something was passed
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['v'] ["verbose"]
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
    , Option ['o'] ["output"]
        (ReqArg (\ f opts -> opts { optOutput = Just f }) "FILE")
        "output FILE"
    , Option ['f'] ["frontend"]
        (ReqArg (\s opts -> opts { optFrontend = parseFrontArg s }) "stg | mhs | ghc")
        "frontend (stg | mhs | ghc)"
    ]

header = "Usage: appfl [OPTION...] inputfile"

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
      case getOpt Permute options argv of
         (o,inp,[]) -> return (foldl (flip id) defaultOptions o, inp)
         (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

checkOpts :: Options ->  [String] -> IO ()
checkOpts (Options {optHelp}) optInputs =
    case optHelp of
      True -> ioError (userError (usageInfo header options))
      False -> case length optInputs of
                   0 -> ioError (userError ("No input files\n" ++ usageInfo header options))
                   1 -> return ()
                   _ ->  ioError (userError ("bad input\n" ++ usageInfo header options))



compile :: Options -> String -> String -> String -> Bool -> IO ()
compile  (Options {optVerbose, optDumpParse, optNoPrelude, optInput,
           optOutput, optDumpSTG, optFrontend}) preludeDir rtLibDir rtIncDir ccompile =
  do
    let input = fromJust optInput
        implicitFrontend = inferFrontend input
        frontend = fromJust $ optFrontend <|> implicitFrontend <|> Just STG
        preludeFN = preludeDir ++ "/Prelude." ++ map toLower (show frontend)
    ifd <- openFile input ReadMode
    src <- hGetContents ifd

    
    when optVerbose $ warnIfFrontendStrange optFrontend implicitFrontend
    
    prelude <- if optNoPrelude || frontend == GHC
               then return ""
               else openFile preludeFN ReadMode >>= hGetContents 
         -- prelude might not end in newline
    let source = prelude ++ "\n" ++ src

    (cginp, ts, os) <- case frontend of
                         GHC -> do
                           (ts,os) <- ghc2stg preludeDir input
                           return (GhcTransformed (ts,os), ts, os)
                         MHS -> return . getParsedSTG $ MhsSource source
                         STG -> return . getParsedSTG $ StgSource source

    when optDumpSTG $
      writeFile (input ++ ".dump.stg") (show $ unparse ts $+$ unparse os)

    case optDumpParse of
      True  -> do
                 let stgtext = show (toCMap $ ts) ++ show os
                 writeFile (input ++ ".dump") stgtext

      False -> do
                 ccenv <- lookupEnv "CC"
                 let cc = fromMaybe "gcc" ccenv
                 cflagsenv <- lookupEnv "CFLAGS"
                 let cflags = fromMaybe "" cflagsenv
                 let coutput = input ++ ".c"
                 let flags = " -Wall -Werror -Wno-missing-braces "
                               ++ cflags ++ " -std=gnu99 "
                               ++ " -Wl,-rpath -Wl," ++ rtLibDir
                               ++ " -L" ++ rtLibDir ++ " -I" ++ rtIncDir
                               ++ " -lruntime -labt -lm -pthread -lstdc++"
                 writeFile coutput (pprinter $ codegener cginp optVerbose)
                 if ccompile
                   then system (cc ++ " " ++ coutput ++ " -o " ++ fromJust optOutput ++ flags)
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
      compile (update opts (head args'))
              (binaryDir ++ "/../etc")
              (binaryDir ++ "/../lib")
              (binaryDir ++ "/../include")
              True
