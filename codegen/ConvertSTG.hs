{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- #!/usr/local/bin/runhaskell -XCPP -cpp -DREWRITE_STG=1
-- not working: Flags don't seem to be passed properly



module ConvertSTG () where

import           ADT
import           Analysis
import           AST
import           CMap
import           Data.Either        (rights)
import qualified Data.Set           as Set
import           DupCheck
import           DupCheck
import           Parser
import           PPrint
import           Rename
import           SetFVs
import InfoTab
import HMStg
import           State
import           System.Environment (getArgs)
import           System.IO
import           Tokenizer
import           Control.Monad(when)
import           Options

type Assumptions = Set.Set (Var, Monotype)


-- args are the STG files to port to the new syntax.
-- All files are written out with the same name (including old
-- extension) with a '.new' suffix.  If we want to do this on
-- all the STG files en masse, I'll make it a little more usable.
-- use runhaskell -cpp -DREWRITE_STG=1 [files] to run as a script
main :: IO ()
main =
  if reWriteSTG
  then
     do
       files <- getArgs
       mapM_ (uncurry rewriteScruts) . zip files . map (++ ".new") $ files
  else
    error "Only run/compile ConvertSTG.hs with -cpp -DREWRITE_STG flags for GHC"


-- ^ Modify an old STG file to update the syntax of case expressions
rewriteScruts
  :: FilePath -- ^ file to change
  -> FilePath -- ^ file to write to
  -> IO () -- ^ side effect: writes result out to specified file
rewriteScruts infile outfile =
  do
    src <- readFile infile
    let eithers = parseWithComments . tokenizeWithComments $ src
        isObj (ObjDef _) = True
        isObj _ = False
        env = Set.fromList . map (\(ObjDef o) -> oname o) .
              filter isObj . rights $ eithers
        (modified, n) = runState (asb env eithers) 0
    putStrLn $ show n ++ " case scrutinee bindings added"
    writeFile outfile . show $
      (vcat $ map unparse modified) $+$
      -- newline defeats the
      -- "Last line is a comment and messes up file concatenation"
      -- bug
      text "\n"


instance (Unparse a, Unparse b) => Unparse (Either a b) where
  unparse (Left x) = unparse x

  -- want to leave space between definitions for readability
  -- Semi-colons are good too
  unparse (Right x) = unparse x <> text ";\n"

instance Unparse Comment where
  unparse = text . init -- hack, removes newline character for cleaner printing




-- ^ Get the next unique variable name ensuring it does not
-- shadow anything visible in the current scope.
-- This may (with low probability) be shadowed further down the
-- expression tree, but, since this is only used for syntax conversion,
-- there is no real harm.
nextBind :: Env -> State Int Var
nextBind env =
  do
    i <- get
    put $ i + 1
    let v = "_scrt" ++ show i
    if v `Set.member` env

      -- v would shadow something, get next potential name.
      -- Ints *are* Bounded, so this is not perfectly robust,
      -- but should be fine for its purpose
      then nextBind env

      -- no shadow, v is good, use it
      else return v


type Env = Set.Set Var

-- ^ ASB  = Add Scrutinee Binding
class ASB a where
  -- ^ Traverse STG AST and add unique scrutinee bindings to convert
  -- an old STG file to the new syntax
  asb :: Env -> a -> State Int a



instance (ASB a) => ASB [a] where
  asb env m = mapM (asb env) m

-- Should only be used on Either Comment (Def ()).
-- Does not modify any Lefts (as in fmap)
instance (ASB b) => ASB (Either a b) where
  asb env (Right d) = asb env d >>= return . Right
  asb _ l = return l


-------------------------------------------------- Def level
instance ASB (Def a) where
  asb env (ObjDef o) = asb env o >>= return . ObjDef
  asb _ dd = return dd

-------------------------------------------------- Obj level
instance ASB (Obj a) where
  asb env obj =
    case obj of
      FUN{vs, e} -> asb newEnv e >>=
                   \e1 -> return obj{vs = vs, e = e1}
        -- function vars shadow previous bindings
        where newEnv = Set.union (Set.fromList vs) env

      THUNK{e} -> asb env e >>= \e1 -> return obj{e = e1}

      -- PAPs and CONs have only atomic expressions and
      -- don't need to be checked/modified
      _ -> return obj

-------------------------------------------------- Expr Level
instance ASB (Expr a) where
  asb env expr =
    case expr of
      ELet{edefs, ee} ->
        let newEnv = Set.union (Set.fromList $ map oname edefs) env
        in do
          edefs1 <- asb newEnv edefs
          ee1 <- asb newEnv ee
          return $ expr{edefs = edefs1,
                        ee = ee1}

      -- Variable addition happens here.
      -- Awful lot of work for such a small change...
      ECase{ee, ealts} ->

        do
          ee1      <- asb env ee
          ealts1   <- asb env ealts
          return expr{ee    = ee1,
                      ealts = ealts1}


      _ -> return expr


-------------------------------------------------- Alts Level
instance ASB (Alts a) where
  asb env a@Alts{alts, scrt} =
    asb env alts >>= \alts1 ->
    nextBind env >>= \v ->
    return a{alts = alts1,
            scrt = scrt{ea = Var v}}

instance ASB (Alt a) where
  asb env alt =
    -- Both Alternative types introduce new bindings
    -- creating a new environment
    let newEnv = Set.union newSet env
        newSet = case alt of
                   ACon{avs} -> Set.fromList avs
                   ADef{av} -> Set.singleton av
    in asb newEnv (ae alt) >>=
       \ae1 -> return alt{ae = ae1}




-- Test conversion with Driver in Progress
--------------------------------------------------

withPrelude x = "../prelude/Prelude.stg.new":[x]

tester :: (String -> a) -> (a -> String) -> [FilePath] -> IO ()
tester tfun sfun infiles =
 do
   ihandles <- mapM (flip openFile $ ReadMode) infiles
   _tester tfun sfun ihandles stdout

_tester :: (String -> a) -> (a -> String) -> [Handle] -> Handle -> IO ()
_tester testfun showfun ifds ofd =
  do
    inp <- mapM hGetContents ifds
    let res = testfun $ concat inp
        out = showfun res
    hPutStrLn ofd out

-- need a better way, like reading from a .h file
stgRTSGlobals :: [String]
stgRTSGlobals = [ "stg_case_not_exhaustive", -- before type checking
                  "stg_case_not_exhaustiveP", -- during codegen
                  "stg_case_not_exhaustiveN"  -- during codegen
                ] ++ map fst primopTab -- from AST.hs



-- strip comments, tokenize input
-- includes basic checking for valid characters and character strings
tokenizer :: String -> [Token]
tokenizer = tokenize

-- parse tokenized input
-- checks for valid syntax
parser :: String -> ([TyCon], [Obj ()])
parser = parse . tokenizer

--checks for duplicates
dupChecker ::  String -> ([TyCon], [Obj ()])
dupChecker = dupCheck . parser

-- set boxity in Monotypes of TyCon DataCons
boxer :: String -> ([TyCon], [Obj ()])
boxer inp = let (tycons, objs) = dupChecker inp
            in (boxMTypes tycons, objs)


renamer :: String -> ([TyCon], [Obj ()])
renamer inp = let (tycons, objs) = boxer inp
              in (tycons, renameObjs objs)


-- generate default cases in Alts blocks that need them
-- The ordering here is important to allow the freevarer and
-- infotaber steps to properly handle the newly generated ADef
-- objects
-- might be worth starting to pass CMap around starting here
-- (currently generated again when setting CMaps in InfoTabs)
defaultcaser :: String -> ([TyCon], [Obj ()], Assumptions)
defaultcaser inp = let (tycons, objs) = renamer inp
                   in (tycons, exhaustCases (toCMap tycons) objs, Set.empty)

freevarer :: String -> ([TyCon], [Obj ([Var],[Var])], Assumptions)
freevarer inp = let (tycons, objs, assums) = defaultcaser inp
                in (tycons, setFVsObjs stgRTSGlobals objs, assums)

infotaber :: String -> ([TyCon], [Obj InfoTab], Assumptions)
infotaber inp = let (tycons, objs, assums) = freevarer inp
                in (tycons, setITs objs :: [Obj InfoTab], assums)

conmaper :: String -> ([TyCon], [Obj InfoTab], Assumptions)
conmaper inp =
  let (tycons, objs, assums) = infotaber inp
      (tycons', objs')       = setCMaps tycons objs
  in (tycons', objs', assums)

typechecker :: String -> ([TyCon], [Obj InfoTab])
typechecker inp =
  let (tycons, objs, assums) = conmaper inp
  in (tycons, hmstg objs)
