{-# LANGUAGE
NamedFieldPuns,
TypeSynonymInstances,
FlexibleInstances #-}


module ConvertSTG () where

import           ADT
import           AST
import           Data.Either        (rights)
import qualified Data.Set           as Set
import           Parser
import           PPrint
import           State
import           System.Environment (getArgs)
import           Tokenizer



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
    print $ show n ++ " case scrutinee bindings added"
    writeFile outfile . show . vcat . map unparse $ modified
    return ()

instance (Unparse a, Unparse b) => Unparse (Either a b) where
  unparse (Left x) = unparse x

  -- want to leave space between definitions, for readability
  unparse (Right x) = unparse x <> char '\n' 

instance Unparse Comment where
  unparse = text . init -- hack, removes newline character for cleaner printing

main :: IO ()
main = do
  files <- getArgs
  mapM_ (uncurry rewriteScruts) . zip files . map (++ ".new") $ files


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
      ECase{ee, ealts, scbnd} ->
        
        do
          ee1      <- asb env ee
          ealts1   <- asb env ealts
          scrtBind <- nextBind env

          return expr{ee    = ee1,
                      ealts = ealts1,
                      scbnd = scbnd{ea = Var scrtBind}}
                      
                      
      _ -> return expr


-------------------------------------------------- Alts Level
instance ASB (Alts a) where
  asb env a@Alts{alts} = asb env alts >>=
                         \alts1 -> return a{alts = alts1}

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
      
