import Data.List
import Lexer
import Parser
import Eval

lexFile :: FilePath -> IO ()
lexFile filename  = do
  input <- readFile filename
  putStrLn $ show $ lexString input

parseFile :: FilePath -> IO ()
parseFile filename  = do
  input <- readFile filename
  putStrLn $ show $ parseString input

evalFile :: FilePath -> IO ()
evalFile filename  = do
  input <- readFile filename
  putStrLn $ evalString input

evalFiles :: [FilePath] -> IO ()
evalFiles filenames = do
  input <- mapM readFile filenames
  putStrLn $ intercalate "\n" 
        ["\n" ++ x ++ "> " ++ evalString x | x <- input]

evalTestFiles =
    [ "test/atomliteral.stg",
    "test/maincon.stg", 
    "test/atomvariable.stg", 
    "test/let.stg", 
    "test/satprimadd.stg", 
    "test/simpleadd.stg", 
    "test/partadd.stg", 
    "test/fulladd.stg", 
    "test/fulladd2.stg",
    "test/list.stg" 
    ]

evalTests :: IO ()
evalTests = evalFiles evalTestFiles
