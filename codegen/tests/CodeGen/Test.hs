module CodeGen.Test where
import           Driver

import           Test.Tasty
import           Test.Tasty.Golden

import           Data.ByteString.Lazy
import           Data.String 
import qualified Data.List as List

unitTests :: TestTree
unitTests = testGroup "Codegen Unit tests"
    [ goldenVsString "codegen one" "tests/CodeGen/one.gold" cgone
    ]
    
inp = "one = CON(I 1); main = THUNK(one);"    

cgone :: IO ByteString
cgone = return $ fromString $ codegener inp True
