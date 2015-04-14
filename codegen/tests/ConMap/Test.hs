module ConMap.Test where
import           Parser
import           Rename
import           SetFVs
import           InfoTab
import           ConMap

import           Test.Tasty
import           Test.Tasty.Golden

import           Data.ByteString.Lazy
import           Data.String

unitTests :: TestTree
unitTests = testGroup "ConMap Unit tests"
    [ goldenVsString "conmap one" "tests/ConMap/mapone.gold" mapone
    ]
    
seqdefs :: [Obj [Var]]
seqdefs = setFVsDefs $ renameObjs $ parser 
         "one = CON(I 1); main=THUNK(one);"
         
mapone :: IO ByteString
mapone = return $ fromString $ show $ 
                getConMap (setITs seqdefs :: [Obj InfoTab])