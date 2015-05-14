module ConMap.Test where

import           ADT
import           Driver
import           ConMaps2IT
import           InfoTab

import           Test.Tasty
import           Test.Tasty.Golden

import           Data.ByteString.Lazy
import           Data.String

unitTests :: TestTree
unitTests = testGroup "ConMap Unit tests"
    [ goldenVsString "conmap one" "tests/ConMap/mapone.gold" mapone
    ]
    
inp = "one = CON(I 1); main=THUNK(one);"
             
mapone :: IO ByteString
mapone = return $ fromString $ showITs $ getObjs $ 
                conmaps2IT  $ unsplitDefs $ typer inp