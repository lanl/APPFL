module ConMap.Test where

import           Driver
import           CMap

import           Test.Tasty
import           Test.Tasty.Golden

import           Data.ByteString.Lazy
import           Data.String

unitTests :: TestTree
unitTests = testGroup "ConMap Unit tests"
    [ goldenVsString "conmap one" "tests/ConMap/mapone.gold" mapone
    ]
    
inp = "data Int = I Int#; one = CON(I 1); main=THUNK(one);"
             
mapone :: IO ByteString
mapone = return $ fromString $ show $ toCMap $ fst $ parser inp