module ConMap.Test where

import           ADT
import           Driver
import           ConMaps
import           InfoTab

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
mapone = return $ fromString $ showITs $ snd $ 
                setConmaps $ typer inp