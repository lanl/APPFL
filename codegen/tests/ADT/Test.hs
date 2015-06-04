module ADT.Test where
import           ADTnew
import           Parser

import           Test.Tasty
import           Test.Tasty.Golden

import           Data.ByteString.Lazy
import           Data.String
import qualified System.IO            as IO

unitTests :: TestTree
unitTests = testGroup "ADT Unit tests"
    [ goldenVsString "adt" "tests/ADT/adt.gold" adt
    ]
               
adt:: IO ByteString
adt = do
        input <- IO.readFile "tests/ADT/adt.stg"
        return $ fromString $ show $ updateTycons $ fst $ parse input