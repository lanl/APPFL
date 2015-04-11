module Scanner.Test where
import Scanner

import Test.Tasty
import Test.Tasty.Golden

import Data.String
import Data.ByteString.Lazy
import qualified System.IO as IO

unitTests :: TestTree
unitTests = testGroup "Scanner Unit tests"
    [ goldenVsString "Scanner comments" "tests/Scanner/comments.gold" comments
    , goldenVsString "Scanner add" "tests/Scanner/add.gold" add
    ]
      
comments :: IO ByteString
comments = do
             input <- IO.readFile "tests/Scanner/comments.stg"
             return $ fromString $ show $ scanner input

add :: IO ByteString
add = do
             input <- IO.readFile "tests/Scanner/add.stg"
             return $ fromString $ show $ scanner input