module Scanner.Test where
import           Scanner

import           Test.Tasty
import           Test.Tasty.Golden

import           Data.ByteString.Lazy
import           Data.String
import qualified System.IO            as IO

unitTests :: TestTree
unitTests = testGroup "Scanner Unit tests"
    [ goldenVsString "Scanner comments" "tests/Scanner/comments.gold" comments
    , goldenVsString "Scanner add" "tests/Scanner/add.gold" add
    , goldenVsString "Scanner unboxed" "tests/Scanner/unboxed.gold" unboxed
     , goldenVsString "Scanner ADT" "tests/Scanner/adt.gold" adt
    ]

comments :: IO ByteString
comments = do
             input <- IO.readFile "tests/Scanner/comments.stg"
             return $ fromString $ show $ scanner input

add :: IO ByteString
add = do
        input <- IO.readFile "tests/Scanner/add.stg"
        return $ fromString $ show $ scanner input

unboxed :: IO ByteString
unboxed = do
            input <- IO.readFile "tests/Scanner/unboxed.stg"
            return $ fromString $ show $ scanner input
            
adt :: IO ByteString
adt = do
        input <- IO.readFile "tests/Scanner/adt.stg"
        return $ fromString $ show $ scanner input