module Tokenizer.Test where
import           Driver

import           Test.Tasty
import           Test.Tasty.Golden

import           Data.ByteString.Lazy
import           Data.String
import qualified System.IO            as IO

unitTests :: TestTree
unitTests = testGroup "Tokenizer Unit tests"
    [ goldenVsString "Tokenizer unboxed" "tests/Tokenizer/unboxed.gold" unboxed
    , goldenVsString "Tokenizer adt" "tests/Tokenizer/adt.gold" adt
    ]
    
unboxed :: IO ByteString
unboxed = do
            input <- IO.readFile "tests/Tokenizer/unboxed.stg"
            return $ fromString $ show $ tokenizer input
            
adt:: IO ByteString
adt = do
        input <- IO.readFile "tests/Tokenizer/adt.stg"
        return $ fromString $ show $ tokenizer input