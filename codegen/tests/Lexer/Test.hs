module Lexer.Test where
import           Lexer

import           Test.Tasty
import           Test.Tasty.Golden

import           Data.ByteString.Lazy
import           Data.String
import qualified System.IO            as IO

unitTests :: TestTree
unitTests = testGroup "Lexer Unit tests"
    [ goldenVsString "Lexer unboxed" "tests/Lexer/unboxed.gold" unboxed
    , goldenVsString "Lexer adt" "tests/Lexer/adt.gold" adt
    ]
    
unboxed :: IO ByteString
unboxed = do
            input <- IO.readFile "tests/Lexer/unboxed.stg"
            return $ fromString $ show $ lexer input
            
adt:: IO ByteString
adt = do
        input <- IO.readFile "tests/Lexer/adt.stg"
        return $ fromString $ show $ lexer input