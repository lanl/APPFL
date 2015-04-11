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
    ]
unboxed :: IO ByteString
unboxed = do
            input <- IO.readFile "tests/Lexer/unboxed.stg"
            return $ fromString $ show $ lexer input