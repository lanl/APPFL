module Parser.Test where
import           Parser

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

import           Data.ByteString.Lazy
import           Data.String
import qualified System.IO            as IO

unitTests :: TestTree
unitTests = testGroup "Parser Unit tests"
    [ testCase "parser 1" parser1
    , goldenVsString "parser 2" "tests/Parser/con2.gold" parser2
    , goldenVsString "parser 2b" "tests/Parser/con2.gold" parser2file
    ]

parser1 :: Assertion
parser1 = let
    ins = "one=CON(I 1);"
    outs = "[ObjDef (CON {omd = (), c = \"I\", as = [LitI 1], oname = \"one\"})]"
    in show (parser ins) @?= outs

parser2 :: IO ByteString
parser2 = return $ fromString $ show $ parser "two = CON(I 2);"

parser2file :: IO ByteString
parser2file = do
                input <- IO.readFile "tests/Parser/con2.stg"
                return $ fromString $ show $ parser input
