module Parser.Test where
import Parser


import Test.Tasty
--import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty.Golden

import Data.String
import Data.ByteString.Lazy
import qualified System.IO as IO

unitTests :: TestTree
unitTests = testGroup "Parser Unit tests"
    [ testCase "parser 1" parser1
    , goldenVsString "parser goldString" "tests/Parser/con2.gold" parser2
    , goldenVsString "parser goldStringFIle" "tests/Parser/con2.gold" parser2file
    ]
      
parser1 :: Assertion
parser1 = let 
    ins = "one=CON(I 1);" 
    outs = "[CON {omd = (), c = \"I\", as = [Lit 1], oname = \"one\"}]"
    in show (parser ins) @?= outs 
     
parser2 :: IO ByteString
parser2 = return $ fromString $ show $ parser "two = CON(I 2);"

parser2file :: IO ByteString
parser2file = do
                   input <- IO.readFile "tests/Parser/con2.stg"
                   return $ fromString $ show $ parser input
