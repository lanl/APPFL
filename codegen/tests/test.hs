module Main where
import qualified ADT.Test
import qualified CodeGen.Test
import qualified ConMap.Test
import qualified InfoTab.Test

import qualified Parser.Test
import qualified Rename.Test
import qualified SetFVs.Test
import qualified Tokenizer.Test

import           Test.Tasty
import           Test.Tasty.Ingredients.Basic
import           Test.Tasty.Runners.AntXML

main :: IO()
main = defaultMainWithIngredients 
       [antXMLRunner,listingTests,consoleTestReporter] tests

tests :: TestTree

tests = testGroup "Tests" [ Tokenizer.Test.unitTests
                          , Parser.Test.unitTests
                          , ADT.Test.unitTests
                          , Rename.Test.unitTests
                          , SetFVs.Test.unitTests
                          , InfoTab.Test.unitTests
                          , ConMap.Test.unitTests
                          , CodeGen.Test.unitTests
                          ]
                      
