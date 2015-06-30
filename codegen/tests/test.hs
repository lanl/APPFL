module Main where
import qualified ADT.Test
import qualified CodeGen.Test
import qualified ConMap.Test
import qualified InfoTab.Test
import qualified Lexer.Test
import qualified Parser.Test
import qualified Rename.Test
import qualified Scanner.Test
import qualified SetFVs.Test

import           Test.Tasty
import                  Test.Tasty.Ingredients.Basic
import                  Test.Tasty.Runners.AntXML

main :: IO()
main = defaultMainWithIngredients 
       [antXMLRunner,listingTests,consoleTestReporter] tests

tests :: TestTree
tests = testGroup "Tests" [ Scanner.Test.unitTests
                          , Lexer.Test.unitTests
                          , Parser.Test.unitTests
                          , ADT.Test.unitTests
                          , Rename.Test.unitTests
                          , SetFVs.Test.unitTests
                          , InfoTab.Test.unitTests
                          , ConMap.Test.unitTests
                          , CodeGen.Test.unitTests
                          ]
                          
