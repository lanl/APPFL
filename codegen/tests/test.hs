module Main where
import qualified CodeGen.Test
import qualified ConMap.Test
import qualified InfoTab.Test
import qualified Lexer.Test
import qualified Parser.Test
import qualified Rename.Test
import qualified Scanner.Test
import qualified SetFVs.Test

import           Test.Tasty

main :: IO()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ Scanner.Test.unitTests
                          , Lexer.Test.unitTests
                          , Parser.Test.unitTests
                          , Rename.Test.unitTests
                          , SetFVs.Test.unitTests
                          , InfoTab.Test.unitTests
                          , ConMap.Test.unitTests
                          , CodeGen.Test.unitTests
                          ]
