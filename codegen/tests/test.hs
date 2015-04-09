module Main where
import qualified ParserTest
import qualified RenameTest


import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

main :: IO()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ParserTest.unitTests, RenameTest.unitTests]
