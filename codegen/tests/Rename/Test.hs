module Rename.Test where

import           TestUtils

import           Test.Tasty
import           Test.Tasty.Golden

import           Data.ByteString.Lazy
import           Data.String

unitTests :: TestTree
unitTests = testGroup "Rename Unit tests"
    [ goldenVsString "rename seq" "tests/Rename/seq.gold" renameseq
    ]

renameseq :: IO ByteString
renameseq = return $ fromString $ show $ renamer
            "seq = FUN(x y -> case x of { z -> y });"

