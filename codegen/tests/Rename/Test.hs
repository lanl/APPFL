module Rename.Test where
import           Parser
import           Rename

import           Test.Tasty
import           Test.Tasty.Golden

import           Data.ByteString.Lazy
import           Data.String

unitTests :: TestTree
unitTests = testGroup "Rename Unit tests"
    [ goldenVsString "rename seq" "tests/Rename/seq.gold" renameseq
    ]

renameseq :: IO ByteString
renameseq = return $ fromString $ show $ renameObjs
            $ parser "seq = FUN(x y -> case x of { z -> y });"

