module SetFVs.Test where
import           Parser
import           Rename
import           SetFVs

import           Test.Tasty
import           Test.Tasty.Golden

import           Data.ByteString.Lazy
import           Data.String

unitTests :: TestTree
unitTests = testGroup "SetFvs Unit tests"
    [ goldenVsString "SetFvs seq" "tests/SetFVs/seqFV.gold" setFVseq
    , goldenVsString "ShowDefs seq" "tests/SetFVs/seqDefs.gold" showseq
    ]
    
setFVseq :: IO ByteString
setFVseq = return $ fromString $ show $ setFVsDefs
           $ renameObjs $ parser "seq = FUN(x y -> case x of { z -> y });"

showseq :: IO ByteString
showseq = return $ fromString $ showDefs $ setFVsDefs
           $ renameObjs $ parser "seq = FUN(x y -> case x of { z -> y });"
