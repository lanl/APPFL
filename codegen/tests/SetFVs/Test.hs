module SetFVs.Test where
import           ADT
import           Driver
import           Parser

import           Test.Tasty
import           Test.Tasty.Golden

import           Data.ByteString.Lazy
import           Data.String

unitTests :: TestTree
unitTests = testGroup "SetFvs Unit tests"
    [ goldenVsString "SetFvs seq" "tests/SetFVs/seqFV.gold" setFVseq
    , goldenVsString "ShowDefs seq" "tests/SetFVs/seqDefs.gold" showseq
    ]
    
inp :: String
inp = "seq = FUN(x y -> case x of { z -> y });"
    
setFVseq :: IO ByteString
setFVseq = return $ fromString $ show $ freevarer inp 

showseq :: IO ByteString
showseq = return $ fromString $ showObjs $ snd $ freevarer inp