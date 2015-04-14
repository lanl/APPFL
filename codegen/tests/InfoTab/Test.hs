module InfoTab.Test where
import           Parser
import           Rename
import           SetFVs
import           InfoTab
import           ConMap2

import           Test.Tasty
import           Test.Tasty.Golden

import           Data.ByteString.Lazy
import           Data.String

unitTests :: TestTree
unitTests = testGroup "InfoTab Unit tests"
    [ goldenVsString "set infotab seq" "tests/InfoTab/seq.gold" setinfoseq
    , goldenVsString "show infotab seq" "tests/InfoTab/tabseq.gold" showinfoseq
    ]
    
seqdefs :: [Obj [Var]]
seqdefs = setFVsDefs $ renameObjs $ parser 
         "seq = FUN(x y -> case x of { z -> y });"
         
setinfoseq :: IO ByteString
setinfoseq = return $ fromString $ show (setITs seqdefs :: [Obj InfoTab])

showinfoseq :: IO ByteString
showinfoseq = return $ fromString $ showITs $ 
                setConMap (setITs seqdefs :: [Obj InfoTab])
