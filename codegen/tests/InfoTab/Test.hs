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
    [ goldenVsString "infotab seq" "tests/Infotab/seq.gold" setinfoseq
    , goldenVsString "infotab seq" "tests/Infotab/tabseq.gold" showinfoseq
    ]
    
seqdefs :: [Obj [Var]]
seqdefs = setFVsDefs $ renameObjs $ parser 
         "seq = FUN(x y -> case x of { z -> y });"
         
setinfoseq :: IO ByteString
setinfoseq = return $ fromString $ show (setITs seqdefs :: [Obj InfoTab])

showinfoseq :: IO ByteString
showinfoseq = return $ fromString $ showITs $ 
                setConMap (setITs seqdefs :: [Obj InfoTab])