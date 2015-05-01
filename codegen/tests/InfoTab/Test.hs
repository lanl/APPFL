module InfoTab.Test where
import           ADT
import           Driver
import           InfoTab

import           Test.Tasty
import           Test.Tasty.Golden

import           Data.ByteString.Lazy
import           Data.String

unitTests :: TestTree
unitTests = testGroup "InfoTab Unit tests"
    [ goldenVsString "set infotab seq" "tests/InfoTab/seq.gold" setinfoseq
    , goldenVsString "show infotab seq" "tests/InfoTab/tabseq.gold" showinfoseq
    ]
    
inp = "seq = FUN(x y -> case x of { z -> y });"
             
setinfoseq :: IO ByteString
setinfoseq = return $ fromString $ show $ infotaber inp

showinfoseq :: IO ByteString
showinfoseq = return $ fromString $ showITs $ getObjs $ conmaper inp
           
