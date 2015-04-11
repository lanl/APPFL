module CodeGen.Test where
import           Parser
import           Rename
import           SetFVs
import           InfoTab
import           ConMap2
import           CodeGen

import           Test.Tasty
import           Test.Tasty.Golden

import           Data.ByteString.Lazy
import           Data.String 
import qualified Data.List as List

unitTests :: TestTree
unitTests = testGroup "Codegen Unit tests"
    [ goldenVsString "codegen one" "tests/Codegen/one.gold" cgone
    ]
    
defsVars :: [Obj [Var]]
defsVars = setFVsDefs $ renameObjs $ parser 
          "one = CON(I 1); main=THUNK(one);"
         
defsInfoTab :: [Obj InfoTab]
defsInfoTab =  setConMap (setITs defsVars :: [Obj InfoTab])
        
source :: String
source = let  (forwards, fundefs) = cgObjs defsInfoTab
    in List.intercalate "\n" forwards ++ 
       showITs defsInfoTab ++
       List.intercalate "\n\n" fundefs

cgone :: IO ByteString
cgone = return $ fromString source
