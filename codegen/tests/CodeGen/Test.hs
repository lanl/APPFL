module CodeGen.Test where
import           Parser
import           Rename
import           SetFVs
import           InfoTab
import           ConMap2
import           CodeGen
import           HeapObj
import           Boilerplate

import           Test.Tasty
import           Test.Tasty.Golden

import           Data.ByteString.Lazy
import           Data.String 
import qualified Data.List as List

unitTests :: TestTree
unitTests = testGroup "Codegen Unit tests"
    [ goldenVsString "codegen one" "tests/CodeGen/one.gold" cgone
    ]
    
defsVars :: [Obj [Var]]
defsVars = setFVsDefs $ renameObjs $ parser 
          "one = CON(I 1); main = THUNK(one);"
         
defsInfoTab :: [Obj InfoTab]
defsInfoTab =  setConMap (setITs defsVars :: [Obj InfoTab])
        
source :: String
source = let  (forwards, fundefs) = cgObjs defsInfoTab
              shos = showSHOs defsInfoTab
    in header ++
       List.intercalate "\n" forwards ++ "\n" ++
       showITs defsInfoTab ++ "\n" ++ 
       showSHOs defsInfoTab ++ "\n" ++ 
       List.intercalate "\n\n" fundefs ++
       footer

cgone :: IO ByteString
cgone = return $ fromString source
