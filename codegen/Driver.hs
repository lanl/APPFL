module Driver (
  renamer,
  normalizer,
  freevarer,
  infotaber,
  conmaper,
  codegener
) where

import           Analysis
import           Boilerplate
import           CodeGen
import           ConMap2
import           InfoTab
import           HeapObj
import           Parser
import           Rename
import           SetFVs

import           Data.List

-- nameDefs
--  :: [([Char], Obj)] ->
--     [([Char], [Char])] ->
--     State [[Char]] [([Char], Obj)]


-- need a better way, like reading from a .h file
stgRTSGlobals :: [String]
stgRTSGlobals = [ "stg_case_not_exhaustive",
                  "True",  -- sho_True
                  "False"] -- sho_False

renamer :: String -> [Obj ()]
renamer = renameObjs . parser

normalizer :: String -> [Obj ()]
normalizer = normalize . renamer

freevarer :: String -> [Obj [Var]]
freevarer inp = let defs = normalizer inp
                in setFVsDefs defs stgRTSGlobals

infotaber :: String -> [Obj InfoTab]
infotaber inp = let defs = freevarer inp
                in setITs defs :: [Obj InfoTab]

conmaper :: String -> [Obj InfoTab]
conmaper = setConMap . infotaber

codegener :: String -> String
codegener inp = let defs = conmaper inp
                    infotab = showITs defs
                    sho = showSHOs defs
                    (forwards, fundefs) = cgObjs defs stgRTSGlobals
                 in header ++
                    intercalate "\n" forwards ++ "\n" ++
                    infotab ++ "\n" ++
                    sho ++ "\n" ++
                    intercalate "\n\n" fundefs ++
                    footer
                   