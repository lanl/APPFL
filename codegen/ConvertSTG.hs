

module ConvertSTG () where

import Parser
import PPrint
import System.Environment (getArgs)


addScruts :: FilePath -> IO ()
addScruts file = undefined
  

main = do
  files <- getArgs
  mapM_ addScruts files
  return ()
  
