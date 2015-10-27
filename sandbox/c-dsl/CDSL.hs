{-# LANGUAGE OverloadedStrings #-}
module CDSL where
import Language.C.Parser
import Language.C.Data.InputStream
import Language.C.System.Preprocess
import Language.C.System.GCC
import Language.C.DSL


-- parse C program string
pStr :: String -> Either ParseError CTranslUnit
pStr str = parseC (inputStreamFromString str) nopos

-- parse C file
pFile :: FilePath -> IO (Either ParseError CTranslUnit)
pFile f = do
            st <- readInputStream f 
            return (parseC st (initPos f))

-- preprocess and parse C file
ppFile :: FilePath -> IO (Either ParseError CTranslUnit)
ppFile f = parseCFile (newGCC "gcc") Nothing ["-I../../build/include"] f

