{-# LANGUAGE OverloadedStrings #-}
module CDSL where
import Language.C.Parser
import Language.C.Data.Ident
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

-- parser generated CConst (CIntConst 8 undefNode) bug?
c = CConst (CIntConst (cInteger 8) undefNode) 

-- parser generated CConst (CStrConst "false" undefNode) bug?
f =  CConst (CStrConst (cString "false") undefNode)

it =  (CDecl [CTypeSpec (CTypeDef (Ident "InfoTab" 236087325 undefNode) undefNode)] [(Just (CDeclr (Just (Ident "it_false" 429191370 undefNode)) [] Nothing [CAttr (Ident "aligned" 219392335 undefNode) [CConst (CIntConst (cInteger 8) undefNode)] undefNode] undefNode),Just (CInitList [([CMemberDesig (Ident "name" 213610734 undefNode) undefNode],CInitExpr (CConst (CStrConst (cString "false") undefNode)) undefNode)] undefNode),Nothing)] undefNode)
