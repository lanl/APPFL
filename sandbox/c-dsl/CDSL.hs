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

it1 =  CDeclExt (CDecl [CTypeSpec (CTypeDef (Ident "InfoTab" 236087325 undefNode) undefNode)] [(Just (CDeclr (Just (Ident "it_false" 429191370 undefNode)) [] Nothing [CAttr (Ident "aligned" 219392335 undefNode) [CConst (CIntConst (cInteger 8) undefNode)] undefNode] undefNode),Just (CInitList [([CMemberDesig (Ident "name" 213610734 undefNode) undefNode],CInitExpr (CConst (CStrConst (cString "false") undefNode)) undefNode)] undefNode),Nothing)] undefNode)

-- cleaner
it2 =  CDeclExt (CDecl [CTypeSpec (CTypeDef "InfoTab" undefNode)] [(Just (CDeclr (Just "it_false") [] Nothing [CAttr "aligned" [CConst (CIntConst (cInteger 8) undefNode)] undefNode] undefNode),Just (CInitList [([CMemberDesig "name" undefNode],CInitExpr (CConst (CStrConst (cString "false") undefNode)) undefNode)] undefNode),Nothing)] undefNode)

it3 = CDeclExt (CDecl [CTypeSpec (CTypeDef "InfoTab" undefNode)] [(Just (CDeclr (Just "it_false") [] Nothing [CAttr "aligned" [CConst (CIntConst (cInteger 8) undefNode)] undefNode] undefNode),Just (CInitList [([CMemberDesig "name" undefNode],CInitExpr (CConst (CStrConst (cString "false") undefNode)) undefNode),([CMemberDesig "entryCode" undefNode],CInitExpr (CUnary CAdrOp (CVar "stg_constructorcall" undefNode) undefNode) undefNode),([CMemberDesig "objType" undefNode],CInitExpr (CVar "CON" undefNode) undefNode),([CMemberDesig "layoutInfo" undefNode,CMemberDesig "payloadSize" undefNode],CInitExpr (CConst (CIntConst (cInteger 0) undefNode)) undefNode),([CMemberDesig "layoutInfo" undefNode,CMemberDesig "boxedCount" undefNode],CInitExpr (CConst (CIntConst (cInteger 0) undefNode)) undefNode),([CMemberDesig "layoutInfo" undefNode,CMemberDesig "unboxedCount" undefNode],CInitExpr (CConst (CIntConst (cInteger 0) undefNode)) undefNode),([CMemberDesig "layoutInfo" undefNode,CMemberDesig "permString" undefNode],CInitExpr (CConst (CStrConst  (cString "") undefNode)) undefNode),([CMemberDesig "conFields" undefNode,CMemberDesig "arity" undefNode],CInitExpr (CConst (CIntConst (cInteger 0) undefNode)) undefNode),([CMemberDesig "conFields" undefNode,CMemberDesig "tag" undefNode],CInitExpr (CConst (CIntConst (cInteger 0) undefNode)) undefNode),([CMemberDesig "conFields" undefNode,CMemberDesig "conName" undefNode],CInitExpr (CConst (CStrConst (cString "False") undefNode)) undefNode)] undefNode),Nothing)] undefNode)

o = CDeclExt (CDecl [CTypeSpec (CTypeDef "Obj" undefNode)] [(Just (CDeclr (Just "sho_four") [] Nothing [] undefNode),Just (CInitList [([CMemberDesig "infoPtr" undefNode],CInitExpr (CCast (CDecl [CTypeSpec (CTypeDef "uintptr_t" undefNode)] [] undefNode) (CUnary CAdrOp (CVar "it_four" undefNode) undefNode) undefNode) undefNode),([CMemberDesig "objType" undefNode],CInitExpr (CVar "CON" undefNode) undefNode),([CMemberDesig "ident" undefNode],CInitExpr (CConst (CStrConst (cString"four") undefNode)) undefNode),([CMemberDesig "payload" undefNode],CInitList [([],CInitList [([CMemberDesig "argType" undefNode],CInitExpr (CVar "INT" undefNode) undefNode),([CMemberDesig "i" undefNode],CInitExpr (CConst (CIntConst (cInteger 4) undefNode)) undefNode)] undefNode)] undefNode)] undefNode),Nothing)] undefNode)

