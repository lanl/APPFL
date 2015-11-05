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

initializer :: CInitializer NodeInfo
initializer = CInitList [([CMemberDesig "infoPtr" undefNode],CInitExpr (CCast (CDecl [CTypeSpec (CTypeDef "uintptr_t" undefNode)] [] undefNode) (CUnary CAdrOp (CVar "it_four" undefNode) undefNode) undefNode) undefNode),([CMemberDesig "objType" undefNode],CInitExpr (CVar "CON" undefNode) undefNode),([CMemberDesig "ident" undefNode],CInitExpr (CConst (CStrConst (cString"four") undefNode)) undefNode),([CMemberDesig "payload" undefNode],CInitList [([],CInitList [([CMemberDesig "argType" undefNode],CInitExpr (CVar "INT" undefNode) undefNode),([CMemberDesig "i" undefNode],CInitExpr (CConst (CIntConst (cInteger 4) undefNode)) undefNode)] undefNode)] undefNode)] undefNode

--

type CInitializerMember a = ([CPartDesignator a], CInitializer a)

declSpecs :: Ident -> [CDeclarationSpecifier NodeInfo]
declSpecs name = [CTypeSpec (CTypeDef name undefNode)]

objDS = declSpecs "Obj"

declarator :: Ident -> CDeclarator NodeInfo
declarator name = CDeclr (Just name) [] Nothing [] undefNode

initStructMember :: Ident -> CInitializer NodeInfo 
  -> CInitializerMember NodeInfo
initStructMember name expr = ([CMemberDesig name undefNode], expr)  

initObjInfoPtr :: Ident -> CInitializerMember NodeInfo
initObjInfoPtr name = let e = CInitExpr (CCast (CDecl [CTypeSpec (CTypeDef "uintptr_t" undefNode)] [] undefNode) (CUnary CAdrOp (CVar name undefNode) undefNode) undefNode) undefNode
                      in initStructMember "infoPtr" e 

enumExpr :: Ident -> CInitializer NodeInfo
enumExpr name = CInitExpr (CVar name undefNode) undefNode

stringExpr :: String -> CInitializer NodeInfo
stringExpr name = CInitExpr (CConst (CStrConst (cString name) undefNode)) undefNode

intExpr :: Integer -> CInitializer NodeInfo
intExpr x = CInitExpr (CConst (CIntConst (cInteger x) undefNode)) undefNode

initObjObjType :: Ident -> CInitializerMember NodeInfo
initObjObjType name = initStructMember "objType" (enumExpr name) 

initObjIdent :: String -> CInitializerMember NodeInfo
initObjIdent name = initStructMember "ident" (stringExpr name) 

initObjPayload :: Ident -> Integer -> CInitializerMember NodeInfo
initObjPayload a i = let e = CInitList [([], CInitList [initObjPayloadArgType a, initObjPayloadI i] undefNode)] undefNode
                     in initStructMember "payload" e

initObjPayloadArgType :: Ident -> CInitializerMember NodeInfo
initObjPayloadArgType name = initStructMember "argType" (enumExpr name)

initObjPayloadI :: Integer -> CInitializerMember NodeInfo
initObjPayloadI x = initStructMember "i" (intExpr x)

objInitializer :: Ident -> Ident -> String -> Ident -> Integer -> CInitializer NodeInfo
objInitializer i t id pa pi = CInitList [initObjInfoPtr i, initObjObjType t, initObjIdent id, initObjPayload pa pi] undefNode

initializer4 :: CInitializer NodeInfo
initializer4 = objInitializer "it_four" "CON" "four" "INT" 4

initStruct :: [CDeclarationSpecifier NodeInfo] -> CDeclarator NodeInfo 
  -> CInitializer NodeInfo -> CExternalDeclaration NodeInfo
initStruct t d i = CDeclExt (CDecl t [(Just d, Just i, Nothing)] undefNode) 

initObj :: Ident -> CInitializer NodeInfo -> CExternalDeclaration NodeInfo
initObj name i = initStruct objDS (declarator name) i

o2 = initObj "sho_four" initializer4
