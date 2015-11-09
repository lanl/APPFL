{-# LANGUAGE FlexibleInstances #-}
module CAST where
import Language.C.Parser
import Language.C.Data.Ident
import Language.C.Data.InputStream
import Language.C.System.Preprocess
import Language.C.System.GCC
import Language.C.DSL

data Ty = EnumTy | StringTy | IntTy | PtrTy | InfoPtrTy | StructTy 

type CInitializerMember a = ([CPartDesignator a], CInitializer a)

-- init a Struct Member w/ an Expr
initStructMemberE :: [String] -> CInitializer NodeInfo
  -> CInitializerMember NodeInfo
initStructMemberE name expr = 
  let f x = CMemberDesig (builtinIdent x) undefNode 
  in (map f name, expr) 

class InitStructMember a where
  initStructMember :: Ty -> [String] -> a -> CInitializerMember NodeInfo

instance InitStructMember String where
  initStructMember ty name val =         
    let e = case ty of
              EnumTy   -> CInitExpr (CVar (builtinIdent val) undefNode) undefNode 
              StringTy -> CInitExpr (CConst (CStrConst (cString val) undefNode)) undefNode
              PtrTy -> CInitExpr (CUnary CAdrOp (CVar (builtinIdent val) undefNode) undefNode) undefNode
              InfoPtrTy -> CInitExpr (CCast (CDecl [CTypeSpec (CTypeDef (builtinIdent "uintptr_t") undefNode)] [] undefNode) (CUnary CAdrOp (CVar (builtinIdent val) undefNode) undefNode) undefNode) undefNode 
    in initStructMemberE name e

instance InitStructMember Integer where
  initStructMember ty name val =         
    let e = case ty of
              IntTy -> CInitExpr (CConst (CIntConst (cInteger val) undefNode)) undefNode
    in initStructMemberE name e

-- nested struct
instance InitStructMember [CInitializerMember NodeInfo] where
  initStructMember ty name val =         
    let e = case ty of
              StructTy -> CInitList [([],  CInitList val undefNode)] undefNode
    in initStructMemberE name e

initStruct :: String -> String -> Bool -> [CInitializerMember NodeInfo] -> CExternalDeclaration NodeInfo
initStruct ty name align is = 
  let t = [CTypeSpec (CTypeDef (builtinIdent ty) undefNode)]
      a = if align 
          then [CAttr (builtinIdent "aligned") [CConst (CIntConst (cInteger 8) undefNode)] undefNode] 
          else []
      d = CDeclr (Just (builtinIdent name)) [] Nothing a undefNode
      i = CInitList is undefNode
      in CDeclExt (CDecl t [(Just d, Just i, Nothing)] undefNode)


-- examples

obj = initStruct "Obj" "sho_four" False 
        [initStructMember InfoPtrTy ["infoPtr"] "it_four"
        ,initStructMember EnumTy ["objType"] "CON"
        ,initStructMember StringTy ["ident"] "four"
        ,initStructMember StructTy ["payload"] 
           [initStructMember EnumTy ["argType"] "INT"
           ,initStructMember IntTy ["i"] (4 :: Integer)
           ]
         ]

info = initStruct "InfoTab" "it_false" True
         [initStructMember StringTy ["name"] "false"
         ,initStructMember PtrTy ["entryCode"] "stg_constructorcall"
         ,initStructMember EnumTy ["objType"] "CON"
         ,initStructMember IntTy ["layoutInfo","payloadSize"] (0 :: Integer)
         ,initStructMember IntTy ["layoutInfo","boxedCount"] (0 :: Integer)
         ,initStructMember IntTy ["layoutInfo","unboxedCount"] (0 :: Integer)
         ,initStructMember StringTy ["layoutInfo","permString"] ""
         ,initStructMember IntTy ["conFields","arity"] (0 :: Integer)
         ,initStructMember IntTy ["conFields","tag"] (0 :: Integer)
         ,initStructMember StringTy ["conFields","conName"] "False"
         ]  

