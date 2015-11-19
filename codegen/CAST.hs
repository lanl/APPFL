{-# LANGUAGE FlexibleInstances #-}

module CAST (
  ExtDecl,
  InitializerMember,
  Ty(..),

  cEnum,
  cRegisterSHOs,
  cInfoTabStruct,
  cObjStruct,
  cStructMember,
) where

import           Data.List.Split
import           GHC.Float
import           Language.C.Data.Ident
import           Language.C.Data.InputStream
import           Language.C.Data.Node
import           Language.C.Pretty
import           Language.C.Syntax

import           AST

data Ty = EnumTy
        | StringTy
        | IntTy
        | FloatTy
        | DoubleTy
        | PtrTy
        | InfoPtrTy
        | VoidTy
        | StructTy deriving(Show)

type CInitializerMember a = ([CPartDesignator a], CInitializer a)

type ExtDecl = CExternalDeclaration NodeInfo
type InitializerMember = CInitializerMember NodeInfo

-- init a Struct Member w/ an Expr
cStructMemberE :: [String] -> CInitializer NodeInfo
  -> CInitializerMember NodeInfo
cStructMemberE name expr =
  let f x = CMemberDesig (builtinIdent x) undefNode
  in (map f name, expr)

class InitStructMember a where
  cStructMember :: Ty -> String -> a -> CInitializerMember NodeInfo

instance InitStructMember String where
  cStructMember ty name val =
    let e = case ty of
              EnumTy   -> CInitExpr (CVar (builtinIdent val) undefNode) undefNode
              StringTy -> CInitExpr (CConst (CStrConst (cString val) undefNode)) undefNode
              PtrTy -> CInitExpr (CUnary CAdrOp (CVar (builtinIdent val) undefNode) undefNode) undefNode
              InfoPtrTy -> CInitExpr (CCast (CDecl [CTypeSpec (CTypeDef (builtinIdent "uintptr_t") undefNode)]
                           [] undefNode) (CUnary CAdrOp (CVar (builtinIdent ("it_" ++ val)) undefNode)
                           undefNode) undefNode) undefNode
              -- empty/zero payload
              StructTy -> CInitList (if null val then [] else [([], CInitExpr (CConst (CIntConst (cInteger 0) undefNode))
                          undefNode)] ) undefNode
              _ -> error ("bad Type in cStructMember (String) " ++ show ty)
    in cStructMemberE (splitOn "." name) e

instance InitStructMember Int where
  cStructMember ty name val =
    let e = case ty of
              IntTy -> CInitExpr (CConst (CIntConst (cInteger (toInteger val)) undefNode)) undefNode
              _ -> error "bad Type in cStructMember (Int)"
    in cStructMemberE (splitOn "." name) e

instance InitStructMember Double where
  cStructMember ty name val =
    let e = case ty of
              DoubleTy -> CInitExpr (CConst (CFloatConst (cFloat (double2Float val)) undefNode)) undefNode
              _ -> error "bad Type in cStructMember (Double)"
    in cStructMemberE (splitOn "." name) e

instance InitStructMember Float where
  cStructMember ty name val =
    let e = case ty of
              FloatTy -> CInitExpr (CConst (CFloatConst (cFloat val) undefNode)) undefNode
              _ -> error "bad Type in cStructMember (Float)"
    in cStructMemberE (splitOn "." name) e

instance InitStructMember [CInitializerMember NodeInfo] where
  cStructMember ty name val =
    let e = case ty of
              StructTy -> CInitList [([],  CInitList val undefNode)] undefNode
              _ -> error "bad Type in cStructMember (Struct)"
    in cStructMemberE  (splitOn "." name) e

instance InitStructMember [[CInitializerMember NodeInfo]] where
  cStructMember ty name vals =
    let e = case ty of
              StructTy -> CInitList (map (\x -> ([], CInitList x undefNode)) vals) undefNode
              _ -> error "bad Type in cStructMember (Struct)"
    in cStructMemberE  (splitOn "." name) e

cObjStruct :: String -> [CInitializerMember NodeInfo] -> ExtDecl
cObjStruct name = cStruct "Obj" ("sho_" ++ name) False

cInfoTabStruct :: String -> [CInitializerMember NodeInfo] -> ExtDecl
cInfoTabStruct name = cStruct "InfoTab" ("it_" ++ name) True

cStruct :: String -> String -> Bool -> [CInitializerMember NodeInfo] -> ExtDecl
cStruct ty name align is =
  let t = [CTypeSpec (CTypeDef (builtinIdent ty) undefNode)]
      a = [CAttr (builtinIdent "aligned") [CConst (CIntConst (cInteger 8) undefNode)]
           undefNode | align]
      d = CDeclr (Just (builtinIdent name)) [] Nothing a undefNode
      i = CInitList is undefNode
      in CDeclExt (CDecl t [(Just d, Just i, Nothing)] undefNode)

cEnum :: String -> [String] -> ExtDecl
cEnum name xs = CDeclExt (CDecl [CTypeSpec (CEnumType (CEnum (Just (builtinIdent ("tycon_" ++ name)))
                   (Just (map (\y -> (builtinIdent ("con_" ++ y) , Nothing)) xs)) [] undefNode) undefNode)]
                   [] undefNode)

cFun :: Ty -> String -> [CDerivedDeclarator NodeInfo] -> [CCompoundBlockItem NodeInfo] -> ExtDecl
cFun ty name dds cbs =
  let r = case ty of
            IntTy -> CTypeSpec (CIntType undefNode)
            VoidTy -> CTypeSpec (CVoidType undefNode)
   in CFDefExt (CFunDef [r] (CDeclr (Just (builtinIdent name)) dds
      Nothing [] undefNode) [] (CCompound [] cbs undefNode) undefNode)

_cRegSHOExpr :: String -> CCompoundBlockItem NodeInfo
_cRegSHOExpr name =  CBlockStmt (CExpr
  (Just (CAssign CAssignOp (CIndex (CVar (builtinIdent "stgStatObj") undefNode)
  (CUnary CPostIncOp (CVar (builtinIdent "stgStatObjCount") undefNode) undefNode)
  undefNode) (CUnary CAdrOp (CVar (builtinIdent ("sho_" ++ name)) undefNode) undefNode)
  undefNode)) undefNode)

cRegisterSHOs :: [String] -> ExtDecl
cRegisterSHOs names = cFun VoidTy "registerSHOs"
                      [CFunDeclr (Right ([],False)) [] undefNode]
                      (map _cRegSHOExpr names)

cCall :: String -> [String] -> CCompoundBlockItem NodeInfo
cCall name args = let cvars = map (\a -> CVar (builtinIdent a) undefNode) args
                   in CBlockStmt (CExpr (Just (CCall (CVar (builtinIdent name)
                      undefNode) cvars undefNode)) undefNode)

cIntReturn :: Integer -> CCompoundBlockItem NodeInfo
cIntReturn x = CBlockStmt (CReturn (Just (CConst (CIntConst (cInteger x) undefNode))) undefNode)

cMain :: Bool -> ExtDecl
cMain v = 
  let top = [cCall "parseArgs" ["argc","argv"]
            ,cCall "initStg" []
            ,cCall "initCmm" []
            ,cCall "initGc" []
            ,cCall "CALL0_0" ["start"]
             ]
      body = top ++ [cCall "showStgHeap" [] | v] ++ [cIntReturn 0]

  in cFun IntTy "main"  
  [CFunDeclr (Right ([CDecl [CTypeSpec (CIntType undefNode)]
  [(Just (CDeclr (Just (builtinIdent "argc")) [] Nothing [] undefNode),Nothing,Nothing)] undefNode,
  CDecl [CTypeSpec (CCharType undefNode)] [(Just (CDeclr (Just (builtinIdent "argv")) 
  [CPtrDeclr [] undefNode,CPtrDeclr [] undefNode] Nothing [] undefNode),Nothing,Nothing)] 
  undefNode],False)) [] undefNode] body
  
 
-- examples

reg = cRegisterSHOs ["False", "True"]

e2 = cEnum "Bool" ["False","True"]


cobj = cObjStruct "four"
        [cStructMember InfoPtrTy "infoPtr" "four"
        ,cStructMember EnumTy "objType" "CON"
        ,cStructMember StringTy "ident" "four"
        ,cStructMember StructTy "payload"
            [cStructMember EnumTy "argType" "INT"
           ,cStructMember IntTy "i" (4 :: Int)
           ]
         ]

fobj = cObjStruct "eqInt"
        [cStructMember InfoPtrTy "infoPtr" "eqInt"
        ,cStructMember EnumTy "objType" "FUN"
        ,cStructMember StringTy "ident" "eqInt"
        ,cStructMember StructTy "payload" ""
        ]

tobj = cStruct "Obj" "sho_main" False
        [cStructMember InfoPtrTy "infoPtr" "main"
        ,cStructMember EnumTy "objType" "THUNK"
        ,cStructMember StringTy "ident" "main"
        ,cStructMember StructTy "payload" "0"
        ]

info = cInfoTabStruct "false"
         [cStructMember StringTy "name" "false"
         ,cStructMember PtrTy "entryCode" "stg_constructorcall"
         ,cStructMember EnumTy "objType" "CON"
         ,cStructMember IntTy "layoutInfo.payloadSize" (0 :: Int)
         ,cStructMember IntTy "layoutInfo.boxedCount" (0 :: Int)
         ,cStructMember IntTy "layoutInfo.unboxedCount" (0 :: Int)
         ,cStructMember StringTy "layoutInfo.permString" ""
         ,cStructMember IntTy "conFields.arity" (0 :: Int)
         ,cStructMember IntTy "conFields.tag" (0 :: Int)
         ,cStructMember StringTy "conFields.conName" "False"
         ]

