{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
#include "options.h"

module CAST (
  ExtDecl,
  InitializerMember,
  Ty(..),

  cEnum,
  cRegisterSHOs,
  cInfoTabStruct,
  cMain,
  cObjStruct,
  cStart,
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
        | UserTy
        | StructTy deriving(Show)

type CInitializerMember a = ([CPartDesignator a], CInitializer a)

type ExtDecl = CExternalDeclaration NodeInfo
type InitializerMember = CInitializerMember NodeInfo

varE :: String -> CExpression NodeInfo
varE val = (CVar (builtinIdent val) undefNode)

addrvarE :: String -> CExpression NodeInfo
addrvarE val = CUnary CAdrOp (varE val) undefNode

intE :: Integer -> CExpression NodeInfo
intE val = CConst (CIntConst (cInteger val) undefNode)

floatE ::Float -> CExpression NodeInfo
floatE val = CConst (CFloatConst (cFloat val) undefNode)

stringE :: String -> CExpression NodeInfo
stringE val = CConst (CStrConst (cString val) undefNode)

memberE :: String -> String -> Bool -> CExpression NodeInfo
memberE x y arrow = CMember (varE x) (builtinIdent y) arrow undefNode

typeSpec :: String -> CDeclarationSpecifier NodeInfo
typeSpec name = CTypeSpec (CTypeDef (builtinIdent name) undefNode)

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
              EnumTy   -> CInitExpr (varE val) undefNode
              StringTy -> CInitExpr (stringE val) undefNode
              PtrTy -> CInitExpr (addrvarE val) undefNode
              InfoPtrTy -> CInitExpr (CCast (CDecl [typeSpec "uintptr_t"]
                           [] undefNode) (addrvarE ("it_" ++ val)) undefNode) undefNode
              -- empty/zero payload
              StructTy -> CInitList (if null val then [] else [([], CInitExpr (intE 0) undefNode)] ) undefNode
              _ -> error ("bad Type in cStructMember (String) " ++ show ty)
    in cStructMemberE (splitOn "." name) e

instance InitStructMember Int where
  cStructMember ty name val =
    let e = case ty of
              IntTy -> CInitExpr (intE (toInteger val)) undefNode
              _ -> error "bad Type in cStructMember (Int)"
    in cStructMemberE (splitOn "." name) e

instance InitStructMember Double where
  cStructMember ty name val =
    let e = case ty of
              DoubleTy -> CInitExpr (floatE (double2Float val)) undefNode
              _ -> error "bad Type in cStructMember (Double)"
    in cStructMemberE (splitOn "." name) e

instance InitStructMember Float where
  cStructMember ty name val =
    let e = case ty of
              FloatTy -> CInitExpr (floatE val) undefNode
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
      a = [CAttr (builtinIdent "aligned") [intE 8]
           undefNode | align]
      d = CDeclr (Just (builtinIdent name)) [] Nothing a undefNode
      i = CInitList is undefNode
      in CDeclExt (CDecl t [(Just d, Just i, Nothing)] undefNode)

cEnum :: String -> [String] -> ExtDecl
cEnum name xs = CDeclExt (CDecl [CTypeSpec (CEnumType (CEnum (Just (builtinIdent ("tycon_" ++ name)))
                   (Just (map (\y -> (builtinIdent ("con_" ++ y) , Nothing)) xs)) [] undefNode) undefNode)]
                   [] undefNode)

cFun :: Ty -> String -> String -> [CDerivedDeclarator NodeInfo] -> [CCompoundBlockItem NodeInfo] -> ExtDecl
cFun ty tyname name dds cbs =
  let r = case ty of
            IntTy  -> [CTypeSpec (CIntType undefNode)]
            VoidTy -> [CTypeSpec (CVoidType undefNode)]
            UserTy -> [CTypeSpec (CTypeDef (builtinIdent tyname) undefNode)]
            _      -> error "bad Fun Ty"
   in CFDefExt (CFunDef r (CDeclr (Just (builtinIdent name)) dds
      Nothing [] undefNode) [] (CCompound [] cbs undefNode) undefNode)

_cRegSHOExpr :: String -> CCompoundBlockItem NodeInfo
_cRegSHOExpr name =  CBlockStmt (CExpr
  (Just (CAssign CAssignOp (CIndex (CVar (builtinIdent "stgStatObj") undefNode)
  (CUnary CPostIncOp (CVar (builtinIdent "stgStatObjCount") undefNode) undefNode)
  undefNode) (CUnary CAdrOp (CVar (builtinIdent ("sho_" ++ name)) undefNode) undefNode)
  undefNode)) undefNode)

emptyFunDeclr = [CFunDeclr (Right ([],False)) [] undefNode]

cRegisterSHOs :: [String] -> ExtDecl
cRegisterSHOs names = cFun VoidTy "" "registerSHOs"
                      emptyFunDeclr
                      (map _cRegSHOExpr names)


cCallExpr :: String -> [CExpression NodeInfo] -> CExpression NodeInfo
cCallExpr name args = CCall (CVar (builtinIdent name) undefNode) args undefNode



cCall:: String -> [CExpression NodeInfo] -> CCompoundBlockItem NodeInfo                      
cCall name args = CBlockStmt (CExpr (Just (cCallExpr name args )) undefNode) 

cCallVars :: String -> [String] -> CCompoundBlockItem NodeInfo
cCallVars name args = let cvars = map (\a -> CVar (builtinIdent a) undefNode) args
                   in cCall name cvars
              

cIntReturn :: Integer -> CCompoundBlockItem NodeInfo
cIntReturn x = CBlockStmt (CReturn (Just (CConst (CIntConst (cInteger x) undefNode))) undefNode)

cMain :: Bool -> ExtDecl
cMain v = 
  let top = [cCallVars "parseArgs" ["argc","argv"]
            ,cCall "initStg" []
            ,cCall "initCmm" []
            ,cCall "initGc" []
            ,cCallVars "CALL0_0" ["start"]
             ]
      body = top ++ [cCallVars "showStgHeap" [] | v] ++ [cIntReturn 0]

  in cFun IntTy "" "main"  
  [CFunDeclr (Right ([CDecl [CTypeSpec (CIntType undefNode)]
  [(Just (CDeclr (Just (builtinIdent "argc")) [] Nothing [] undefNode),Nothing,Nothing)] undefNode,
  CDecl [CTypeSpec (CCharType undefNode)] [(Just (CDeclr (Just (builtinIdent "argv")) 
  [CPtrDeclr [] undefNode,CPtrDeclr [] undefNode] Nothing [] undefNode),Nothing,Nothing)] 
  undefNode],False)) [] undefNode] body


_ptrDecl :: String -> CDeclarator NodeInfo
_ptrDecl name = CDeclr (Just (builtinIdent name)) [CPtrDeclr [] undefNode] Nothing [] undefNode

_objPtrDecl name val = CBlockDecl (CDecl [typeSpec "Obj"] [(Just (_ptrDecl name)
                        ,val, Nothing)] undefNode)


assign:: CExpression NodeInfo -> CExpression NodeInfo -> CCompoundBlockItem NodeInfo
assign lhs rhs = CBlockStmt (CExpr (Just (CAssign CAssignOp lhs rhs undefNode)) undefNode)

cStart :: ExtDecl
cStart = let body = [cCall "_POPVALS0" []
                    ,cCall "registerSHOs" []
                    ,_objPtrDecl "showResultCont" (Just (CInitExpr (cCallExpr "stgAllocCallCont2"
                      [addrvarE "it_stgShowResultCont",intE 0]) undefNode))
#if USE_ARGTYPE                      
                    ,assign (memberE "stgCurVal" "argType" False) (varE "HEAPOBJ")
#endif                   
                    ,assign (memberE "stgCurVal" "op" False) (addrvarE "sho_main")
                    ,cCall "STGJUMP1" 
                       [CMember (cCallExpr "getInfoPtr" [memberE "stgCurVal" "op" False])
                       (builtinIdent "entryCode") True undefNode, varE "stgCurVal"]
                    ]
         in cFun UserTy "FnPtr" "start" emptyFunDeclr body     

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

