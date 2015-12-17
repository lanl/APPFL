{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
#include "options.h"

module CAST (
  CExpr,
  CExtDecl,
  CInitializerMember,
  Ty(..),

  cAddrvarE,
  cAssign,
  cCall,
  cCallExpr,
  cCallVars,
  cCInfoTabStruct,
  cEnum,
  cFloatE,
  cFun,
  cFunProto,
  cInfoTabStruct,
  cInitCall,
  cIntE,
  cIntReturn,
  cMemberE,
  cNewHeapObj,
  cObjStruct,
  cPayloadE,
  cPtrD,
  cStringE,
  cStructMember,
  cTypeSpec,
  cUserPtrDecl,
  cVarE,

) where

import           Data.List.Split
import           GHC.Int
import           GHC.Float
import           Language.C.Data.Ident
--import           Language.C.Data.InputStream
import           Language.C.Data.Node
import           Language.C.Syntax

import           AST

data Ty = EnumTy
        | StringTy
        | IntTy
        | LongTy
        | FloatTy
        | DoubleTy
        | PtrTy
        | InfoPtrTy
        | VoidTy
        | UserTy
        | StructTy deriving(Show)

type CInitializerMember = ([CDesignator], CInit)

cPtrD = CPtrDeclr [] undefNode

cVarE :: String -> CExpr
cVarE val = CVar (builtinIdent val) undefNode

cAddrvarE :: String -> CExpr
cAddrvarE val = CUnary CAdrOp (cVarE val) undefNode

cIntE :: Integer -> CExpr
cIntE val = CConst (CIntConst (cInteger val) undefNode)

cFloatE ::Float -> CExpr
cFloatE val = CConst (CFloatConst (cFloat val) undefNode)

cStringE :: String -> CExpr
cStringE val = CConst (CStrConst (cString val) undefNode)

cMemberE :: String -> String -> Bool -> CExpr
cMemberE x y arrow = CMember (cVarE x) (builtinIdent y) arrow undefNode

cPayloadE :: String -> Integer -> CExpr
cPayloadE name n = CIndex (cMemberE name "payload" True)
                   (cIntE n) undefNode

cTypeSpec :: String -> CDeclSpec
cTypeSpec name = CTypeSpec (CTypeDef (builtinIdent name) undefNode)

cFunProto :: CDeclSpec -> String -> CExtDecl
cFunProto declspec name =
  CDeclExt (CDecl [declspec] [(Just (CDeclr (Just (builtinIdent name))
  [CFunDeclr (Right ([],False)) [] undefNode] Nothing [] undefNode),
  Nothing,Nothing)] undefNode)

-- init a Struct Member w/ an Expr
cStructMemberE :: [String] -> CInit -> CInitializerMember
cStructMemberE name expr =
  let f x = CMemberDesig (builtinIdent x) undefNode
  in (map f name, expr)

class InitStructMember a where
  cStructMember :: Ty -> String -> a -> CInitializerMember

instance InitStructMember String where
  cStructMember ty name val =
    let e = case ty of
              EnumTy   -> CInitExpr (cVarE val) undefNode
              StringTy -> CInitExpr (cStringE val) undefNode
              PtrTy -> CInitExpr (cAddrvarE val) undefNode
              InfoPtrTy -> CInitExpr (cAddrvarE ("it_" ++ val)) undefNode
              -- empty/zero payload
              StructTy -> CInitList (if null val then [] else [([],
                          CInitExpr (cIntE 0) undefNode)] ) undefNode
              _ -> error ("bad Type in cStructMember (String) " ++ show ty)
    in cStructMemberE (splitOn "." name) e

instance InitStructMember Int where
  cStructMember ty name val =
    let e = case ty of
              IntTy -> CInitExpr (cIntE (toInteger val)) undefNode
              _ -> error "bad Type in cStructMember (Int)"
    in cStructMemberE (splitOn "." name) e
instance InitStructMember Int64 where
  cStructMember ty name val =
    let e = case ty of
              IntTy -> CInitExpr (cIntE (toInteger val)) undefNode
              _ -> error "bad Type in cStructMember (Int)"
    in cStructMemberE (splitOn "." name) e

instance InitStructMember Double where
  cStructMember ty name val =
    let e = case ty of
              DoubleTy -> CInitExpr (cFloatE (double2Float val)) undefNode
              _ -> error "bad Type in cStructMember (Double)"
    in cStructMemberE (splitOn "." name) e

instance InitStructMember Float where
  cStructMember ty name val =
    let e = case ty of
              FloatTy -> CInitExpr (cFloatE val) undefNode
              _ -> error "bad Type in cStructMember (Float)"
    in cStructMemberE (splitOn "." name) e

instance InitStructMember [CInitializerMember] where
  cStructMember ty name val =
    let e = case ty of
              StructTy -> CInitList [([],  CInitList val undefNode)] undefNode
              _ -> error "bad Type in cStructMember (Struct)"
    in cStructMemberE  (splitOn "." name) e

instance InitStructMember [[CInitializerMember]] where
  cStructMember ty name vals =
    let e = case ty of
              StructTy -> CInitList (map (\x -> ([], CInitList x undefNode))
                          vals) undefNode
              _ -> error "bad Type in cStructMember (Struct)"
    in cStructMemberE  (splitOn "." name) e

cObjStruct :: String -> [CInitializerMember] -> (CExtDecl, CExtDecl)
cObjStruct name xs =
  let n = ("sho_" ++ name)
      proto = CDeclExt (CDecl [CStorageSpec (CExtern undefNode),
                 CTypeSpec (CTypeDef (builtinIdent "Obj") undefNode)]
                 [(Just (CDeclr (Just (builtinIdent n)) [] Nothing []
                 undefNode),Nothing,Nothing)] undefNode)
  in (proto, cStruct "Obj" n False xs)

cInfoTabStruct :: String -> [CInitializerMember] -> CExtDecl
cInfoTabStruct name = cStruct "InfoTab" ("it_" ++ name) True

cCInfoTabStruct :: String -> [CInitializerMember] -> CExtDecl
cCInfoTabStruct name = cStruct "CInfoTab" ("it_" ++ name) True

cStruct :: String -> String -> Bool -> [CInitializerMember] -> CExtDecl
cStruct ty name align is =
  let t = [cTypeSpec ty]
      a = [CAttr (builtinIdent "aligned") [cIntE 8]
           undefNode | align]
      d = CDeclr (Just (builtinIdent name)) [] Nothing a undefNode
      i = CInitList is undefNode
      in CDeclExt (CDecl t [(Just d, Just i, Nothing)] undefNode)

cEnum :: String -> [String] -> CExtDecl
cEnum name xs = CDeclExt (CDecl [CTypeSpec (CEnumType (CEnum (Just (builtinIdent
                ("tycon_" ++ name))) (Just (map (\y -> (builtinIdent ("con_" ++ y),
                Nothing)) xs)) [] undefNode) undefNode)] [] undefNode)

cFun :: Ty -> String -> String -> [CDerivedDeclr] -> [CBlockItem] -> CExtDecl
cFun ty tyname name dds cbs =
  let r = case ty of
            IntTy  -> [CTypeSpec (CIntType undefNode)]
            VoidTy -> [CTypeSpec (CVoidType undefNode)]
            UserTy -> [CTypeSpec (CTypeDef (builtinIdent tyname) undefNode)]
            _      -> error "bad Fun Ty"
   in CFDefExt (CFunDef r (CDeclr (Just (builtinIdent name)) dds
      Nothing [] undefNode) [] (CCompound [] cbs undefNode) undefNode)

cCallExpr :: String -> [CExpr] -> CExpr
cCallExpr name args = CCall (CVar (builtinIdent name) undefNode) args undefNode

cCall:: String -> [CExpr] -> CBlockItem
cCall name args = CBlockStmt (CExpr (Just (cCallExpr name args )) undefNode)

cCallVars :: String -> [String] -> CBlockItem
cCallVars name args = let cvars = map (\a -> CVar (builtinIdent a) undefNode) args
                   in cCall name cvars

cIntReturn :: Integer -> CBlockItem
cIntReturn x = CBlockStmt (CReturn (Just (CConst (CIntConst
               (cInteger x) undefNode))) undefNode)

_ptrDecl :: String -> CDeclr
_ptrDecl name = CDeclr (Just (builtinIdent name)) [cPtrD] Nothing [] undefNode

cUserPtrDecl :: String -> String -> Maybe CInit -> CBlockItem
cUserPtrDecl ty name val = CBlockDecl (CDecl [cTypeSpec ty] [(Just (_ptrDecl name),
                           val, Nothing)] undefNode)

cInitCall :: String -> [CExpr] -> Maybe CInit
cInitCall name args =  Just (CInitExpr (cCallExpr name args) undefNode)

cNewHeapObj :: String -> String -> CBlockItem
cNewHeapObj name val = cUserPtrDecl "Obj" name (cInitCall "stgNewHeapObj" [cAddrvarE val])

cAssign :: CExpr -> CExpr -> CBlockItem
cAssign lhs rhs = CBlockStmt (CExpr (Just (CAssign CAssignOp lhs rhs undefNode)) undefNode)
