{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
#include "options.h"

module CAST (
  CExpr,
  CExtDecl,
  CInitializerMember,
  Ty(..),

  cEnum,
  cRegisterSHOs,
  cInfoTabStruct,
  cCInfoTabStruct,
  cMain,
  cObjStruct,
  cStart,
  cStructMember,
  cPoLE,
  cHOTOPLshoE,
  cHOTOPLE,
  cFVE,
  cACE,
  cVarE,
  cFnPtrFun,
  cNewHeapObj,
) where

import           Data.List.Split
import           GHC.Int
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
        | LongTy
        | FloatTy
        | DoubleTy
        | PtrTy
        | InfoPtrTy
        | VoidTy
        | UserTy
        | StructTy deriving(Show)

type CInitializerMember = ([CPartDesignator NodeInfo], CInitializer NodeInfo)

ptrD = CPtrDeclr [] undefNode

cVarE :: String -> CExpr
cVarE val = CVar (builtinIdent val) undefNode

addrvarE :: String -> CExpr
addrvarE val = CUnary CAdrOp (cVarE val) undefNode

intE :: Integer -> CExpr
intE val = CConst (CIntConst (cInteger val) undefNode)

floatE ::Float -> CExpr
floatE val = CConst (CFloatConst (cFloat val) undefNode)

stringE :: String -> CExpr
stringE val = CConst (CStrConst (cString val) undefNode)

memberE :: String -> String -> Bool -> CExpr
memberE x y arrow = CMember (cVarE x) (builtinIdent y) arrow undefNode

typeSpec :: String -> CDeclarationSpecifier NodeInfo
typeSpec name = CTypeSpec (CTypeDef (builtinIdent name) undefNode)

funProto :: CDeclarationSpecifier NodeInfo -> String -> CExtDecl
funProto declspec name =
  CDeclExt (CDecl [declspec] [(Just (CDeclr (Just (builtinIdent name))
  [CFunDeclr (Right ([],False)) [] undefNode] Nothing [] undefNode),Nothing,Nothing)] undefNode)

-- init a Struct Member w/ an Expr
cStructMemberE :: [String] -> CInitializer NodeInfo
  -> CInitializerMember
cStructMemberE name expr =
  let f x = CMemberDesig (builtinIdent x) undefNode
  in (map f name, expr)

class InitStructMember a where
  cStructMember :: Ty -> String -> a -> CInitializerMember

instance InitStructMember String where
  cStructMember ty name val =
    let e = case ty of
              EnumTy   -> CInitExpr (cVarE val) undefNode
              StringTy -> CInitExpr (stringE val) undefNode
              PtrTy -> CInitExpr (addrvarE val) undefNode
              InfoPtrTy -> CInitExpr (addrvarE ("it_" ++ val)) undefNode
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
instance InitStructMember Int64 where
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

instance InitStructMember [CInitializerMember] where
  cStructMember ty name val =
    let e = case ty of
              StructTy -> CInitList [([],  CInitList val undefNode)] undefNode
              _ -> error "bad Type in cStructMember (Struct)"
    in cStructMemberE  (splitOn "." name) e

instance InitStructMember [[CInitializerMember]] where
  cStructMember ty name vals =
    let e = case ty of
              StructTy -> CInitList (map (\x -> ([], CInitList x undefNode)) vals) undefNode
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
  let t = [typeSpec ty]
      a = [CAttr (builtinIdent "aligned") [intE 8]
           undefNode | align]
      d = CDeclr (Just (builtinIdent name)) [] Nothing a undefNode
      i = CInitList is undefNode
      in CDeclExt (CDecl t [(Just d, Just i, Nothing)] undefNode)

cEnum :: String -> [String] -> CExtDecl
cEnum name xs = CDeclExt (CDecl [CTypeSpec (CEnumType (CEnum (Just (builtinIdent ("tycon_" ++ name)))
                   (Just (map (\y -> (builtinIdent ("con_" ++ y) , Nothing)) xs)) [] undefNode) undefNode)]
                   [] undefNode)

cFun :: Ty -> String -> String -> [CDerivedDeclarator NodeInfo] -> [CBlockItem] -> CExtDecl
cFun ty tyname name dds cbs =
  let r = case ty of
            IntTy  -> [CTypeSpec (CIntType undefNode)]
            VoidTy -> [CTypeSpec (CVoidType undefNode)]
            UserTy -> [CTypeSpec (CTypeDef (builtinIdent tyname) undefNode)]
            _      -> error "bad Fun Ty"
   in CFDefExt (CFunDef r (CDeclr (Just (builtinIdent name)) dds
      Nothing [] undefNode) [] (CCompound [] cbs undefNode) undefNode)

_cRegSHOExpr :: String -> CBlockItem
_cRegSHOExpr name =  CBlockStmt (CExpr
  (Just (CAssign CAssignOp (CIndex (CVar (builtinIdent "stgStatObj") undefNode)
  (CUnary CPostIncOp (CVar (builtinIdent "stgStatObjCount") undefNode) undefNode)
  undefNode) (CUnary CAdrOp (CVar (builtinIdent ("sho_" ++ name)) undefNode) undefNode)
  undefNode)) undefNode)

emptyFunDeclr = [CFunDeclr (Right ([],False)) [] undefNode]

cRegisterSHOs :: [String] -> (CExtDecl, CExtDecl)
cRegisterSHOs names = let name = "registerSHOs"
                      in (funProto (CTypeSpec (CVoidType undefNode)) name,
                          cFun VoidTy "" name emptyFunDeclr
                          (map _cRegSHOExpr names))

cCallExpr :: String -> [CExpression NodeInfo] -> CExpr
cCallExpr name args = CCall (CVar (builtinIdent name) undefNode) args undefNode


cCall:: String -> [CExpression NodeInfo] -> CBlockItem
cCall name args = CBlockStmt (CExpr (Just (cCallExpr name args )) undefNode)


cCallVars :: String -> [String] -> CBlockItem
cCallVars name args = let cvars = map (\a -> CVar (builtinIdent a) undefNode) args
                   in cCall name cvars


cIntReturn :: Integer -> CBlockItem
cIntReturn x = CBlockStmt (CReturn (Just (CConst (CIntConst (cInteger x) undefNode))) undefNode)

cMain :: Bool -> CExtDecl
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
  [ptrD, ptrD] Nothing [] undefNode),Nothing,Nothing)]
  undefNode],False)) [] undefNode] body


_ptrDecl :: String -> CDeclarator NodeInfo
_ptrDecl name = CDeclr (Just (builtinIdent name)) [ptrD] Nothing [] undefNode

_userPtrDecl ty name val = CBlockDecl (CDecl [typeSpec ty] [(Just (_ptrDecl name)
                        ,val, Nothing)] undefNode)

cInitCall :: String-> [CExpression NodeInfo] -> Maybe (CInitializer NodeInfo)
cInitCall name args =  Just (CInitExpr (cCallExpr name args) undefNode)

cNewHeapObj :: String -> String -> CBlockItem
cNewHeapObj name val = _userPtrDecl "Obj" name (cInitCall "stgNewHeapObj" [addrvarE val])

assign :: CExpression NodeInfo -> CExpression NodeInfo -> CBlockItem
assign lhs rhs = CBlockStmt (CExpr (Just (CAssign CAssignOp lhs rhs undefNode)) undefNode)

cStart :: CExtDecl
cStart = let body = [cCall "_POPVALS0" []
                    ,cCall "registerSHOs" []
                    ,_userPtrDecl "cont" "showResultCont"
                    (cInitCall "stgAllocCallCont"
                    [addrvarE "it_stgShowResultCont",intE 0])
#if USE_ARGTYPE
                    ,assign (memberE "stgCurVal" "argType" False) (cVarE "HEAPOBJ")
#endif
                    ,assign (memberE "stgCurVal" "op" False) (addrvarE "sho_main")
                    ,cCall "STGJUMP1"
                       [CMember (cCallExpr "getInfoPtr" [memberE "stgCurVal" "op" False])
                       (builtinIdent "entryCode") True undefNode, cVarE "stgCurVal"]
                    ]
         in cFun UserTy "FnPtr" "start" emptyFunDeclr body


-- PointerOrLiteral members
_polMembers(LitI i) = [
#if USE_ARGTYPE
                       cStructMember EnumTy "argType" "INT",
#endif
                       cStructMember IntTy "i" i]

_polMembers(LitL l) = [
#if USE_ARGTYPE
                       cStructMember EnumTy "argType" "LONG",
#endif
                       cStructMember IntTy "l" l]

_polMembers (LitF f) = [
#if USE_ARGTYPE
                       cStructMember EnumTy "argType" "FLOAT",
#endif
                       cStructMember FloatTy "f" f]

_polMembers (LitD d) = [
#if USE_ARGTYPE
                       cStructMember EnumTy "argType" "DOUBLE",
#endif
                       cStructMember DoubleTy "d" d]

_polMembers(LitC c) = [
#if USE_ARGTYPE
                       cStructMember EnumTy "argType" "INT",
#endif
                       cStructMember EnumTy "i" ("con_" ++ c)]

_polMembers(Var v) = error "Var Type in _polMembers"

cPoLE :: Atom -> CExpr
cPoLE a =  CCompoundLit (CDecl [typeSpec "PtrOrLiteral"] [] undefNode)
              (_polMembers a) undefNode

cHOTOPLshoE :: String -> CExpr
cHOTOPLshoE name = CCall (cVarE "HOTOPL") [CUnary CAdrOp (cVarE ("sho_" ++ name)) undefNode] undefNode

cHOTOPLE :: Int-> Int -> CExpr
cHOTOPLE x y = CCall (cVarE "HOTOPL") [CCast (CDecl [typeSpec "Obj"]
                [(Just (CDeclr Nothing [ptrD] Nothing [] undefNode),Nothing,Nothing)] undefNode)
                (CCall (cVarE "STGHEAPAT") [intE $ toInteger x, intE $ toInteger y] undefNode) undefNode] undefNode

cFVE :: Int -> CExpr
cFVE x = CIndex (CMember (CMember (cVarE "self") (builtinIdent "op") False undefNode)
          (builtinIdent "payload") True undefNode) (intE $ toInteger x) undefNode

cACE :: String -> Int -> CExpr
cACE v i = CIndex (memberE v "payload" True) (intE $ toInteger i) undefNode


cFnPtrFun :: String -> CExtDecl
cFnPtrFun name = funProto (typeSpec "FnPtr") ("fun_" ++ name)

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
