{-# LANGUAGE FlexibleInstances #-}

module CAST (
  Ty(..),
  ExtDecl,
  cEnum,
  cRegisterSHOs,
  cInfoTabStruct,
  cObjStruct,
  cStructMember,
) where

import Language.C.Parser
import Language.C.Data.Ident
import Language.C.Data.InputStream
import Language.C.System.Preprocess
import Language.C.System.GCC
import Language.C.DSL
import Data.List.Split
import GHC.Float

import AST

data Ty = EnumTy 
        | StringTy 
        | IntTy  
        | FloatTy
        | DoubleTy
        | PtrTy  
        | InfoPtrTy  
        | StructTy deriving(Show)

type CInitializerMember a = ([CPartDesignator a], CInitializer a)

type ExtDecl = CExternalDeclaration NodeInfo

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
              StructTy -> CInitList (_initList vals) undefNode
              _ -> error "bad Type in cStructMember (Struct)"
    in cStructMemberE  (splitOn "." name) e    

_initList :: [CInitializerList NodeInfo] -> [([CPartDesignator NodeInfo], CInitializer NodeInfo)]
_initList (x:xs) = ([],(CInitList x undefNode)) : _initList xs
_initList [] = []
       
cObjStruct :: String -> [CInitializerMember NodeInfo] -> ExtDecl
cObjStruct name xs = cStruct "Obj" ("sho_" ++ name) False xs  

cInfoTabStruct :: String -> [CInitializerMember NodeInfo] -> ExtDecl  
cInfoTabStruct name xs = cStruct "InfoTab" ("it_" ++ name) True xs    
       
cStruct :: String -> String -> Bool -> [CInitializerMember NodeInfo] -> ExtDecl
cStruct ty name align is = 
  let t = [CTypeSpec (CTypeDef (builtinIdent ty) undefNode)]
      a = if align 
          then [CAttr (builtinIdent "aligned") [CConst (CIntConst (cInteger 8) undefNode)] undefNode] 
          else []
      d = CDeclr (Just (builtinIdent name)) [] Nothing a undefNode
      i = CInitList is undefNode
      in CDeclExt (CDecl t [(Just d, Just i, Nothing)] undefNode)

cEnum :: String -> [String] -> ExtDecl
cEnum name xs = CDeclExt (CDecl [CTypeSpec (CEnumType (CEnum (Just (builtinIdent ("tycon_" ++ name)))
                   (Just (map (\y -> (builtinIdent ("con_" ++ y) , Nothing)) xs)) [] undefNode) undefNode)] 
                   [] undefNode)
                      
_cRegSHOExpr :: String -> CCompoundBlockItem NodeInfo                   
_cRegSHOExpr name =  CBlockStmt (CExpr 
  (Just (CAssign CAssignOp (CIndex (CVar (builtinIdent "stgStatObj") undefNode) 
  (CUnary CPostIncOp (CVar (builtinIdent "stgStatObjCount") undefNode) undefNode) 
  undefNode) (CUnary CAdrOp (CVar (builtinIdent ("sho_" ++ name)) undefNode) undefNode)
  undefNode)) undefNode)

cRegisterSHOs :: [String] -> ExtDecl
cRegisterSHOs names = CFDefExt (CFunDef [CTypeSpec (CVoidType undefNode)] (CDeclr (Just (builtinIdent "registerSHOs"))
  [CFunDeclr (Right ([],False)) [] undefNode] Nothing [] undefNode) [] (CCompound [] (map _cRegSHOExpr names) undefNode) undefNode) 
                  
                    
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

