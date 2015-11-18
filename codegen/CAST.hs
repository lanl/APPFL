{-# LANGUAGE FlexibleInstances #-}

module CAST (
  Ty(..),
  ExtDecl,
  initEnum,
  initStruct,
  initStructMember,
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
initStructMemberE :: [String] -> CInitializer NodeInfo
  -> CInitializerMember NodeInfo
initStructMemberE name expr = 
  let f x = CMemberDesig (builtinIdent x) undefNode 
  in (map f name, expr) 

class InitStructMember a where
  initStructMember :: Ty -> String -> a -> CInitializerMember NodeInfo

instance InitStructMember String where
  initStructMember ty name val =         
    let e = case ty of
              EnumTy   -> CInitExpr (CVar (builtinIdent val) undefNode) undefNode 
              StringTy -> CInitExpr (CConst (CStrConst (cString val) undefNode)) undefNode
              PtrTy -> CInitExpr (CUnary CAdrOp (CVar (builtinIdent val) undefNode) undefNode) undefNode
              InfoPtrTy -> CInitExpr (CCast (CDecl [CTypeSpec (CTypeDef (builtinIdent "uintptr_t") undefNode)] [] undefNode) (CUnary CAdrOp (CVar (builtinIdent val) undefNode) undefNode) undefNode) undefNode
              -- empty/zero payload
              StructTy -> CInitList (if null val then [] else [([], CInitExpr (CConst (CIntConst (cInteger 0) undefNode)) undefNode)] ) undefNode
              _ -> error ("bad Type in initStructMember (String) " ++ show ty)
    in initStructMemberE (splitOn "." name) e

instance InitStructMember Int where
  initStructMember ty name val =         
    let e = case ty of
              IntTy -> CInitExpr (CConst (CIntConst (cInteger (toInteger val)) undefNode)) undefNode
              _ -> error "bad Type in initStructMember (Int)"
    in initStructMemberE (splitOn "." name) e

instance InitStructMember Double where
  initStructMember ty name val =         
    let e = case ty of
              DoubleTy -> CInitExpr (CConst (CFloatConst (cFloat (double2Float val)) undefNode)) undefNode
              _ -> error "bad Type in initStructMember (Double)"
    in initStructMemberE (splitOn "." name) e

instance InitStructMember Float where
  initStructMember ty name val =         
    let e = case ty of
              FloatTy -> CInitExpr (CConst (CFloatConst (cFloat val) undefNode)) undefNode
              _ -> error "bad Type in initStructMember (Float)"
    in initStructMemberE (splitOn "." name) e

instance InitStructMember [CInitializerMember NodeInfo] where
  initStructMember ty name val =         
    let e = case ty of
              StructTy -> CInitList [([],  CInitList val undefNode)] undefNode
              _ -> error "bad Type in initStructMember (Struct)"
    in initStructMemberE  (splitOn "." name) e
    
instance InitStructMember [[CInitializerMember NodeInfo]] where
  initStructMember ty name vals = 
    let e = case ty of
              StructTy -> CInitList (_initList vals) undefNode
              _ -> error "bad Type in initStructMember (Struct)"
    in initStructMemberE  (splitOn "." name) e    

_initList :: [CInitializerList NodeInfo] -> [([CPartDesignator NodeInfo], CInitializer NodeInfo)]
_initList (x:xs) = ([],(CInitList x undefNode)) : _initList xs
_initList [] = []
       
initStruct :: String -> String -> Bool -> [CInitializerMember NodeInfo] -> ExtDecl
initStruct ty name align is = 
  let t = [CTypeSpec (CTypeDef (builtinIdent ty) undefNode)]
      a = if align 
          then [CAttr (builtinIdent "aligned") [CConst (CIntConst (cInteger 8) undefNode)] undefNode] 
          else []
      d = CDeclr (Just (builtinIdent name)) [] Nothing a undefNode
      i = CInitList is undefNode
      in CDeclExt (CDecl t [(Just d, Just i, Nothing)] undefNode)


_initEnumList (x:xs) = (builtinIdent ("con_" ++ x) , Nothing) : _initEnumList xs
_initEnumList [] = []

initEnum :: String -> [String] -> ExtDecl
initEnum name xs = CDeclExt (CDecl [CTypeSpec (CEnumType (CEnum (Just (builtinIdent ("tycon_" ++ name)))
                   (Just (_initEnumList xs)) [] undefNode) undefNode)] [] undefNode)
-- examples

e2 = initEnum "Bool" ["False","True"]


cobj = initStruct "Obj" "sho_four" False 
        [initStructMember InfoPtrTy "infoPtr" "it_four"
        ,initStructMember EnumTy "objType" "CON"
        ,initStructMember StringTy "ident" "four"
        ,initStructMember StructTy "payload" 
            [initStructMember EnumTy "argType" "INT"
           ,initStructMember IntTy "i" (4 :: Int)
           ]
         ]

fobj = initStruct "Obj" "sho_eqInt" False 
        [initStructMember InfoPtrTy "infoPtr" "it_eqInt"
        ,initStructMember EnumTy "objType" "FUN"
        ,initStructMember StringTy "ident" "eqInt"
        ,initStructMember StructTy "payload" ""
        ]

tobj = initStruct "Obj" "sho_main" False 
        [initStructMember InfoPtrTy "infoPtr" "it_main"
        ,initStructMember EnumTy "objType" "THUNK"
        ,initStructMember StringTy "ident" "main"
        ,initStructMember StructTy "payload" "0"
        ]

info = initStruct "InfoTab" "it_false" True
         [initStructMember StringTy "name" "false"
         ,initStructMember PtrTy "entryCode" "stg_constructorcall"
         ,initStructMember EnumTy "objType" "CON"
         ,initStructMember IntTy "layoutInfo.payloadSize" (0 :: Int)
         ,initStructMember IntTy "layoutInfo.boxedCount" (0 :: Int)
         ,initStructMember IntTy "layoutInfo.unboxedCount" (0 :: Int)
         ,initStructMember StringTy "layoutInfo.permString" ""
         ,initStructMember IntTy "conFields.arity" (0 :: Int)
         ,initStructMember IntTy "conFields.tag" (0 :: Int)
         ,initStructMember StringTy "conFields.conName" "False"
         ]  

