module WiredIn where

import ADT


tyVarsUsed :: Int
tyVarsUsed = 1
mkDirtyMVar n = MVar $ 't' : show n

intPrimTy         = MPrim PInt    -- MCon (Just False) intPrimTyName    []
doublePrimTy      = MPrim PDouble -- MCon (Just False) doublePrimTyName []
stringPrimTy      = MPrim PString -- MCon (Just False) stringPrimTyName []
voidPrimTy        = MPrim PVoid
charPrimTy        = MCon (Just False) charPrimTyName   []

charPrimTyName    = intPrimTyName
intPrimTyName     = "Int#"
doublePrimTyName  = "Double#"
stringPrimTyName  = "String#"

intTyConName      = "Int"
intTy             = MCon (Just True) intTyConName [intPrimTy]
intTyCon          = TyCon True intTyConName [] [intDataCon]
intDataConName    = "I_h"
intDataCon        = DataCon intDataConName [intPrimTy]

charTyConName     = "Char"
charTy            = MCon (Just True) charTyConName [charPrimTy]
charTyCon         = TyCon True charTyConName [] [charDataCon]
charDataConName   = "C_h"
charDataCon       = DataCon charDataConName [charPrimTy]

doubleTyConName   = "Double"
doubleTy          = MCon (Just True) doubleTyConName [doublePrimTy]
doubleTyCon       = TyCon True doubleTyConName [] [doubleDataCon]
doubleDataConName = "D_h"
doubleDataCon     = DataCon doubleDataConName [doublePrimTy]

listTyConName     = "List"
listTy            = MCon (Just True) listTyConName [mkDirtyMVar 0]
listTyCon         = TyCon True listTyConName [] [nilDataCon, consDataCon]
nilDataConName    = "Nil"
nilDataCon        = DataCon nilDataConName []
consDataConName   = "Cons"
consDataCon       = DataCon consDataConName [mkDirtyMVar 0, listTy]

stringTy          = MCon (Just True) listTyConName [charTy]

noExhaustName = "stg_case_not_exhaustive"
mainName      = "main"

builtinNames = [ charPrimTyName , intPrimTyName, doublePrimTyName, stringPrimTyName
               , intTyConName, intDataConName, charTyConName, charDataConName
               , doubleTyConName, doubleDataConName
               , listTyConName, nilDataConName, consDataConName
               , noExhaustName, mainName
               ]
