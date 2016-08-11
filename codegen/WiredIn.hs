module WiredIn where

import ADT


tyVarsUsed :: Int
tyVarsUsed = 1
mkDirtyMVar n = MVar $ 't' : show n

intPrimTy         = MCon (Just False) intPrimTyName    []
longPrimTy        = MCon (Just False) longPrimTyName   []
floatPrimTy       = MCon (Just False) floatPrimTyName  []
doublePrimTy      = MCon (Just False) doublePrimTyName []
stringPrimTy      = MCon (Just False) stringPrimTyName []
charPrimTy        = MCon (Just False) charPrimTyName   []

charPrimTyName    = intPrimTyName
intPrimTyName     = "Int_h"
longPrimTyName    = "Long_h"
floatPrimTyName   = "Float_h"
doublePrimTyName  = "Double_h"
stringPrimTyName  = "String_h"

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

floatTyConName    = "Float"
floatTy           = MCon (Just True) floatTyConName [floatPrimTy]
floatTyCon        = TyCon True floatTyConName [] [floatDataCon]
floatDataConName  = "F_h"
floatDataCon      = DataCon floatDataConName [floatPrimTy]

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

builtinNames = [ charPrimTyName , intPrimTyName, longPrimTyName
               , floatPrimTyName, doublePrimTyName, stringPrimTyName
               , intTyConName, intDataConName, charTyConName, charDataConName
               , floatTyConName, floatDataConName, doubleTyConName, doubleDataConName
               , listTyConName, nilDataConName, consDataConName
               , noExhaustName, mainName
               ]
