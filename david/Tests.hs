import DavidParser
import Tokenizer
import PPrint
import CMap
import DAnalysis
import AST
import ADT









fromDataDef :: Def a -> TyCon
fromDataDef (DataDef t) = t

fromObjDef :: Def a -> Obj a
fromObjDef (ObjDef o) = o


testCMaps filename =
  do
    (_,dats) <- parseFile filename
    let cmap = toCMap $ map fromDataDef dats
    print $ toDoc cmap


testCaseExhaust filename =
  do
    (os,ds) <- parseFile filename
    let cmap = toCMap $ map fromDataDef ds
    let newTree = exhaustCases cmap $ map fromObjDef os
    print $ map toDoc newTree
