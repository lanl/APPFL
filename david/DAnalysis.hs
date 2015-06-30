{-# LANGUAGE NamedFieldPuns #-}


module DAnalysis
(exhaustCases)
where

import ADT
import AST
import CMap



exhaustCases :: CMap -> [Obj a] -> [Obj a]
exhaustCases cmap = map (exhaustObj cmap)

exhaustObj :: CMap -> Obj a -> Obj a
exhaustObj cmap obj =
  case obj of
   ff@FUN{e} -> ff{e = exhaustExpr cmap e}
   tt@THUNK{e} -> tt{e = exhaustExpr cmap e}
   _ -> obj
   
exhaustExpr :: CMap -> Expr a -> Expr a
exhaustExpr cmap expr =
  case expr of
   el@ELet{edefs, ee} -> el{edefs = exhaustCases cmap edefs, ee = exhaustExpr cmap ee}
   ec@ECase{ealts} -> ec{ealts = exhaustAlts cmap ealts}
   _ -> expr

exhaustAlts :: CMap -> Alts a -> Alts a
exhaustAlts cmap aa@Alts{alts, aname} =
  let acons = filter isACon alts
      adefs = filter (not.isACon) alts
      isACon x = case x of ACon{} -> True; _ -> False
      newAlts = if not (null adefs) || consExhaust (map ac acons) cmap
                then alts
                else alts ++ [defAult "exh"]  --get it?
  in aa{alts = newAlts}


defAult :: String -> Alt a
defAult name =
  let
    mdErr s = error $ s ++ " metadeta not set!"
    fcal = EFCall{emd   = mdErr "EFCall",
                  ev    = "stg_case_not_exhaustive",
                  eas   = [Var "x"]}
    eatm = EAtom {emd   = mdErr "EAtom",
                  ea    = Var (name ++ "_exhaust")}
    thnk = THUNK {omd   = mdErr "THUNK",
                  e     = fcal,
                  oname = name ++ "_exhaust"}
    letx = ELet  {emd   = mdErr "ELet",
                  edefs = [thnk],
                  ee    = eatm}
  in ADef{amd = mdErr "ADef",
          av  = "x",
          ae  = letx}
