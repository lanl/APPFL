{-# LANGUAGE
FlexibleInstances,
TypeSynonymInstances,
NamedFieldPuns #-}

module Transform where


import AST
import NewAST
import ADT
import qualified Data.Map as Map
import PPrint
import Util (mapSnd, deleteAll)



{--------------------------------------------------------------------
Setting user-defined type signatures in definitions that have them.
Could be expanded to set a type for the expression (and sub expressions)
in the definition.
---------------------------------------------------------------------}

type TypeMap = Map.Map Var Monotype

class SetTypeSigs a where
  setTypeSigs :: TypeMap -> a -> a

instance SetTypeSigs Defn where
  setTypeSigs tmap d = case d of
    ODefn{bnd, oexp} ->
      let oexp' = setTypeSigs tmap oexp
      in d {oexp = oexp',
            mmtype = Map.lookup bnd tmap}

    DDefn{} -> d
    TDefn{} -> d

instance SetTypeSigs Exp where
  setTypeSigs tmap e = case e of
    EAp{fexp, eexp} ->
      let fexp' = setTypeSigs tmap fexp
          eexp' = setTypeSigs tmap eexp
      in e {fexp = fexp',
            eexp = eexp'}

    EFn{eexp} ->
      let eexp'  = setTypeSigs tmap eexp
      in e {eexp = eexp'}

    ECs{eexp, cls} ->
      let eexp' = setTypeSigs tmap eexp
          cls'  = map (mapSnd (setTypeSigs tmap)) cls
      in e {eexp = eexp',
            cls  = cls'}

    ELt{eexp, defns} ->
      -- have to delete all new bindings from map. Type signatures bind only
      -- at the same "depth" in a program.
      -- Must also add new signatures, which are legal in let exprs
      let newBinds = map bnd defns
          tmap'    = Map.union (buildTMap defns) (deleteAll newBinds tmap)
          eexp'    = setTypeSigs tmap' eexp
          defns'   = map (setTypeSigs tmap') defns
      in e {eexp  = eexp',
            defns = defns'}

    EAt{} -> e

setTypes :: [Defn] -> [Defn]
setTypes defs = 
  let tmap = buildTMap defs
  in map (setTypeSigs tmap) defs


buildTMap :: [Defn] -> TypeMap
buildTMap defs =
  let (_, _, ts) = partitionDefs defs
  in Map.fromList $ map (\t -> (bnd t, mtyp t)) ts



