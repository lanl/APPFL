{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Rename (
  renameObjs,
  renameDefs
) where

import Prelude
import AST
import ADT
import State
import Data.Char (isDigit)

renameDefs :: [Def a] -> [Def a]
renameDefs = onObjs renameObjs

withsuff :: String -> [String] -> (String, String)
withsuff _ [] = error "this should never happen"
withsuff s (x:xs) = 
    let (h,t) = splitAt (length s) x in
    if h == s && t /= [] && head t == '_' && all isDigit (tail t) then
        (h, tail t)
    else withsuff s xs
          
nextv :: String -> [String] -> String
nextv s used =
    if not $ elem (s ++ "_0") used then s ++ "_0"
    else -- find most recent
        let (base, suff) = withsuff s used
        in base ++ "_" ++ show (read suff + 1)

-- only have to get into the monad one time, here
suffixname :: String -> State [String] String
suffixname v = State $ \used -> let nv = nextv v used in (nv, nv:used)

suffixnames :: [String] -> State [String] [String]
suffixnames = mapM suffixname

-- uniquify obj names, systematically renaming 

-- no need to flail in the monad at Atom level
nameAtom :: Atom -> [(Var, Var)] -> Atom
nameAtom (Var v) tt = Var $ condrepl v tt
nameAtom (Lit l) _ = Lit l

nameAtoms :: [Atom] -> [(Var, Var)] -> [Atom]
nameAtoms as tt = map ((flip nameAtom) tt) as

renameObjs :: [Obj a] -> [Obj a]
renameObjs objs =
  -- preserve top-level names--
  let (objs', _) = runState (topNameObjs objs []) []
  in objs'

-- this is the tricky one because they're all in the same letrec scope
-- need to figure out how to do circular programs with the state monad involved

topNameObjs :: [Obj a] -> [(Var, Var)] -> State [Var] [Obj a]
topNameObjs objs tt = mapM ((flip nameObj) tt) objs

-- do name substitution at [Obj] level so top-level names can be preserved

nameObjs :: [Obj a] -> [(String, String)] -> State [String] [Obj a]
nameObjs objs tt =
    do
      let names = map oname objs
      names' <- suffixnames names
      let objs' = [o {oname = name'} | (name',o) <- zip names' objs]
      let tt' = (zip names names') ++ tt
      objs'' <- mapM ((flip nameObj) tt') objs'
      return objs''

nameObj :: Obj a -> [(String, String)] -> State [String] (Obj a)
nameObj (FUN md vs e name) tt =
    do
      e' <- nameExpr e (dropall vs tt)
      return (FUN md vs e' name)

nameObj (THUNK md e name) tt =
    do
      e' <- nameExpr e tt
      return (THUNK md e' name)

nameObj (PAP md f as name) tt =
    let f' = condrepl f tt
        as' = nameAtoms as tt
    in
      return (PAP md f' as' name)

nameObj (CON md c as name) tt =
    return (CON md c (nameAtoms as tt) name)

nameObj (BLACKHOLE md name) tt = 
    return (BLACKHOLE md name)

nameExpr :: Expr a -> [(Var, String)] -> State [String] (Expr a)
nameExpr (ELet md objs e) tt =
    do
      let names = map oname objs
      objs' <- nameObjs objs tt
      let names' = map oname objs'
      e' <- nameExpr e ((zip names names')++tt)
      return (ELet md objs' e')

nameExpr (ECase md e alts) tt =
    do
      e' <- nameExpr e tt
      alts' <- nameAlts alts tt
      return (ECase md e' alts')

nameExpr (EAtom md a) tt =
    return $ EAtom md (nameAtom a tt)

nameExpr (EFCall md f as) tt =
    let f' = condrepl f tt
        as' = nameAtoms as tt in
    return (EFCall md f' as')

nameExpr (EPrimop md p as) tt =
    let as' = nameAtoms as tt in
    return (EPrimop md p as')

nameAlts :: Alts a -> [(Var, String)] -> State [String] (Alts a)
nameAlts (Alts md alts name) tt =
    do 
      name' <- suffixname name
      alts' <- mapM ((flip nameAlt) tt) alts
      return (Alts md alts' name')

nameAlt (ACon md c vs e) tt = 
    do
      e' <- nameExpr e (dropall vs tt)
      return (ACon md c vs e')

nameAlt (ADef md v e) tt = 
    do
      e' <- nameExpr e (dropall [v] tt)
      return (ADef md v e')

-- drop all references to vs in map tt
-- there's a fold in here...

dropall :: Eq b => [b] -> [(b, b1)] -> [(b, b1)]
dropall [] tt = tt
dropall _ [] = [] -- shortcut
dropall (v:vs) tt = dropall vs (filter ((/=v).fst) tt)

condrepl :: Eq b => b -> [(b, b)] -> b
condrepl v tt = case lookup v tt of
                  Just v' -> v'
                  Nothing -> v

