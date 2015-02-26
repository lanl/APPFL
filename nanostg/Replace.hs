-- replace a variable w/ an atom in an expression
{-# LANGUAGE FlexibleInstances #-}

module Replace
( replace
, replaceMany
) where

import AST

type BoundVars = [Variable]

type Replacement = (Variable, Atom)

replaceMany :: [Replacement] -> BoundVars -> Expression -> Expression
replaceMany (r:rs) bvs e = replaceMany rs bvs e'
                         where e' = replace r bvs e
replaceMany [] _ e = e

class Replace a where replace ::  Replacement -> BoundVars -> a -> a

instance Replace Expression where
  replace r bvs (Atom a) 
    = Atom (replace r bvs a)

  replace r bvs (FunctionCall f k as) 
    = FunctionCall f k as'
    where as' = replace r bvs as 

  replace r bvs (SatPrimCall op as) 
    = SatPrimCall op as'
    where as' = replace r bvs as

  -- need to update boundvars
  replace r bvs (Let vos e) 
    = Let vos' e' 
    where vs = map fst vos 
          os = map snd vos
          bvs' = bvs ++ vs
          os' = [replace r bvs' o | o <-os]
          e' = replace r bvs' e
          vos' = zip vs os'

  replace r bvs (Case e alts) 
    = Case e' alts'
    where e' = replace r bvs e
          alts' = replace r bvs alts

instance Replace Atom where
  replace (var, atom) bvs (Variable x) | x == var && (notElem var bvs) = atom
  replace (var, atom) bvs x = x -- return original

instance Replace [Atom] where
  replace r bvs as 
    = [replace r bvs a | a <-as ]

instance Replace Variable where
  replace (vin, Variable vout) bvs v 
    = if v == vin && (notElem vin bvs) then vout else v

-- need to update boundvars
instance Replace Alternative where
  replace r bvs (DefaultAlt v e) 
    = DefaultAlt v e'
    where e' = replace r (v:bvs) e

  replace r bvs (Alt c vs e) 
    = Alt c vs e'
    where e' = replace r (vs++bvs) e

instance Replace [Alternative] where
  replace r bvs alts 
    = [replace r bvs alt | alt <-alts]

instance Replace Object where
  -- need update boundvars
  replace r bvs (FUN vs e) 
    = FUN vs (replace r (bvs++vs) e)

  replace r bvs (PAP f as) 
    = PAP f (replace r bvs as)

  replace r bvs (CON c as) 
    = CON c (replace r bvs as)

  replace r bvs (THUNK e) 
    = THUNK (replace r bvs e)

