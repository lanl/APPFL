{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SetFVs (
  setFVsDefs,
) where

import Prelude
import Parser
import Data.List

-- *****************************************************

-- after rename, make the fvs meaningful
-- TLDs are not considered free vars in expressions

setFVsDefs :: [Obj a] -> [Obj [Var]] -- monomorphism restriction
setFVsDefs defs = case setfvs (map oname defs) defs of
                    ([], defs') -> defs'
                    _ -> error "free variables in outermost scope!"


class FVs a where fvsof :: [Var] -> a -> [Var]

instance FVs Atom where
    fvsof tlds (Var v) = if elem v tlds then [] else [v]
    fvsof tlds (Lit l) = []

instance FVs [Atom] where
    fvsof tlds as = nub $ concatMap (fvsof tlds) as

-- setfvs both sets the metadata to the thing's fvs and returns the fvs
-- here "a" is e.g. "Expr ()" from the parser and "b" is "Expr [Var]"

class SetFVs a b where 
    setfvs :: [Var] -> a -> ([Var], b)

instance SetFVs (Expr a) (Expr [Var]) where
    setfvs tlds (EAtom _ a) = 
        let myfvs = fvsof tlds a \\ tlds
        in (myfvs, EAtom myfvs a)

    --  efcall introduces a var
    setfvs tlds (EFCall _ f as) = 
        -- nub superfluous if typed, \\ tlds just for f
        let myfvs = (nub $ f : (fvsof tlds as)) \\ tlds  
        in (myfvs, EFCall myfvs f as)

    setfvs tlds (EPrimop _ p as) = 
        let myfvs = fvsof tlds as 
        in (myfvs, EPrimop myfvs p as)

    -- let introduces scope
    setfvs tlds (ELet _ defs e) = 
        let (defsfvs, defs') = setfvs tlds defs 
            names = map oname defs'
            -- \\ not needed if objs have unique names
            (efvs, e') = setfvs (tlds \\ names) e  
            myfvs = nub $ defsfvs ++ (efvs \\ names)
        in (myfvs, ELet myfvs defs' e')

    setfvs tlds (ECase _ e alts) = 
        let (altsfvs, alts') = setfvs tlds alts
            (efvs, e') = setfvs tlds e
            myfvs = nub $ efvs ++ altsfvs
        in (myfvs, ECase myfvs e' alts')

-- alts introduce scope
instance SetFVs (Alt a) (Alt [Var]) where
    setfvs tlds (ACon _ c vs e) = 
        let (efvs, e') = setfvs (tlds \\ vs) e
            myfvs = efvs \\ vs
        in (myfvs, ACon myfvs c vs e')
    setfvs tlds (ADef _ v e) = 
        let (efvs, e') = setfvs (tlds \\ [v]) e
            myfvs = efvs \\ [v]
        in  (myfvs, ADef myfvs v e')

instance SetFVs (Alts a) (Alts [Var]) where
    setfvs tlds (Alts _ alts name) = 
        let (fvs, alts') = setfvs tlds alts
        in (fvs, Alts fvs alts' name)

instance SetFVs [Alt a] [Alt [Var]] where
    setfvs tlds alts =
        let (altsfvss, alts') = unzip $ map (setfvs tlds) alts
        in (nub $ concat altsfvss, alts')

-- FUN introduces scope
instance SetFVs (Obj a) (Obj [Var]) where
    setfvs tlds (FUN _ vs e n) = 
        let (efvs, e') = setfvs (tlds \\ vs) e -- shadow vs in tlds
            myfvs = efvs \\ vs
        in (myfvs, FUN myfvs vs e' n)

    -- PAP introduces a var
    setfvs tlds (PAP _ f as n) = 
        let asfvs = fvsof tlds as
            myfvs = (nub $ f : asfvs) \\ tlds -- like EFCall
        in (myfvs, PAP myfvs f as n)

    setfvs tlds (CON _ c as n) = 
        let myfvs = fvsof tlds as
        in (myfvs, CON myfvs c as n)

    setfvs tlds (THUNK _ e n) = 
        let (myfvs, e') = setfvs tlds e
        in (myfvs, THUNK myfvs e' n)

    setfvs tlds (BLACKHOLE _ n) = 
        ([], BLACKHOLE [] n)

--only interesting for non-recursive let
--instance SetFVs Def where
--    setfvs tlds (v, o) = (setfvs tlds o) \\ [v]

instance SetFVs [Obj a] [Obj [Var]] where
    setfvs tlds objs = 
        let vs = map oname objs 
            (osfvss, objs') = unzip $ map (setfvs tlds) objs
            osfvs = nub $ concat osfvss
        in (osfvs \\ vs, objs')

{-
instance SetFVs [Def a] [Def [Var]] where
    setfvs tlds defs = 
        let (vs, os) = unzip defs 
            (osfvss, os') = unzip $ map (setfvs tlds) os
            osfvs = nub $ concat osfvss
        in (osfvs \\ vs, zip vs os')
-}
