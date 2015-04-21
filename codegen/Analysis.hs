{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}

module Analysis(
--  isSimple,
  normalize,
) where

import Parser

{-
-- isSimple means will not do heap allocation
class IsSimple a where
  isSimple :: a -> Bool

instance IsSimple (Expr a) where
  isSimple EAtom{} = True
  isSimple EFCall{} = False        -- could be True pending deeper analysis
  isSimple EPrimop{} = True
  isSimple ELet{edefs, ee} = False -- assuming edefs is not empty
  isSimple ECase{ee, ealts} = isSimple ee && isSimple ealts

instance IsSimple (Alts a) where
  isSimple Alts{alts} = all isSimple alts

instance IsSimple (Alt a) where
  isSimple alt = isSimple $ ae alt
-}

-- normalize could do many things, for right now it provides a default
-- case alt if needed (in the absence of type information that means if
-- one is not present
-- NOTE:  while metadata is preserved, none is added so this should probably
--   be done early

class Normalize a where
  normalize :: a -> a --should that be a -> b? which won't work with field names?

instance Normalize (Expr a) where
  normalize e@EAtom{} = e
  normalize e@EFCall{} = e
  normalize e@EPrimop{} = e
  normalize e@ELet{ee, edefs} = e{ee = normalize ee, edefs = normalize edefs}
  normalize e@ECase{ee, ealts} = e{ee = normalize ee, ealts = normalize ealts}

instance Normalize (Alts a) where
  normalize a@Alts{alts} = 
    -- if no ADef add one
    -- if ADef followed by ACons we could remove them
    let alts' = if not (areExhaustive alts) then
                    alts ++ 
                   [ADef{amd = error "altsmd not initialized",
                         av = "x", 
                         ae = EFCall{emd = error "emd not initialized",
                                     ev = "stg_case_not_exhaustive", 
                                     eas = [Var "x"]}}]
                else alts
    in a{alts = map normalize alts'}

instance Normalize (Alt a) where
  normalize a = a{ae = normalize $ ae a}

instance Normalize (Obj a) where
  normalize o@FUN{e} = o{e = normalize e}
  normalize o@THUNK{e} = o{e = normalize e}
  -- PAP, CON, BH
  normalize o = o

instance Normalize [Obj a] where
  normalize = map normalize

areExhaustive = (any isConI) `pfOr` (any isADef)

isADef ADef{} = True
isADef _      = False

isConI ACon{ac} | ac == "I" = True
isConI _                    = False

pfOr p1 p2 x = (p1 x) || (p2 x)
