{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}


module Analysis.KHL91 () where

import Prelude hiding ((+), (*))

import Analysis.Language
import qualified Data.Map as M

data Proj
  = Lift Proj        -- ^ Make a projection "lazy"
  | Bot              -- ^ Map entire domain to ABORT, like FAIL from WH87
  | Prod [Proj]      -- ^ Strict Product
  | Mu String [Proj] -- ^ Fixed point of a recursive Sum projection
  | Rec String       -- ^ Recursive term in a 'Mu' projection
  | Sum [Proj]       -- ^ Strict Sum
  deriving (Show, Eq)


pattern One = Prod []       -- Monomorphic Strict, e.g. Nil from WH87
pattern Ide = Lift Str      -- Polymorphic Identity
pattern Str = Sum [Prod []] -- Polymorphic Strict, maybe should just be Sum []?
pattern Abs = Lift Bot      -- Polymorphic Absence


-- Head Strict List
headStrList = Mu "List" [One, Prod [Str, Rec "List"]]

(.+) :: Proj -> Proj -> Proj
Sum a .+ Sum b = Sum $ a ++ b
Sum a .+ b     = Sum $ a ++ [b]
a     .+ Sum b = Sum $ a : b
a     .+ b     = Sum [a, b]

(.*) :: Proj -> Proj -> Proj
Prod a .* Prod b = Prod $ a ++ b
Prod a .* b      = Prod $ a ++ [b]
a      .* Prod b = Prod $ a : b
a      .* b      = Prod [a, b]



lub, (&) :: Proj -> Proj -> Proj
lub a b | a == b = a
lub x Bot = x
lub Bot x = x
lub x Abs = Lift x
lub Abs x = Lift x
lub (Sum xs)  (Sum ys)  = Sum $ zipWith lub xs ys
lub (Prod xs) (Prod ys) = Prod $ zipWith lub xs ys
lub (Lift a)  (Lift b)  = Lift $ a `lub` b
lub a         (Lift b)  = Lift $ a `lub` b
lub (Lift a)  b         = Lift $ a `lub` b
lub (Mu sa a) (Mu sb b) | sa == sb = Mu sa $ zipWith lub a b
lub a b = error $ "lub: " ++ show a ++ ", " ++ show b


a      & b | a == b = a
a      & Bot = Bot
Bot    & a = Bot
Lift a & Lift b = Lift $ a `lub` b
a      & Lift b = a `lub` (a & b)
Lift a & b      = b `lub` (b & a)
Sum xs & Sum ys = Sum $ zipWith (&) xs ys
Prod xs & Prod ys = Prod $ zipWith (&) xs ys
Mu sa a & Mu sb b | sa == sb = Mu sa $ map comb zipped
  where zipped = zip a b
        comb = undefined
                    
a & b | a == b = a
      | otherwise = error $
                    "(&): " ++ show a ++ ", " ++ show b





