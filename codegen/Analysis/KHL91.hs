{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module Analysis.KHL91 () where

import Prelude hiding ((+), (*))

import Analysis.Language
import qualified Data.Map as M

data Proj
  = Str Proj
  | Lift Proj
  | Bot
  | Prod [Proj]
  | Mu String Proj
  | Rec String
  | Sum [Proj]
  deriving (Show, Eq)


pattern One = Prod []  -- Monomorphic Identity, e.g. Nil from WH87
pattern Ide = Prod []  -- Polymorphic Identity
pattern Abs = Lift Bot -- Right?


-- Head Strict List
headStrList = Mu "List" (Sum [One, Prod [Str Ide, Rec "List"]])


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
lub x Bot = x
lub Bot x = x
lub x Ide = Ide
lub Ide x = Ide
lub x Abs = Lift x
lub Abs x = Lift x
lub (Sum xs)  (Sum ys)  = Sum $ zipWith lub xs ys
lub (Prod xs) (Prod ys) = Prod $ zipWith lub xs ys
lub (Lift a)  (Lift b)  = Lift $ a `lub` b
lub (Str a)   (Lift b)  = Lift $ a `lub` b
lub (Lift a)  (Str b)   = Lift $ a `lub` b
lub (Str a)   (Str b)   = Str  $ a `lub` b
lub (Mu sa a) (Mu sb b) | sa == sb = Mu sa (a `lub` b)
lub a b | a == b = a
        | otherwise = error $
                      "lub: " ++ show a ++ ", " ++ show b


a      & Bot = Bot
Bot    & a = Bot
Lift a & Abs = Lift a
Abs    & Lift a = Lift a
Str a  & Str b  = Str $ a & b
Str a  & Lift b = Str $ a `lub` (a & b)
Lift a & Str b  = Str $ b `lub` (b & a)
Lift a & Lift b = Lift $ a `lub` b
Sum xs & Sum ys = Sum $ zipWith (&) xs ys
Prod xs & Prod ys = Prod $ zipWith (&) xs ys
Mu sa a & Mu sb b | sa == sb =
                    undefined -- TODO
a & b | a == b = a
      | otherwise = error $
                    "(&): " ++ show a ++ ", " ++ show b
