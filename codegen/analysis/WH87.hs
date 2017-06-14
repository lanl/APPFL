{-# LANGUAGE OverloadedStrings #-}

module Analysis.WH87 where

import GHC.Exts (IsString(..))
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Function (on)
import Data.Ord (comparing)
import Data.List (intersectBy, nub, sortBy)
import Control.Applicative ((<|>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

{-

New construction of Strict part of List Domain projections

Assume Nil holds an unlifted nullary tuple to mirror the unlifted
binary tuple in Cons e.g.

  data IList = Nil (# #) | Cons (# Int, IList #)

(Note that this is valid Haskell with GHC's UnboxedTuples extension.)


With the introduction of FAIL and the Lightening Bolt (BOLT) point,
the nullary tuple is lifted, and the NIL projection should be treated
like CONS: as a generator of projections.

So, NIL is now defined

NIL a (Nil (# #)) = Nil (a (# #))
NIL _ _ = BOLT

and CONS is defined
CONS a B (Cons (# x, xs #)) = Cons (# a x, B xs #)
CONS _ _ _ = BOLT

so the FAIL projection arises more naturally:
  (λa. λB. μy. NIL a ∐ CONS B y) FAIL FAIL == FAIL

FNF a = NIL a U (CONS a (ABS U FNF a))
INF a = CONS (a (ABS U INF a))
FIN a = NIL a U (CONS a (FIN a))

-}


data Def = Def { dname  :: String
               , params :: [String]
               , def    :: Expr
               }
         deriving (Show)

instance Eq Def where
  a == b = a `compare` b == EQ
  
instance Ord Def where
  compare = comparing dname


data Expr
  = Var   { ename :: String}
  | Abort -- for partial functions
  | LInt  { ival  :: Int}
  | LBool { bval  :: Bool}
  | App   { fun   :: String
          , args  :: [Expr]}
  | If    { pred  :: Expr
          , consq :: Expr
          , alter :: Expr}
  | Case  { scrut :: Expr                   -- case e0 of
          , aNull :: Expr                   -- []   -> e1
          , aCons :: (String, String, Expr) -- x:xs -> e2
          }
  deriving (Show)


-- Convieniences for writing programs
instance IsString Expr where
  fromString s = fromMaybe (Var s) $
                 LBool <$> readMaybe s <|> LInt  <$> readMaybe s


(=:) :: (String, [String]) -> Expr -> Def
(name, args) =: e = Def name args e


($$) :: String -> [Expr] -> Expr
($$) = App

prog :: [Def]
prog =
  [ ("fac", ["n"]) =: (If ("eq" $$ ["n","0"])
                       "0"
                       ("mul" $$ ["n",
                                  ("fac" $$ [("sub" $$ ["n", "1"])])])
                      )
  ]



-- `Gen` is the generic projection generator for the strict part of the list
-- projection domain:
--
--   Gen a b c = Nil a U Cons b (c U Gen a b c)
--
-- We could add a fourth parameter `d` to catch the non-strict part by adding `d
-- U ..` to the front of the definition, where `d` comes from {FAIL, ABS}. There
-- is a clear 1:1 relationship between the two parts though.  It's worth
-- restricting `c` in the same way (can only be FAIL or ABS) since the
-- projections generated when `c` is ID or STR are not particularly interesting
-- thanks to the following properties:
--
--   STR U NIL a == STR U Cons b c == STR, for all a, b, c
--   ID U a = ID, for all a
--
-- Since the argument to NIL is a projection into a two-point domain, it must be
-- restricted to ID or FAIL.
--
-- Now we can enumerate all (2x4x2=16) possible argument sets to Gen and rewrite
-- them in terms of more readable generators:
--
--   FNF a = Gen ID a ABS
--         = Nil ID U Cons a (ABS U FNF a)
--   INF a = Gen FAIL a ABS
--         = Cons a (ABS U FNF a)
--   FIN a = Gen ID a FAIL
--         = Nil ID U Cons a (FIN a)
--
--   Gen FAIL FAIL FAIL = FAIL
--   Gen FAIL FAIL ABS  = FAIL
--   Gen FAIL ABS  FAIL = FAIL *
--   Gen FAIL STR  FAIL = FAIL *
--   Gen FAIL ID   FAIL = FAIL *
--
--   * These projections would recognize infinite lists as "acceptable" but
--     finite lists as "unacceptable", if they were not the same as FAIL
--
--   Gen FAIL ABS  ABS  = INF ABS  == "LazyIgnStream"
--     e.g. f x:xs = 3 : f xs
--
--   Gen FAIL STR  ABS  = INF STR  == "AccumUntil"
--     e.g. f x:xs = if x == 3 then True else f xs
--      or `head` (?)
--
--   Gen FAIL ID   ABS  = INF ID   == "LazyStream"
--     e.g. f p x:xs = if p then x else 0 : f xs
--
--   Gen ID   FAIL ABS  = FIN FAIL == NIL ID
--   Gen ID   FAIL FAIL = FNF FAIL == NIL ID
--     [] is the only acceptable arg
--
--   Gen ID   ABS  FAIL = FIN ABS  == "Spine"
--     e.g. length
--
--   Gen ID   ABS  ABS  = FNF ABS  == "InfSpine"
--     e.g. longerThan10 _ []   = False
--          longerThan10 l _:xs =
--            if l > 10 then True else longerThan10 (l + 1) xs
--
--   Gen ID   STR  FAIL = FIN STR  == "AccumAll"
--     e.g. sum
--
--   Gen ID   STR  ABS  = FNF STR  == "AccumSome"
--     e.g.  prodToZ []   = 1
--           prodToZ x:xs = if (x == 0) then 1 else x * prodToZ xs
--
--   Gen ID   ID   FAIL = FIN ID   == "AccumPartsOfAll"
--     e.g. sumEvenIdx _ [] = 0
--          sumEvenIdx i (h:t) = sumEvenIdx (i + 1) t +
--                               (if even i then h else 0)
--
--   Gen ID   ID   ABS  = FNF ID   == "WHNF" == STR
--     e.g. takeOneIf p [] = Nothing
--          takeOneIf p (x:_) = if p then Just x else Nothing


data ListProj
 -- Start with four point domain of projections, each of which can be
 -- found in some form in the full list projection domain.  
 = FAIL
   -- ^ maps x to BOLT for all x
   -- == Gen FAIL FAIL x == Gen FAIL x FAIL

 | STR
   -- ^ maps BOT and BOLT to BOLT, x to x for all other x
   -- == FNF ID == Gen ID ID ABS

 | ABS
   -- ^ maps BOT to BOT, x to x (including BOLT to BOLT) for all other x not
   -- part of the strict half of the list domain alone.  It only appears when
   -- ABS U Gen a b c is considered.
   -- == ABS U FAIL

 | ID
   -- ^ maps x to x for all x
   -- == ABS U STR

 -- Extra, List-particular projections
 
 | FinFail 
   -- ^ [] is the only acceptable argument
   -- == NIL ID
   
 | FinStr
   -- ^ Strict in elements and tail

 | FinAbs
   -- ^ Strict in list spine; never uses list elements
   -- e.g. length

 | FinId
   -- ^ may or may not use list elements, strict in spine
   -- e.g. a function that sums the elements at even indices?

 | InfStr
   -- ^ Cons is the only acceptable constructor, strict in list elements
   -- e.g. a function that counts elements until a particular element is found
   -- (without having a [] case)

 | InfAbs
   -- ^ Cons is the only acceptable constructor, does not use list elements
   -- e.g. something like map (const 0)

 | InfId
   -- ^ Cons is the only acceptable constructor, may or may not use list
   -- elements. No great examples of this.

 | FnfStr
   -- ^ Cons and Nil are OK, list may be infinite/partial, strict in elements
   -- e.g. takeWhile

 | FnfAbs
   -- ^ Cons and Nil are OK, list may be infinite/partial, elements are not used
   -- e.g. a function that checks if a list is longer than length n
 deriving (Eq, Show, Ord)

-- FAIL, STR, ABS, ID, FinFail, FinStr, FinAbs, FinId, InfStr, InfAbs, InfId, FnfStr, FnfAbs


class (Ord a) => MeetLattice a where
  ordering :: Map a (Set a, Set a)
  elems    :: [a]
  elems = M.keys ordering

instance MeetLattice ListProj where
  ordering = M.fromList $ map (fmap (\(l,g) -> (S.fromList l, S.fromList g)))
    [ (FAIL,
       ( [ FAIL]
       , [ FAIL, FinFail, InfStr, InfAbs]))
      
    , (ABS,
       ( [ ABS, FAIL]
       , [ ABS, ID]))
      
    , (ID,
       ( [ ID, STR, ABS]
       , [ ID]))
      
    , (STR,
       ( [ STR, FinId, InfId, FnfStr, FnfAbs]
       , [ STR, ID]))
      
    , (FinFail,
       ( [ FinFail, FAIL]
       , [ FinFail, FinStr, FinAbs]))
      
    , (FinStr,
       ( [ FinStr, FinFail]
       , [ FinStr, FinId, FnfStr]))
      
    , (FinAbs,
       ( [ FinAbs, FinFail]
       , [ FinAbs, FinId, FnfAbs]))
      
    , (InfStr,
       ( [ InfStr, FAIL]
       , [ InfStr, InfId, FnfStr]))
      
    , (InfAbs,
       ( [ InfAbs, FAIL]
       , [ InfAbs, InfId, FnfAbs]))

    , (FinId,
       ( [ FinId, FinAbs, FinStr]
       , [ FinId, STR]))
      
    , (FnfStr,
       ( [ FinStr, InfStr, FnfStr]
       , [ FnfStr, STR]))

    , (FnfAbs,
       ( [ FnfAbs, FinAbs, InfAbs]
       , [ FnfAbs, STR]))

    , (InfId,
       ( [ InfId, InfStr, InfAbs]
       , [ InfId, STR]))
    ]



allLTE :: MeetLattice a => a -> Set a
allLTE a = case M.lookup a ordering of
  Nothing -> S.singleton a
  Just (b, _) | [e] <- S.toList b, e /= a -> error "MeetLattice reflexivity fails"
              | otherwise -> S.union (S.singleton a) (S.unions . map allLTE . S.toList $ S.delete a b)

a `lte` b = a `S.member` allLTE b

glb :: MeetLattice a => a -> a -> a
glb a b = let lowerbounds = sortBy ordGTE . S.toList $ S.intersection (allLTE a) (allLTE b)
          in case lowerbounds of
               x:xs | all (`lte` x) xs -> x
               _                       -> error "No GLB in the MeetLattice!"


ordGTE, ordLTE :: MeetLattice a => a -> a -> Ordering
ordGTE = flip ordLTE
a `ordLTE` b = lte b a `compare` lte a b
--   (True , False) -> LT
--   (False, True ) -> GT
--   (x    , x    ) -> EQ




lu_cons FAIL _ = FAIL
lu_cons _ FAIL = FAIL

lu_cons ABS FinFail = FinAbs
lu_cons ABS FinAbs  = FinAbs
lu_cons ABS FinStr  = FinId
lu_cons ABS FinId   = FinId

lu_cons ABS InfStr  = InfId
lu_cons ABS InfAbs  = InfAbs
lu_cons ABS InfId   = InfId

lu_cons ABS FnfStr  = STR -- FNF ID
lu_cons ABS FnfAbs  = FnfAbs
lu_cons ABS STR     = STR -- FNF ID

lu_cons STR FinFail = FinStr
lu_cons STR FinAbs  = FinId
lu_cons STR FinStr  = FinStr
lu_cons STR FinId   = FinId

lu_cons STR InfStr  = InfStr
lu_cons STR InfAbs  = InfId
lu_cons STR InfId   = InfId

lu_cons STR FnfStr  = FnfStr
lu_cons STR FnfAbs  = STR
lu_cons STR STR     = STR

lu_cons ID FinFail  = FinId
lu_cons ID FinAbs   = FinId
lu_cons ID FinStr   = FinId
lu_cons ID FinId    = FinId

lu_cons ID InfStr   = InfId
lu_cons ID InfAbs   = InfId
lu_cons ID InfId    = InfId

lu_cons ID FnfStr   = STR
lu_cons ID FnfAbs   = STR
lu_cons ID STR      = STR

lu_cons a b = error $ "Strange CONS lookup? -- " ++ show a  ++ ", " ++ show b



guard, (&), (|.|)  :: ListProj -> ListProj -> ListProj
ID   `guard` x = ID -- pretty sure
ABS  `guard` x = ABS
FAIL `guard` x = FAIL
a    `guard` b = b

ABS     & b       = b
b       & ABS     = b
FAIL    & b       = FAIL
b       & FAIL    = FAIL
InfAbs  & FinFail = FAIL
InfStr  & FinFail = FAIL
InfId   & FinFail = FAIL
FinFail & InfAbs  = FAIL
FinFail & InfStr  = FAIL
FinFail & InfId   = FAIL
-- Cons a b & Cons c d = Cons (a & b) (c & d)
InfAbs  & InfStr  = InfStr
InfStr  & InfAbs  = InfStr
InfId   & InfAbs  = InfId
InfAbs  & InfId   = InfId
a       & b       = a |.| b

 


a |.| b = undefined

type DefStrMap = Map (String, ListProj) [ListProj]


usedIn :: String -> Expr -> Bool
usedIn v e = case e of
  Case sc e1 (hvar,tvar,e2) ->
    v `usedIn` e1 || v `usedIn` sc ||
    (v /= hvar && v /= tvar && v `usedIn` e2)
    
  If p c a -> v `usedIn` p || v `usedIn` c || v `usedIn` a
  Var a    -> v == a
  App f es -> f == v || any (v `usedIn`) es

  _ -> False

defStr :: Def -> ListProj -> DefStrMap -> [ListProj]
defStr (Def name params expr) ctx smap = map (varStr ctx smap expr) params

varStr :: ListProj -> DefStrMap -> Expr -> String -> ListProj
varStr ctx smap exp var
  | not (var `usedIn` exp) = ABS
  | otherwise = guard ctx $ case exp of
      Abort    -> FAIL
      Var _    -> ctx -- var must be *this* var
      LInt _   -> ABS
      LBool _  -> ABS
      App f es ->
        let luFail = error $ "can't find function in map?: " ++
                     f ++ " in " ++ show ctx
            evalApp pStrs = foldr (&) ABS $
                            zipWith (\ctx exp -> varStr ctx smap exp var) pStrs es
        in maybe luFail evalApp $
           M.lookup (f, ctx) smap

      If p c a -> varStr STR smap p var &
                  (varStr ctx smap c var |.| varStr ctx smap a var)

      Case scr nilB (hvar, tvar, consB) ->
        let scrNilStr = varStr FinFail smap scr var
            nilStr    = varStr FinFail smap nilB var
            consHStr  = varStr ctx smap consB hvar
            consTStr  = varStr ctx smap consB tvar
            consStrV  = if var /= hvar && var /= tvar
                        then varStr ctx smap consB var
                        else ABS
        in
          (scrNilStr & nilStr) |.|
          (varStr (lu_cons consHStr consTStr) smap scr var & consStrV)
        

      
  
  
