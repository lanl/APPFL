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
import Debug.Trace
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



type Bounds a = (Set a, Set a)


class (Ord a) => Lattice a where
  ordering :: Map a (Bounds a)
  elems    :: [a]
  top, bot :: a
  elems = M.keys ordering

instance Lattice ListProj where
  top = ID
  bot = FAIL
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



allLTE :: Lattice a => a -> Set a
allLTE = cone fst
allGTE :: Lattice a => a -> Set a
allGTE = cone snd

-- select all elements GTE or LTE some element in a lattice.  "Cone" faces
-- upwards or downwards in the lattice, depending on the select function used
-- (`fst` or `snd`)
cone :: Lattice a => (Bounds a -> Set a) -> a -> Set a
cone select elem = maybe singl (runCone . select) $ M.lookup elem ordering
  where
    singl = S.singleton elem
    runCone set | [e] <- S.toList set, e /= elem =
                    error "Lattice reflexivity fails"
                | otherwise = let rest = S.unions . map (cone select) .
                                         S.toList $ S.delete elem set
                              in S.union singl rest

a `lte` b = a `S.member` allLTE b
a `gte` b = a `S.member` allGTE b

glb :: Lattice a => a -> a -> a
glb a b = head . sortBy ordGTE . S.toList $ S.intersection (allLTE a) (allLTE b)

lub :: Lattice a => a -> a -> a
lub a b = head . sortBy ordLTE . S.toList $ S.intersection (allGTE a) (allGTE b)

ordGTE, ordLTE :: Lattice a => a -> a -> Ordering
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
lu_cons ABS ABS     = STR
lu_cons ABS ID      = STR -- FNF ID

lu_cons STR FinFail = FinStr
lu_cons STR FinAbs  = FinId
lu_cons STR FinStr  = FinStr
lu_cons STR FinId   = FinId

lu_cons STR InfStr  = InfStr
lu_cons STR InfAbs  = InfId
lu_cons STR InfId   = InfId

lu_cons STR FnfStr  = FnfStr
lu_cons STR FnfAbs  = STR

lu_cons STR STR     = FinStr
lu_cons STR ABS     = FnfStr
lu_cons STR ID      = FnfStr

-- TODO: Review these.  Suspect some are not correct
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
lu_cons ID ABS      = undefined
lu_cons ID ID       = undefined

lu_cons a b = error $ "Strange CONS lookup? -- " ++ show a  ++ ", " ++ show b

-- HEAD (Cons a b) == a if b /= FAIL
lu_head ID  = ID  -- ABS U FNF ID :  Demand on the head can be no stronger than
lu_head ABS = ABS -- ABS U FAIL   :  the result.

lu_head STR  = ID   -- FNF ID
lu_head FAIL = FAIL -- Cons Fail Fail

lu_head FinFail = FAIL
lu_head FinAbs  = ABS
lu_head FinStr  = STR
lu_head FinId   = ID

lu_head InfStr  = STR
lu_head InfAbs  = ABS
lu_head InfId   = ID

lu_head FnfStr  = STR
lu_head FnfAbs  = ABS

-- TAIL (Cons a b) = b if a /= FAIL This is simpler, since most of our List
-- Projection domain (except Nil) is recursively defined with itself as the `b`
-- term in Cons.  The "funny" ones (FAIL, ABS) should also not change.
lu_tail FinFail = FAIL
lu_tail x = x

guard, (&)  :: ListProj -> ListProj -> ListProj
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
a       & b       = a `lub` b


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

      -- special case for the list constructor
      App "cons" [h,t] -> let hStr = varStr ctx smap h var
                              cStr = varStr ctx smap h var
                          in lu_head ctx & lu_tail ctx
      App f es ->
        let luFail = evalApp (repeat FAIL)
            evalApp pStrs = foldr (&) ABS $ zipWith recur pStrs es
            recur ctx exp = varStr ctx smap exp var
        in maybe luFail evalApp $ M.lookup (f, ctx) smap

      If p c a -> let cStr = varStr STR smap p var
                  in (cStr & varStr ctx smap c var) `lub`
                     (cStr & varStr ctx smap a var)

      Case scr nilB (hvar, tvar, consB) ->
        let scrNilStr = varStr FinFail smap scr var
            nilStr    = varStr FinFail smap nilB var
            consHStr  = varStr ctx smap consB hvar
            consTStr  = varStr ctx smap consB tvar
            consStrV  = if var /= hvar && var /= tvar
                        then varStr ctx smap consB var
                        else ABS
        in
          (scrNilStr & nilStr) `lub`
          (varStr (lu_cons consHStr consTStr) smap scr var & consStrV)


-------------------- TEST --------------------

prog :: [Def]
prog =
  [ ("fac", ["n"]) =: (If ("eq" $$ ["n","0"])
                       "0"
                       ("mul" $$ ["n",
                                  ("fac" $$ [("sub" $$ ["n", "1"])])])
                      )
  , ("strict", ["x"]) =: (If "True" "0" "x")
  , ("length", ["x"]) =: (Case "x"
                         "0" -- Nil
                         ("a", "as", "add" $$ ["1", "length" $$ ["as"]]))
  , ("before", ["x"]) =: (Case "x"
                         "nil" -- Nil
                         ("a", "as", If ("eq" $$ ["a", "0"])
                                     "nil"
                                     ("cons" $$ ["a", "before" $$ ["as"]])))
  ]

builtinMap :: DefStrMap
builtinMap = M.fromList
  [ (("eq"  , STR) , [STR , STR])
  , (("neq" , STR) , [STR , STR])
  , (("lt"  , STR) , [STR , STR])
  , (("lte" , STR) , [STR , STR])
  , (("gt"  , STR) , [STR , STR])
  , (("gte" , STR) , [STR , STR])
  , (("add" , STR) , [STR , STR])
  , (("sub" , STR) , [STR , STR])
  , (("mul" , STR) , [STR , STR])
  , (("div" , STR) , [STR , STR])
  , (("and" , STR) , [STR , STR])
  , (("or"  , STR) , [STR , STR])
  ]

progStr :: [Def] -> DefStrMap
progStr defs =
  let initMap = builtinMap `M.union` approx
      approx  = M.unions $ map mkApprox defs
      mkApprox d = M.fromList $
                   map (\s -> ((dname d, s), map (const FAIL) $ params d)) elems
      run ctx init = foldr (oneDef ctx) init defs
      oneDef ctx def smap = let newStrs = defStr def ctx smap
                            in M.insert ((dname def), ctx) newStrs smap
  in run STR (initMap `M.union` approx)
