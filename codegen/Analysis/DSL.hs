{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE LiberalTypeSynonyms           #-}

module Analysis.DSL where

import           Analysis.Language
import           Analysis.Type
import           Data.Char         (isNumber, isUpper)
import           GHC.Exts          (IsString (..))

-- DSL for building programs until a parser is written:

class (IsString (a -> b)) => Definable a b | b -> a where
  (=:) :: String -> a -> b
  s =: b = fromString s $ b

infix 0 =:

datatype = id

match scrt bnd paths = CaseOf scrt bnd paths ()
as :: (ID -> [Clause ()] -> Expr ()) -> ID -> [Clause ()] -> Expr ()
as f id cls = f id cls

letrec :: [ValDef ()] -> Expr () -> () -> Expr ()
letrec = LetRec


instance Definable [DataDef a -> Constructor a] (DataDef a)
instance Definable (Expr ()) (ValDef ())

instance IsString ([DataDef a -> Constructor a] -> DataDef a) where
  fromString s conFs = let def = DDef (fromString s) $ map ($ def) conFs
                       in def

instance IsString (DataDef a -> Constructor a) where
  fromString s = \def -> DCon (ID s (-1)) [] def

instance IsString (Expr () -> ValDef ()) where
  fromString s = case words s of
    [b]  -> \e -> VDef (ID b (-1)) e ()
    x:xs -> \e -> VDef (ID x (-1)) (lam e) ()
      where lam e = Lambda (map (`ID` (-1)) xs) e ()
    []   -> error "VDef syntax error"

instance IsString Type where
  fromString s = case words s of
    ["I#"] -> TPrim PInt
    [x:xs] | isUpper x -> TApp (x:xs) []
           | otherwise -> TVar (x:xs)
    x:xs -> TApp x $ map fromString xs
    _ -> error "Type syntax error"

instance IsString (Expr () -> Clause ()) where
  fromString s = case words s of
    ["_"] -> (`Default` ())
    [x] | (dgs, "#") <- span isNumber x
          -> \e -> LitMatch (UBInt $ read dgs) e ()
    x:xs -> \ e -> ConMatch (ID x (-1)) (map (`ID` (-1)) xs) e ()
    []   -> error "Case clause syntax error"

instance IsString (Expr () -> Expr ()) where
  fromString s = \e -> Apply (fromString s) e ()

instance IsString (Expr ()) where
  fromString s = case words s of
    [x] | (dgs, "#") <- span isNumber x
          -> Lit (UBInt $ read dgs) ()
        | otherwise
          -> Var (ID x (-1)) ()
    x:xs -> let mkApp f st = Apply f (fromString st) ()
            in foldl mkApp (Var (ID x (-1)) ()) xs

    []   -> error "Expression syntax error"

instance IsString ID where
  fromString s = ID s (-1)

infixr 0 -->
(-->) :: (Expr () -> Clause ()) -> Expr () -> Clause ()
(-->) = ($)

infixr 1 .$
(.$) :: Expr () -> Expr () -> Expr ()
f .$ e = Apply f e ()

(%) :: String -> [Type] -> DataDef a -> Constructor a
s % tys = \def -> DCon (ID s (-1)) tys def

infixr 1 .>
(.>) :: (Expr () -> () -> Expr ()) -> Expr () -> Expr ()
l .> e = l e ()



listDef, boolDef :: DataDef a
listDef = datatype "List a" =: [ "Nil"
                               , "Cons" % ["a", "List a"]
                               ]
boolDef = datatype "Bool" =: [ "True" 
                             , "False"
                             ]

builtin :: String -> ValDef ()
builtin s = fromString s =: "builtin"

builtins = map builtin
  [ "builtin"
  , "add#"
  , "mul#"
  , "div#"
  ]

appendDef, foldrDef, subDef, facDef, broken :: ValDef ()
subDef = "sub# a b" =: "builtin a b"


facDef = "fac a#" =: match "a#" "x#"
         [ "0#" --> "1#"
         , "_"  --> letrec [] .> "mul# a#" .$ "fac" .$ "sub# 1# x#"
         ]

appendDef = "append xs ys" =: match "xs" "z"
            [ "Nil" --> "ys"
            , "Cons h t" --> "Cons h" .$ "append t ys"
            ]

foldrDef = "foldr f s l" =: match "l" "z"
           [ "Nil" --> "s"
           , "Cons h t" --> "f h" .$ "foldr f s t"
           ]

broken = "broken x" =: match "x" "z"
         [ "Nil" --> "Nil"
         , "Cons h t" --> "Cons" .$ "broken h" .$ "broken t"
         ]

prog = Prog
  ( builtins ++
    [
      facDef
    , subDef
    , foldrDef
--    , broken
--    , appendDef
    ]
  , [ listDef
    , boolDef
    ]
  )

uprog = uniquify prog
tprog = typecheck uprog
