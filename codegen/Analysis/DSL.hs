{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings   #-}

module Analysis.DSL where

import GHC.Exts (IsString (..))
import Analysis.Language
import Data.Char     (isNumber, isUpper)

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


instance Definable [Constructor a] (DataDef a)
instance Definable (Expr ()) (ValDef ())

instance IsString ([Constructor a] -> DataDef a) where
  fromString s = DDef $ fromString s

instance IsString (Expr () -> ValDef ()) where
  fromString s = case words s of
    [b]  -> VDef (ID b (-1))
    x:xs -> \e -> VDef (ID x (-1)) $
                  Lambda (map (`ID` (-1)) xs) e ()
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
    ["_"] -> Default
    [x] | (dgs, "#") <- span isNumber x
          -> LitMatch (UBInt $ read dgs)
    x:xs -> ConMatch (ID x (-1)) $ map (`ID` (-1)) xs
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

(%) :: String -> [Type] -> Constructor a
s % tys = DCon (ID s (-1)) tys

infixr 1 .>
(.>) :: (Expr () -> () -> Expr ()) -> Expr () -> Expr ()
l .> e = l e ()



listDef :: DataDef a
listDef = datatype "List a" =: [ "Nil"  % []
                               , "Cons" % ["a", "List a"]
                               ]

builtin :: String -> ValDef ()
builtin s = fromString s =: "builtin"

facDef :: ValDef ()
facDef = "fac a#" =: letrec ["x" =: "0#"] 
         .> match "a#" "x#" 
         [ "0#" --> "1#"
         , "_"  --> letrec [] .> "fac" .$ "x#"
         ]



  
