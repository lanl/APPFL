-- ast for stg like lang.

module AST
( Variable
, Constructor
, Literal(..)
, Atom(..)
, FunctionArity(..)
, Expression(..)
, Alternative(..)
, Object(..)
, Declaration(..)
, Program(..)
, Primitive(..)
, display
) where

import Data.List

-- Syntax from "Making a Fast Curry..." by Simon Marlow and Simon Peyton Jones
-- pg 

type Variable = String

type Constructor = String

data Literal = Int Int | Double Double deriving (Eq, Show, Read)

-- Literal or Varaible
data Atom = Literal Literal | Variable Variable deriving (Eq, Show, Read)

-- Deal with possible unknown arity
type FunctionArity = Maybe Int

data Expression = Atom Atom
                | FunctionCall Variable FunctionArity [Atom]
                | SatPrimCall Primitive [Atom] 
                | Let [(Variable,Object)] Expression
                | Case Expression [Alternative]
                deriving (Eq, Show, Read)
               
data Alternative = Alt Constructor [Variable] Expression
                 | DefaultAlt Variable Expression
                 deriving (Eq, Show, Read)
                 
data Object = FUN [Variable] Expression
            | PAP Variable [Atom]
            | CON Constructor [Atom]
            | THUNK Expression
            | BLACKHOLE
            | ERROR
            deriving (Eq, Show, Read)

data Declaration = Declaration Variable Object deriving (Show)

data Program = Program [Declaration] deriving (Show)

data Primitive = Add | Sub | Mul | Div 
               | Equal | NotEqual 
               | LessThan | GreaterThan 
               | LessThanOrEqual | GreaterThanOrEqual 
               | IntToBool deriving (Eq, Show, Read)

-- display functions
class Display a where display :: a -> String

instance Display Expression where
    display (Atom a) = display a
    display e = error ("Non atom expression " ++ show e)
    
instance Display Atom where
    display (Literal (Int x)) = show x
    display (Variable x) = x

instance Display Object where
    display (CON c as) 
        =  "(" ++ c ++ " " ++ intercalate " " [display a | a <- as] ++ ")"
    display e = error ("show non CON object " ++ show e)

