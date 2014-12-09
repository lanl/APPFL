-- stg like parser

import Parser 

-- Syntax from "Making a Fast Curry..." by Simon Marlow and Simon Peyton Jones
-- pg 4

type Variable = String

type Constructor = String

-- Skip floating point for now
data Literal = Int Int deriving (Show)

-- Literal or Varaible
data Atom = Literal Literal | Variable Variable deriving (Show)

-- Deal with possible unknown arity
type FunctionArity = Maybe Int

data Expression = Atom Atom
                | FunctionCall Variable FunctionArity [Atom]
                | SatPrimCall Primitive [Atom] 
                | Let Variable Object Expression
                | Case Expression [Alternative]
                deriving (Show)
               
data Alternative = Alt Constructor [Variable] Expression
                 | DefaultAlt Variable Expression
                 deriving (Show)
                 
data Object = FUN [Variable] Expression
            | PAP Variable [Atom]
            | CON Constructor [Atom]
            | THUNK Expression
            | BLACKHOLE
            deriving (Show)

data Declaration = Declaration Variable Object deriving (Show)

data Program = Program [Declaration] deriving (Show)

data Primitive = Add | Sub | Mul | Div deriving (Show)

-- lit from parser4
sym :: [Char] -> Parser (Pos Token) [Char]
sym xs = literal (Symbol,xs) `using` snd

atom :: Parser (Pos Token) Atom
atom = (kind Number `using` numFN) `alt`
       (kind Ident `using` Variable)
 
numFN :: String -> Atom
numFN xs = Literal (Int (read xs :: Int))


{-
expression :: Parser (Pos Token) Expression
expression =



functionCall :: Parser (Pos Token) Expression
functionCall =  
-}




