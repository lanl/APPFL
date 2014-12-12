module Parser
( program
, declaration -- for testing
) where

-- stg like parser

import Parsing
import Lexer

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

sym :: [Char] -> Parser (Pos Token) [Char]
sym xs = literal (Symbol,xs) `using` snd

obj :: [Char] -> Parser (Pos Token) [Char]
obj xs = literal (Obj,xs) `using` snd

kind :: Tag -> Parser (Pos Token) [Char]
kind t = (satisfy ((==t).fst)) `using` snd


atom :: Parser (Pos Token) Atom
atom = (kind Number `using` numFN) `alt`
       (kind Ident `using` Variable)
 
numFN :: String -> Atom
numFN xs = Literal (Int (read xs :: Int))

-- only doing "CON"/"PAP" cases for now
object :: Parser (Pos Token) Object
object = ((obj "PAP" `xthen` sym "(" `xthen` kind Ident 
            `then'` some atom `thenx` sym ")") `using` papFN) 
         `alt`
         ((obj "CON" `xthen` sym "(" `xthen` kind Construct 
            `then'` many atom `thenx` sym ")") `using` conFN) 

funFN :: ([Variable], Expression) -> Object
funFN (vs, e) = FUN vs e

papFN :: (Variable, [Atom]) -> Object
papFN (v, as) = PAP v as

conFN :: (Constructor, [Atom]) -> Object
conFN (c,as) = CON c as 

declaration :: Parser (Pos Token) Declaration
declaration = (kind Ident `thenx` sym "=" `then'` object `thenx` sym ";") 
                `using` declFN

declFN :: (Variable, Object) -> Declaration
declFN (v,o) = Declaration v o

program :: Parser (Pos Token) Program
program = (some declaration) `using` Program

{-
expression :: Parser (Pos Token) Expression
expression =

functionCall :: Parser (Pos Token) Expression
functionCall =  
-}
