module Lexer (
  Token(..),
  Keyword(..),
  Object(..),
  Symbol(..),
  lexer
) where

import Data.Char(isLower,isUpper)
import Scanner
import AST

data Keyword = KWlet 
             | KWin 
             | KWcase 
             | KWof 
             | KWdata
             | KWunboxed
               deriving(Eq,Show)

data Object = OFUN | OPAP | OCON | OTHUNK | OBLACKHOLE deriving(Eq,Show)

data Symbol = SymArrow 
            | SymLParen 
            | SymRParen 
            | SymBind 
            | SymLBrace 
            | SymRBrace 
            | SymSemi
            | SymPipe
              deriving (Eq,Show)
              
data Token = BIInt Int
           | BIDouble Double
           | Ident String
           | KW Keyword
           | Ctor String
           | Obj Object
           | Sym Symbol
           | PO Primop
           | BIT BuiltinType
           deriving(Show, Eq)

bigtab :: [(String, Token)]
bigtab = 
    [("->",        Sym SymArrow),
     ("(",         Sym SymLParen),
     (")",         Sym SymRParen),
     ("=",         Sym SymBind),
     ("{",         Sym SymLBrace),
     ("}",         Sym SymRBrace),
     (";",         Sym SymSemi),
     ("|",         Sym SymPipe),
     ("FUN",       Obj OFUN),
     ("CON",       Obj OCON),
     ("PAP",       Obj OPAP),
     ("THUNK",     Obj OTHUNK),
     ("ERROR",     Obj OBLACKHOLE),  -- for compatibility with BJP's ministg

     ("let",       KW KWlet), 
     ("in",        KW KWin), 
     ("case",      KW KWcase), 
     ("of",        KW KWof),
     ("data",      KW KWdata),
     ("unboxed",   KW KWunboxed),

     ("Int#",      BIT UBInt),
     ("Double#",   BIT UBDouble)
    ]
             

lexer :: [Char] -> [Token]
lexer = map trans . scanner

trans :: Lexeme -> Token
trans (ScanNum, str) | last str == '#' = BIInt (read $ init str)
                     | otherwise = BIInt (read str)

trans (ScanSym, str) = 
    case lookup str bigtab of
      Nothing -> error "trans error"
      Just s -> s

trans (ScanIdent, str) = 
    case lookup str bigtab of
      Just o -> o
      Nothing ->
          if isUpper (head str) then
                  Ctor str
          else if isLower (head str) then
                  Ident str
               else error $ "trans:  what is \"" ++ str ++ "\""
               
trans (ScanJunk, str) = error $ "trans: junk \"" ++ str ++ "\""



