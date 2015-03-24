module Lexer (
  Token(..),
  Keyword(..),
  Object(..),
  Primop(..),
  Symbol(..),
  lexer
) where

import Data.Char(isLower,isUpper)
import Scanner

data Keyword = KWlet | KWin | KWcase | KWof deriving(Eq,Show)

data Object = OFUN | OPAP | OCON | OTHUNK | OBLACKHOLE deriving(Eq,Show)

data Primop = Pplus 
            | Psub 
            | Pmult
            | Peq
            | PintToBool
              deriving(Eq,Show)

data Symbol = SymArrow 
            | SymLParen 
            | SymRParen 
            | SymBind 
            | SymLBrace 
            | SymRBrace 
            | SymSemi
              deriving (Eq,Show)

data Token = Number Int 
           | Ident String
           | KW Keyword
           | Ctor String
           | Obj Object
           | Sym Symbol
           | PO Primop
           deriving(Show, Eq)

bigtab = 
    [("->",        Sym SymArrow),
     ("(",         Sym SymLParen),
     (")",         Sym SymRParen),
     ("=",         Sym SymBind),
     ("{",         Sym SymLBrace),
     ("}",         Sym SymRBrace),
     (";",         Sym SymSemi),
     ("FUN",       Obj OFUN),
     ("CON",       Obj OCON),
     ("PAP",       Obj OPAP),
     ("THUNK",     Obj OTHUNK),
     ("ERROR",     Obj OBLACKHOLE),  -- for compatibility with BJP's ministg
     ("let",       KW KWlet), 
     ("in",        KW KWin), 
     ("case",      KW KWcase), 
     ("of",        KW KWof),
     ("plus#",     PO Pplus), 
     ("sub#",      PO Psub),
     ("mult#",     PO Pmult),
     ("eq#",       PO Peq),
     ("intToBool#",PO PintToBool)
    ]
             

lexer :: [Char] -> [Token]
lexer = map trans . scanner

trans :: Lexeme -> Token
trans (ScanNum, str) = Number (read str)

trans (ScanSym, str) = 
    case lookupassoc bigtab str of
      Nothing -> error "trans error"
      Just s -> s

trans (ScanIdent, str) = 
    case lookupassoc bigtab str of
      Just o -> o
      Nothing ->
          if isUpper (head str) then 
              Ctor str
          else if isLower (head str) then
                   Ident str
               else error $ "trans:  what is \"" ++ str ++ "\""

lookupassoc [] k = Nothing
lookupassoc ((k',v):kvs) k | k == k' = Just v
                           | otherwise = lookupassoc kvs k



