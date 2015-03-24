module Lexer2 (

) where

import ParseComb
import Scanner

data KeyWord = KWlet 
             | KWin 
             | KWcase 
             | KWof 
               deriving(Eq,Show)

keywordtab = [ ("let",  KWlet),
               ("in",   KWin),
               ("case", KWcase),
               ("of",   KWof)
             ]

data Object = OFUN 
            | OPAP 
            | OCON 
            | OTHUNK 
            | OBLACKHOLE 
              deriving(Eq,Show)

objecttab = [("FUN",       OFUN),
             ("PAP",       OPAP),
             ("CON",       OCON),
             ("THUNK",     OTHUNK),
             ("BLACKHOLE", OBLACKHOLE)
            ]

data Symbol = SymArrow 
            | SymLParen 
            | SymRParen 
            | SymBind 
            | SymLBrace 
            | SymRBrace 
            | SymSemi
              deriving (Eq,Show)

symboltab = [("->", SymArrow),
             ("(",  SymLParen),
             (")",  SymRParen),
             ("=",  SymBind),
             ("{",  SymLBrace),
             ("}",  SymRBrace),
             (";",  SymSemi)
            ]

data Primop = Pplus 
            | Pminus 
            | Psub
            | Pmult
            | Peq
            | PintToBool
              deriving(Eq,Show)

primoptab = [("plus#",     Pplus),
             ("minus#",    Pminus),
             ("sub#",      Psub),
             ("mult

data Token = Number Int 
           | Ident String
           | KW Keyword
           | Ctor String
           | Obj Object
           | Sym Symbol
           | PO Primop
           deriving(Show, Eq)

lexer :: [Lexeme] -> [Token]
lexer = map trans

trans :: Lexeme -> Token
trans (ScanNum, str) = Number (read str)
trans (ScanSym, str) = Sym $ lookupassoc symboltab str

trans (ScanIdent, str) =
    if elem str ["FUN","PAP","CON","THUNK","BLACKHOLE"] then (Obj, str)
    else if elem str ["let", "in", "case", "of"] then (Keyword, str)
    else if isupper (head str) then (Ctor, str)
    else if (last str) == '#' then (Primop, str)
    else (Ident, str)

lookupassoc [] 



