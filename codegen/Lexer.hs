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

data Token = Number Int 
           | Ident String
           | KW Keyword
           | Ctor String
           | Obj Object
           | Sym Symbol
           | PO Primop
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

     ("plus#",     PO Piadd), 
     ("sub#",      PO Pisub),
     ("mult#",     PO Pimul),
     ("div#",      PO Pidiv),
     ("mod#",      PO Pimod),

     ("neg#",      PO Pineg),

     ("min#",      PO Pimax),
     ("max#",      PO Pimin),
    
     ("eq#",       PO Pieq),
     ("ne#",       PO Pine),
     ("lt#",       PO Pilt),
     ("le#",       PO Pile),
     ("gt#",       PO Pigt),
     ("ge#",       PO Pigt),

     ("intToBool#",PO PintToBool)
    ]
             

lexer :: [Char] -> [Token]
lexer = map trans . scanner

trans :: Lexeme -> Token
trans (ScanNum, str) | last str == '#' = Number (read $ init str)
                     | otherwise = Number (read str)

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
               
trans (ScanJunk, str) = error $ "trans: junk \"" ++ str ++ "\""

lookupassoc :: Eq a1 => [(a1, a)] -> a1 -> Maybe a
lookupassoc [] _ = Nothing
lookupassoc ((k',v):kvs) k | k == k' = Just v
                           | otherwise = lookupassoc kvs k



