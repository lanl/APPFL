{-# LANGUAGE NamedFieldPuns #-}

module Tokenizer
(
  Token
) where

import Data.Char
import Control.Monad
import Data.Maybe
import DavidParser
import AST


type Pos = (Int, Int)

data Rsrv = RCON | RTHUNK | RFUN | RPAP |
            RERROR {- error = ERROR in Prelude.stg, blackhole? -} |
            Arrow | RCase | ROf | RLet | RIn | RData| REq | RSC | RBar |
            LParen | RParen | LBrace | RBrace
          deriving (Show)
                                       
data Meta = MInt Int | MFlt Float | MPrimop Primop |
            MConstr String | MID String | MRsrv Rsrv | MEOF
          deriving (Show)

data Token = Tok {pos::Pos, meta::Meta}


data TokenState =  StrTok {tS::String, tP::Pos} |
                   NumTok {tS::String, tP::Pos} |
                   None
                deriving (Show)


instance Show Token where
  show (Tok p m) = '{' : show p ++ " " ++ show m ++ "}"

symTable =
  [
    ('{', LBrace),
    ('}', RBrace),
    ('(', LParen),
    (')', RParen),
    --('[', LSquare),
    --(']', RSquare),
    ('=', REq),
    (';', RSC),
    (chr 124, RBar)-- not using '|' because of weird haskell-mode syntax highlighting
  ] 

primopTable =
  [
    ("iplus#",   Piadd),
    ("isub#",    Pisub),
    ("imul#",    Pimul),
    ("idiv#",    Pidiv),
    --("imod#",    Pimod), -- commented pairs are based on inferrence from the pattern
    --("imax#",    Pimax), 
    ("ieq#",     Pieq)--,
    --("ine#",     Pine),
    --("ilt#",     Pilt),
    --("ile#",     Pile),
    --("igt#",     Pigt),
    --("ige#",     Pige),
    --("ineg#",    Pineg),           
    --("imin#",    Pimax),
    --("imax#",    Pimin),
  ]

rsrvTable =
  [
    ("CON",    RCON),
    ("THUNK",  RTHUNK),
    ("FUN",    RFUN),
    ("PAP",    RPAP),
    ("ERROR",  RERROR),
    ("case",   RCase),
    ("let",    RLet),
    ("in",     RIn),
    ("of",     ROf),
    ("data",   RData)
  ]   

symChars = fst . unzip $ symTable
isKeySym = (flip elem) symChars
metaSym c = MRsrv . fromJust . lookup c $ symTable
                                        
testTokenizer fileName = do
  file <- readFile fileName
  mapM_ (putStrLn.show) (tokenize file)
  return ()

tokenize :: String -> [Token]
tokenize ls = aux None (0,0) (stripComments ls) []
  where
    aux :: TokenState -> Pos -> String -> [Token] -> [Token]
    
    -- base case, finalize any token in progress, add EOF
    -- Should tokenizer expect a literal '\NUL' or assume usage of readFile or similar?
    aux st ps [] toks =
      let eof = Tok ps MEOF in
       case st of
        None -> reverse (eof:toks)
        _    -> reverse (eof:fromTokenState st:toks)

    -- recursive case, match on input, then tokenizing state
    aux st (l,c) xl@(x:xs) toks

    -- whitespace always terminates token-in-progress, otherwise is ignored
      | isSpace x =
        let newPs = if x == '\n' then (l+1,0) else (l,c+1) in
         case st of
          None -> aux None newPs xs toks
          _    -> aux None newPs xs (fromTokenState st:toks)


    -- reserved symbols defined in table above. They always terminate a token-in-progress
      | isKeySym x =
        let sym = Tok (l,c) (metaSym x) in
         case st of
          None -> aux None (l,c+1) xs (sym:toks)
          _    -> aux None (l,c+1) xs (sym:fromTokenState st:toks)
        

    -- digits start/continue numeric tokens, are valid inside constructors and identifiers
      | isDigit x =
        case st of
         None -> aux (NumTok [x] (l,c)) (l,c+1) xs toks
         _    -> aux st{tS=x:(tS st)} (l,c+1) xs toks -- accept digits mid-token

    -- alphabetic characters are only valid in Stringy things
      | isAlpha x =
        case st of
         None        -> aux (StrTok [x] (l,c)) (l,c+1) xs toks
         StrTok {tS} -> aux st{tS=x:tS} (l,c+1) xs toks
         NumTok _ _  -> error
                       ("\nError at " ++ show (l,c) ++
                        "\nexpected digit, '.', or number-terminating character." ++
                        "\nRead " ++ show x)


    -- single two-char non-alpha keyword (I think).  This could be generalized...
      | arrowHead xl = aux None (l,c+2) (tail xs) (Tok (l,c) (MRsrv Arrow):toks)


    -- only valid as a non-beginning part of an identifier or constructor
    -- if found outside of such, should it be read as a comment in the comment stripper?
      | x == '#' =
        case st of
        StrTok {tS} -> aux st{tS=x:tS} (l,c+1) xs toks
        _           -> error
                       ("\nError at " ++ show (l,c) ++
                        "\nNot expecting '#' outside some form of identifier")
        
    {- not sure how to handle this yet
    thoughts:
       in haskell: read ".12" :: Float --> no parse
                   1 + .12 --> compile time error
       is this due to the many roles '.' plays in haskell?
       (Module "accessor", function composition, numeric decimal point)
       GHC dumps STG code with *lots* of Module access via '.'
    -}        
      | x == '.' =
        case st of
        NumTok {tS} -> aux st{tS=x:tS} (l,c+1) xs toks
        _           -> error
                       ("\nError at " ++ show (l,c) ++
                        "\nNot expecting '.' outside numerals yet...")
      | otherwise = error
                    ("\nError at " ++ show (l,c) ++
                     "\nDidn't match " ++ show x)
   
    arrowHead ('-':'>':x) = True
    arrowHead _ = False
    fromTokenState st =
      case st of
      NumTok cs ps -> Tok ps (metaDigits (reverse cs))
      StrTok cs ps -> Tok ps (metaChars (reverse cs))
    metaDigits cs
      | elem '.' cs = MFlt (read cs :: Float)
      | otherwise = MInt (read cs :: Int)

    metaChars x = case lookup x rsrvTable of
                   Just s  -> MRsrv s
                   Nothing -> case lookup x primopTable of
                               Just s   -> MPrimop s
                               Nothing  -> case isUpper . head $ x of
                                            True  -> MConstr x
                                            False -> MID x
