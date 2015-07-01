{-# LANGUAGE NamedFieldPuns #-}


module Tokenizer
(
  Token(..),
  primopTable,
  tokenize,
  stripComments
) where

import AST
import Data.Char
import Control.Monad
import PPrint

------------------------- Comment Stripper ------------------------------
data ScanState =
  BComment {depth::Int, str::String} |
  LComment {str::String} |
  Quote {str::String} |
  Top {str::String}

instance Show ScanState where
  show (Top s) = "\nTOP::\n" ++ s
  show (BComment d s) = "\nBLOCK COMMENT::\n" ++ s
  show (LComment s) = "\nLINE COMMENT::\n" ++ s
  show (Quote s) = "\nQUOTE::\n " ++ s


-- CHANGED 6-16: Place appropriate whitespace in comment strings
-- This preserves line and column information for tokenizer
-- The choice to do the substitution here is for efficiency and simplicity
-- (probably better than traversing the list of data structures and
-- mapping the whitespace substitution appropriately)
readComments inp = aux [(Top "")] inp
  where
    -- Ending in Block Comment or Quote state implies uneven delimiter counts
    aux all@((BComment{}):xs) [] = error ("mismatched comment braces:\n" ++
                                          (concatMap show (reverse all)))
    aux all@((Quote{}):xs) []  = error ("mismatched quotes:\n" ++
                                        (concatMap show (reverse all)))

    -- base case
    aux all [] = reverse . map (\x->x{str = reverse (str x)}) $ all
    
    -- pattern match everything else based on accumulator head and beginning sequence of input
    -- Top level state: match comment start characters or double quote, then default
    aux all@(Top {}:xs) ('{':'-':cs) = aux (BComment 1 "  ":all) cs
    aux all@(Top {}:xs) ('"':cs) = aux (Quote "\"":all) cs
    aux all@(Top {}:xs) ('-':'-':cs) = aux (LComment "  ":all) cs
    aux (Top s:xs) (c:cs) = aux (Top (c:s):xs) cs

    -- Quote state: match escape sequence or terminating quote, then default
    aux (Quote s:xs) ('\\':c:cs) = case c of -- stupid escape characters
                                      '\\' -> aux (Quote ('\\':'\\':s):xs) cs
                                      '"'  -> aux (Quote ('"':'\\':s):xs) cs
                                      x    -> aux (Quote (x:'\\':s):xs) cs
    aux (Quote s:xs) ('"':cs) = aux (Top "":Quote ('"':s):xs) cs
    aux (Quote s:xs) (c:cs) = aux (Quote (c:s):xs) cs

    -- Block Comment state: match nested comment (increment depth) or terminating
    -- char sequence (decrementing depth appropriately or reverting to Top state)
    -- then default
    aux (BComment d s:xs) ('{':'-':cs) = aux (BComment (d + 1) (' ':' ':s):xs) cs
    aux (BComment d s:xs) ('-':'}':cs) = case d of
                                            1 -> aux (Top "":BComment 0 (' ':' ':s):xs) cs
                                            _ -> aux (BComment (d - 1) (' ':' ':s):xs) cs
    aux (BComment d s:xs) (c:cs) = case c of
                                    '\n' -> aux (BComment d ('\n':s):xs) cs
                                    _ -> aux (BComment d (' ':s):xs) cs

    -- Line Comment state : match EOL char to terminate, then default
    aux (LComment s:xs) ('\n':cs) = aux (Top "":LComment ('\n':s):xs) cs
    aux (LComment s:xs) (c:cs) = aux (LComment (' ':s):xs) cs


stripComments :: String -> String
stripComments = concatMap str . readComments



---------------------------- Tokenizer --------------------------------------------


type Pos = (Int, Int)
  
data Token = TokInt  {ivl::Int,    pos::Pos} |
             TokFlt  {fvl::Float,  pos::Pos} |
             TokPrim {tks::String, pos::Pos} |
             TokId   {tks::String, pos::Pos} |
             TokCon  {tks::String, pos::Pos} |
             TokRsv  {tks::String, pos::Pos} |
             TokEOF  {             pos::Pos}


data TokenState =  StrTok {tS::String, tP::Pos} |
                   NumTok {tS::String, tP::Pos} |
                   None
                deriving (Show)

instance PPrint Token where
  toDoc = text.show

instance Show Token where
  show (TokInt i p) = bracketize i $ showpos p
  show (TokFlt i p) = bracketize i $ showpos p
  show (TokEOF   p) = "{EOF @ " ++ showpos p ++ "}"
  show tok          = bracketize (tks tok) $ showpos (pos tok)
bracketize a b = "{" ++ show a ++ " @ " ++ b ++ "}"
showpos (l,c) = "(ln:" ++ show l ++ "," ++ "col:" ++ show c ++ ")"

primopTable =
  [
    ("iplus#",   Piadd),
    ("isub#",    Pisub),
    ("imul#",    Pimul),
    ("idiv#",    Pidiv),
    --("imod#",    Pimod),
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

reserveds =
  [
    "CON", "THUNK", "FUN", "PAP", "ERROR",
    "case", "of", "let", "in", "data", "unboxed",
    "{", "}", "(", ")", ";", "|", "="
  ]
  
primops = fst $ unzip primopTable
isReserved = flip elem $ reserveds
isPrimop = flip elem $ primops
                                                       
tokenize :: String -> [Token]
tokenize ls = aux None (0,0) (stripComments ls) []
  where
    aux :: TokenState -> Pos -> String -> [Token] -> [Token]
    
    -- base case, finalize any token in progress, add EOF
    -- Should tokenizer expect a literal '\NUL' or assume usage of readFile or similar?
    aux st ps [] toks =
      let eof = TokEOF ps in
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


    -- reserved defined in table above. Single chars always terminate a token-in-progress
      | isReserved [x] =
        let sym = TokRsv [x] (l,c) in
         case st of
          None -> aux None (l,c+1) xs (sym:toks)
          _    -> aux None (l,c+1) xs (sym:fromTokenState st:toks)
        

    -- digits start/continue numeric tokens, are valid inside constructors and identifiers
      | isDigit x =
        case st of
         None -> aux (NumTok [x] (l,c)) (l,c+1) xs toks
         _    -> aux st{tS = x:(tS st)} (l,c+1) xs toks -- accept digits mid-token

    -- alphabetic characters are only valid in Stringy things
      | isAlpha x =
        case st of
         None        -> aux (StrTok [x] (l,c)) (l,c+1) xs toks
         StrTok {tS} -> aux st{tS = x:tS} (l,c+1) xs toks
         NumTok _ _  -> error
                       ("\nError at " ++ show (l,c) ++
                        "\nexpected digit, '.', or number-terminating character." ++
                        "\nRead " ++ show x)


    -- only two-char non-alpha keyword (I think). This could be generalized for others
    -- might need a new class of token-in-progress, depending on the characters
      | arrowHead xl =
        let arw = TokRsv (take 2 xl) (l,c) in
         case st of
          None -> aux None (l,c+2) (tail xs) (arw:toks)
          _    -> aux None (l,c+2) (tail xs) (arw:fromTokenState st:toks)


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
                   read "12." :: Float --> no parse
                   1 + .12 --> compile error
                   1 + 12. --> compile xerror
                   
       is this due to the many roles '.' plays in haskell?
       (Module "accessor", function composition, numeric decimal point)
       GHC dumps STG code with *lots* of Module access via '.'
    -}        
      | x == '.' =
        case st of
        NumTok {tS} -> aux st{tS=x:tS} (l,c+1) xs toks
        _           -> error $
                       "\nError at " ++ show (l,c) ++
                       "\nNot expecting '.' outside numerals yet..."

      | otherwise = error $
                    "\nError at " ++ show (l,c) ++
                    "\nDidn't match " ++ show x
   
    arrowHead ('-':'>':x) = True
    arrowHead _ = False
    fromTokenState st =
      case st of
      NumTok cs ps  -> fromNum (reverse cs) ps
      StrTok cs ps -> fromString (reverse cs) ps
      _            -> error $ "should never see this... caused by " ++ show st

    fromNum cs
      | elem '.' cs = TokFlt (read cs :: Float) -- not really used?
      | otherwise = TokInt (read cs :: Int)

    fromString x
      | isReserved x = TokRsv x
      | isPrimop x = TokPrim x
      | isUpper $ head x = TokCon x
      | otherwise = TokId x
