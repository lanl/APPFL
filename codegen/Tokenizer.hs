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
import Data.List (isPrefixOf)
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

    aux states input = error "Tokenizer.readComments (aux): pattern fallthrough"


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
                   None   {            tP::Pos} -- useful to have position for error messages

instance PPrint TokenState where
  pprint StrTok {tS, tP} = brackets $ text "String-type token:" <+> text (show $ reverse tS) <+> text (showpos tP)
  pprint NumTok {tS, tP} = brackets $ text "Numeric-type token:" <+> text (show $ reverse tS) <+> text (showpos tP)
  pprint None{tP}        = brackets $ text "No token in progress:" <+> text (showpos tP)

instance PPrint Token where
  pprint = text.show

instance Show Token where
  show (TokInt i p) = bracketize (show i) (showpos p)
  show (TokFlt i p) = bracketize (show i) (showpos p)
  show (TokEOF   p) = bracketize "EOF" (showpos p)
  show tok          = bracketize (tks tok) (showpos $ pos tok)
bracketize a b = "{" ++ a ++ " @ " ++ b ++ "}"
showpos (l,c) = "(ln:" ++ show l ++ "," ++ "col:" ++ show c ++ ")"

primopTable =
  [
    ("iplus#",   Piadd),
    ("isub#",    Pisub),
    ("imul#",    Pimul),
    ("idiv#",    Pidiv),
    ("imod#",    Pimod),
    ("imax#",    Pimax), 
    ("ieq#",     Pieq),
    ("ine#",     Pine),
    ("ilt#",     Pilt),
    ("ile#",     Pile),
    ("igt#",     Pigt),
    ("ige#",     Pige),
    ("ineg#",    Pineg),           
    ("imin#",    Pimin),
    ("imax#",    Pimax)
  ]

specials =
  [  "{", "}", "(", ")", ";", "|", "=", "\\", "::", "->"]

reserveds =
  [
    "CON", "THUNK", "FUN", "PAP", "ERROR",
    "case", "of", "let", "in", "data", "unboxed"
  ] ++ specials
  
primops = fst $ unzip primopTable
isReserved = flip elem reserveds
isPrimop = flip elem primops

isIdSym x = isAlphaNum x ||
            elem x "#_"

tokErr t m = error $ show $
             text "Tokenization error with token" <+> pprint t $+$
             text m
                                                       
tokenize :: String -> [Token]
tokenize ls = aux (None (0,0)) (0,0) (stripComments ls) []
  where
    aux :: TokenState -> Pos -> String -> [Token] -> [Token]
    
    -- base case, finalize any token in progress, add EOF
    aux st ps [] toks =
      let eof = TokEOF ps in
       case st of
        None {} -> reverse (eof:toks)
        _       -> reverse (eof:fromTokenState st:toks)

    -- recursive case, match on input, then tokenizing state
    aux st (l,c) xl@(x:xs) toks

    -- whitespace always terminates token-in-progress, otherwise is ignored
      | isSpace x =
        let newPs = if x == '\n' then (l+1,0) else (l,c+1) in
         case st of
          None {} -> aux (None newPs) newPs xs toks
          _       -> aux (None newPs) newPs xs (fromTokenState st:toks)


    -- special string sequences always terminate token in progress
      | isSpecial xl =
        let
          sym   = getSpecialHead xl
          tok   = TokRsv sym (l,c)
          len   = length sym
          rest  = drop len xl
          newPs = (l,c+len)
        in
         case st of
          None {} -> aux (None newPs) newPs rest (tok:toks)
          _       -> aux (None newPs) newPs rest (tok:fromTokenState st:toks)


    -- special case for '.':
         -- must only appear in a NumTok once, not at the start or end
         -- cannot start a token (yet)
         -- currently 
      | x == '.' =
        case st of
        NumTok {tS} | elem '.' tS
                      -> tokErr st "Not expecting more than one '.' inside a numeric token"

                    | null xs || -- don't want to look for head at end of input
                      not (isDigit $ head xs) -- must be a number following the '.'
                      -> tokErr st "not expecting '.' to terminate a numeric token"
                      
                    | otherwise
                      -> aux st {tS = x:tS} (l,c+1) xs toks
                         
        _     ->  tokErr st "Not expecting '.' outside numeric types (yet)"

--        StrTok {tS} -> aux st {tS = x:tS} (l,c+1) xs toks

    -- digits start/continue numeric tokens, are valid inside constructors and identifiers
      | isDigit x =
        case st of
         None {} -> aux (NumTok [x] (l,c)) (l,c+1) xs toks

         -- accept digits mid-token, whether numeric or stringy
         _       -> aux st{tS = x:(tS st)} (l,c+1) xs toks 

    -- only valid as a non-beginning part of an identifier or constructor
    -- if found outside of such, should it be read as a comment in the comment stripper?
    -- MODIFIED 7.1, currently allowing '#' symbol in identifiers everywhere
--      | x == '#' =n
--        case st of
--        StrTok {tS} -> aux st{tS= x:tS} (l,c+1) xs toks
--        _           -> error
--                       ("\nError at " ++ show (l,c) ++
--                        "\nNot expecting '#' outside some form of identifier")

    -- match on valid identifier symbols
      | isIdSym x =
        case st of
         None {}     -> aux (StrTok [x] (l,c)) (l,c+1) xs toks
         StrTok {tS} -> aux st{tS = x:tS} (l,c+1) xs toks
         NumTok _ _  -> tokErr st $
                        "expected digit, '.', or number-terminating character. Got " ++ show x

       
    -- give some kind of meaningful error message if something unexpected is read
      | otherwise = tokErr st $
                    "Couldn't match " ++ show x
   
    isSpecial xs = any (`isPrefixOf` xs) specials
    getSpecialHead xs = head $ filter (`isPrefixOf` xs) specials
    


    fromTokenState st =
      case st of
      NumTok cs ps  -> fromNum (reverse cs) ps
      StrTok cs ps  -> fromString (reverse cs) ps
      _             -> tokErr st $
                       "should never see this error message\n" ++
                       "trying to parse Token from invalid TokState object."

    fromNum cs
      | elem '.' cs = TokFlt (read cs :: Float) -- not really used?
      | otherwise = TokInt (read cs :: Int)

    fromString x
      | isReserved x = TokRsv x
      | isPrimop x = TokPrim x
      | isUpper $ head x = TokCon x
      | otherwise = TokId x
