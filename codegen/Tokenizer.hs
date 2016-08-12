{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Tokenizer
(
  Token(..),
  tokenize,
  tokenizeWithComments,
) where

import MHS.AST 
import Data.Char
import Data.List (isPrefixOf)
import ParserComb
import Debug.Trace
import PPrint hiding (space, hash, underscore)

testtok file =
  do
    lns <- readFile file
    let t1 = tokenize lns
    print $ vcat $ map pprint t1

tokenize' inp keepComments =
  let inp' = numberize inp
      toks = case prog inp' of
              [] -> error $ unlines
                    ["tokenization error, not sure where.."]
              x:xs -> fst x
  in rmWhitespace keepComments toks

tokenize i = tokenize' i False

tokenizeWithComments i = tokenize' i True

numberize inp = aux inp (1,1)
  where aux [] ps = [('\NUL', ps)]
        aux (x:xs) (l,c) = case x of
          -- don't like tabs, replace with spaces
          '\t' -> let d = 8 - (mod (c-1) 8) -- default tab width
                      ps = zip (repeat l) [c..]
                      chrs = replicate d ' '
                  in zip chrs ps ++ aux xs (l, c+d)
          '\r' -> aux xs (l,c) -- ignore silly CR chars
          '\n' -> (x,(l,c)) : aux xs (l+1,1)
          _ | isControl x -> aux xs (l,c) -- ignore other control chars
            | otherwise -> (x, (l,c)) : aux xs (l,c+1)

failP msg parser inp =
    cutP (show (text "Tokenization Error at "
               <+> case inp of
                     []  -> text "end of input:"
                     i:_ -> pprint i <> char ':'
               $+$ text msg))
          parser inp

satisfyFst f = satisfy (f.fst)

litC c = satisfyFst (== c)

litStr cs = case cs of
  [] -> accept ([], error "litStr passed empty arg")
  c:cs ->
    litC c >>> \(c,p) ->
    litStr cs >>> \(cs,_) ->
                   accept (c:cs, p)

anyEq' cs = satisfyFst (\c -> or $ map (== c) cs)

prog = many' (whitespace `xorP` lexeme) `thenx` optP eof >>> \tks ->
       let (l,c) = maximum $ map pos tks
       in accept (tks ++ [TokEOF "EOF" (l,c+1)])

eof = litC '\NUL' >>> \(c,p) -> accept $ TokEOF "EOF" p

whitespace = orExList [whtChrs, comment, ncomment]

whtChrs =
  let whtChr = orExList [newline, htab, space]
  in
   some' whtChr  >>> \((c,p):cs) ->
   accept $ TokWht (c:map fst cs) p False

newline = orExList [winNL, lF, cR]
htab = litC '\t'
space = litC ' '
winNL = litStr "\r\n" >>> \(_,p) -> accept ('\n',p)
lF = litC '\n'
cR = litC '\r'

dashes =
  litStr "--" >>> \(ds1,p) ->
  many' (litC '-') >>> \tups ->
                        accept (ds1 ++ (fst $ unzip tups), p)

comment :: Parser (Char,Pos) Token
comment =
  dashes >>> \(ds, p) ->
  many' (notP newline) >>> \tups ->
  newline >>> \(c,_) ->
               accept $ TokWht (ds ++ (fst $ unzip tups) ++ [c]) p True

opencom = litStr "{-"
closecom = litStr "-}"

ncomment =
  let inner = orExList
              [ ncomment >>> \(TokWht cs p _) -> accept cs,
                notP closecom >>> \(c, p) -> accept [c] ]
  in
   opencom >>> \(op,p) ->
   many' inner >>> \chars ->
   failP "Expected end of nested comment" closecom >>> \(cls,_) ->
                 accept $ TokWht (op ++ concat chars ++ cls) p True

lexeme = orExList
         [varid, conid, special, primitive, reserved, literal]


upper = satisfyFst isUpper
lower = satisfyFst isLower
underscore = litC '_'
apost = litC '\''
period = litC '.'
hash = litC '#'
digit = satisfyFst isDigit
idchar = orExList [upper, lower, digit, apost, hash, underscore]

special = orExList
          (map litStr specials) >>> \(s,p) ->
                                     accept $ TokRsv s p
reserved = orExList
           (map litStr reserveds) >>> \(s,p) ->
                                       accept $ TokRsv s p

-- to be removed
primitive = orExList
            (map litStr primops) >>> \(s,p) ->
                                      accept $ TokPrim s p

varid =
  orExList [lower, underscore, hash] >>> \(s,p) ->
  many idchar >>> \ss ->
                   let str = s:map fst ss
                   in
                    case () of
                     _ | str `elem` reserveds -> reject
                       | str `elem` primops -> reject
                       | otherwise ->
                           accept $ TokId str p

conid =
  upper >>> \(s,p) ->
  many idchar >>> \ss ->
      let con = (s:map fst ss)
      in if con `elem` reserveds
         then reject
         else accept $ TokCon con p


literal =
  orExList [floating, integral] >>> \(s,p) ->
  failP "Expected no more than two '#' as a suffix to a numeric literal" $
  asManyAsP 2 hash >>> \ hs ->
                         accept $ TokNum (s ++ map fst hs) p

decimal =
  some' digit >>> \((n,p):ns) ->
                   accept (n:map fst ns, p)

floating =
  decimal >>> \(ns1, p) ->
  period >>> \_ ->
  decimal >>> \(ns2, _) ->
      accept $ (ns1 ++ '.' : ns2, p)

integral =
  decimal >>> \(ns, p) -> accept (ns, p)

rmWhitespace wComments =
  let f tok = case tok of
               TokWht{cmnt} -> (wComments && cmnt) || False
               _ -> True
  in filter f


type Pos = (Int, Int)

data Token
    = TokNum  {tks::String, pos::Pos}
    | TokPrim {tks::String, pos::Pos}
    | TokId   {tks::String, pos::Pos}
    | TokCon  {tks::String, pos::Pos}
    | TokRsv  {tks::String, pos::Pos}
    | TokWht  {tks::String, pos::Pos, cmnt::Bool}
    | TokEOF  {tks::String, pos::Pos}



instance PPrint Pos where
  pprint (l,c) = parens (int l <> comma <+> int c)

instance PPrint Token where
  pprint tk = case tk of
    TokNum s p   -> text "TokNum" <> braces
                   (text s <> comma <+> pprint p)
    TokPrim s p  -> text "TokPrim" <> braces
                   (text s <> comma <+> pprint p)
    TokId s p    -> text "TokId" <> braces
                   (text s <> comma <+> pprint p)
    TokCon s p   -> text "TokCon" <> braces
                   (text s <> comma <+> pprint p)
    TokRsv s p   -> text "TokRsv" <> braces
                   (text s <> comma <+> pprint p)
    TokWht s p _ -> text "TokWht" <> braces
                   (text s <> comma <+> pprint p)
    TokEOF s p   -> text "TokEOF" <> braces
                   (text s <> comma <+> pprint p)

instance Show Token where
    show tok = bracketize (tks tok) (showpos $ pos tok)

bracketize a b = "{" ++ a ++ " @ " ++ b ++ "}"
showpos (l,c) = "(ln:" ++ show l ++ "," ++ "col:" ++ show c ++ ")"

specials =
  [  "{", "}", "(", ")", ";", "|", "=", "->"]

reserveds =
  [
    "CON", "THUNK", "FUN", "PAP", "ERROR",
    "case", "of", "let", "in", "data", "unboxed"
  ] ++ specials


primops = fst $ unzip primOpTab
isReserved = flip elem reserveds
isPrimop = flip elem primops

isIdSym x = isAlphaNum x ||
            elem x "#_"

tokErr t m = error $ show $
             text "Tokenization error with token" <+> pprint t $+$
             text m
