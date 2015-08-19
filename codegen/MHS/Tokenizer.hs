{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module MHS.Tokenizer
(
  Token(..),
  primopTable,
  tokenize
) where

import Tokenizer (primopTable)
import MHS.AST (Primop (..))
import Data.Char
import Data.List (isPrefixOf)
import ParserComb
import Debug.Trace
import PPrint hiding (space, hash)

testtok file =
  do
    lns <- readFile file
    let t1 = tokenize lns
    print $ vcat $ map (text.tks) t1

tokenize inp =
  let inp' = numberize inp
      toks = case prog inp' of
              [] -> error $ unlines
                    ["tokenization error, not sure where.."]
              x:xs -> fst x
      noLayout = rmLayout toks
  in rmWhitespace noLayout

{-     
numberize inp =
  let inp' = tabify inp
      lns = uncurry (:) $ foldr f ("\NUL",[]) inp
      f c (ln, lns) = if c == '\n'
                      then ("\n", ln : lns)
                      else (c:ln, lns)
      zipd = zip lns [1..]
      mapd = concatMap (\ (l,n) -> zip l (zip (repeat n) [1..])) zipd
  in mapd
-}

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
                           
satisfyFst f = satisfy (f.fst)

litC c = satisfyFst (== c)

litStr cs = case cs of
  [] -> accept ([], error "litStr passed empty arg")
  c:cs ->
    litC c >>> \(c,p) ->
    litStr cs >>> \(cs,_) ->
                   accept (c:cs, p)
                   
anyEq' cs = satisfyFst (\c -> or $ map (== c) cs)

prog = many' (whitespace `xorP` lexeme) `ordP` eof >>> \(tks, lst) ->
       accept (tks ++ [lst])

eof = litC '\NUL' >>> \(c,p) -> accept $ TokEOF p

whitespace = orExList [whtChrs, comment, haskBlock, ncomment]

whtChrs =
  let whtChr = orExList [newline, htab, space]
  in
   some' whtChr  >>> \((c,p):cs) ->
   accept $ TokWht (c:map fst cs) p

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
               accept $ TokWht (ds ++ (fst $ unzip tups) ++ [c]) p

opencom = litStr "{-"
closecom = litStr "-}"
haskDelim = litStr "{-#-}"


haskBlock =
  haskDelim >>> \(opn,p) ->
  many' (notP haskDelim) >>> \tups ->
  haskDelim >>> \(end,_) ->
                 accept $ TokWht (opn ++ map fst tups ++ end) p

ncomment =
  let inner = orExList
              [ ncomment >>> \(TokWht cs p) -> accept cs,
                notP closecom >>> \(c, p) -> accept [c] ]
  in
   opencom >>> \(op,p) ->
   many' inner >>> \chars ->
   closecom >>> \(cls,_) ->
                 accept $ TokWht (op ++ concat chars ++ cls) p

lexeme = orExList
         [ varid, conid, special, reserved, primitive, literal]


upper = satisfyFst isUpper
lower = satisfyFst isLower
underscore = litC '_'
apost = litC '\''
period = litC '.'
hash = litC '#'
digit = satisfyFst isDigit
idchar = orExList [upper, lower, digit, apost, hash]

special = orExList
          (map litStr specials) >>> \(s,p) ->
                                     accept $ TokRsv s p 
reserved = orExList
           (map litStr reserveds) >>> \(s,p) ->
                                       accept $ TokRsv s p

primitive = orExList
            (map litStr primops) >>> \(s,p) ->
                                      accept $ TokPrim s p
                                       
varid =
  lower >>> \(s,p) ->
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
                   accept $ TokCon (s:map fst ss) p


literal = orExList
          [floating, integral] >>> \(s,p) ->
                                    accept $ TokNum s p

decimal =
  some' digit >>> \((n,p):ns) ->
                   accept (n:map fst ns, p)

floating =
  decimal >>> \(ns1, p) ->
  period >>> \_ ->
  decimal >>> \(ns2, _) ->
  optP hash >>> \h ->
                 let end = maybe "" ((:[]) . fst) h
                 in accept$ (ns1 ++ '.' : ns2 ++ end, p)

integral =
  decimal >>> \(ns, p) ->
  optP hash >>> \h ->
                 let end = maybe "" ((:[]) . fst) h
                 in accept (ns ++ end, p) 
  

addDelims toks = chkFirst toks
  where

    -- check first lexeme  of program.
    -- If '{', then add brace indicator with appropriate column,
    -- else add nothing
    chkFirst toks = case toks of 
      t@TokWht{}:ts -> t:chkFirst ts -- keep looking for lex
      -- found lex, enter "main" auxiliary function
      TokRsv "{" (l,c):ts -> aux toks True
      t:ts -> aux (TokBrc (snd $ pos t):toks) False
      _ -> toks -- no lexemes in prog
    
    -- auxiliary function adds brace and semicolon indicators according
    -- to rules specified at section 10.3 of the haskell 2010 report
    -- see https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17800010.3
    -- TokBrc corresponds to {n} and TokSem corresponds to <n>
    -- boolean precBrc keeps track of whether the last non-white token read was
    -- an added TokBrc
    aux tks precBrc = case tks of
        
      (tk1:ts) | blkTok tk1 -> case notOpenBrcNxt ts of
                                Nothing -> tk1 : aux ts False
                                Just tk -> case tk of
                                            TokEOF{} -> tk1:TokBrc 0:[tk]
                                            _ -> tk1:TokBrc (snd (pos tk)): aux ts True
                                             
      (tk1:tk2:ts) | newWhite tk1 &&
                     not precBrc &&
                     isLex tk2 -> tk1: TokSem (snd (pos tk2)): aux (tk2:ts) False
             
      tk:ts | isLex tk -> tk : aux ts False
            | otherwise -> tk : aux ts precBrc

      [] -> []                              

    isLex tk = case tk of
      TokWht{} -> False
      TokBrc{} -> False
      TokSem{} -> False
      _ -> True
      
    blkTok tk = case tk of
      TokRsv s p -> s `elem` ["let", "of"]
      _ -> False
        
    notOpenBrcNxt tks = case tks of
      TokRsv "{" _:_ -> Nothing
      TokWht{}:tks -> notOpenBrcNxt tks
      t:tks -> Just t
      [] -> error "Tokenizer.rmLayout.notOpenBrcNxt, no EOF at end of program?"
      
    newWhite tk = case tk of
      TokWht s p -> '\n' `elem` s
      _ -> False
                            

rmLayout toks =
  let toks' = addDelims toks

      aux tt@(TokSem n:tks) (m:ms)
      -- same indentation of surrounding context: part of the block
      -- add delimiting semicolon, keep context stack
        | m == n  = TokRsv ";" (0,n) : aux tks (m:ms)
                    
      -- less indented than surrounding context: end the block,
      -- continue processing tokens with remainder of context stack
        | n < m   = TokRsv ";" (0,n) : TokRsv "}" (0,n) : aux tt ms

      -- (n > m or stack is empty) <=> part of the same "line" (defn/alt)
      -- don't add anything
      aux (TokSem n:tks) ms = aux tks ms

      -- greater indentation than surrounding context, add new context to stack
      -- and add brace to open block
      aux (TokBrc n:tks) (m:ms)
        | n > m   = TokRsv "{" (0,n) : aux tks (n:m:ms)

      -- no layout context (usually top level) and
      -- braces not added at top level (n > 0),
      -- set context, add brace
      aux (TokBrc n:tks) []
        | n > 0   = TokRsv "{" (0,n) : aux tks [n]

      -- indentation of next lex following a blocking lex is not greater than
      -- the enclosing context, must treat as empty block and carry indentation
      -- down as TokSem instead
      aux (TokBrc n:tks) ms = TokRsv "{" (0,n) : TokRsv "}" (0,n) : aux (TokSem n:tks) ms

      -- explicit open brace signaled by '0' on layout context stack, requires explicit close
      aux (t@(TokRsv "{" _):tks) ms = t : aux tks (0:ms)
      
      -- explicit closing brace matches explicit open brace
      aux (t@(TokRsv "}" _):tks) (0:ms) = t : aux tks ms

      -- explicit closing brace found while not in '0' state of layout context
      aux (TokRsv "}" _:tks) ms = error "closing brace found with no opening brace"


      -- done processing, base case
      aux [] [] = error "didn't see EOF"
      
      aux [] ms = error "didn't see EOF"
      
      aux [TokEOF{}] [] = []

      -- close any still open braces at end of token stream
      aux [t@TokEOF{}] (m:ms) = TokRsv "}" (0,m) : aux [t] ms
      
      -- no layout signaling token: keep token, stack, process remainder
      aux (t:tks) ms = t : aux tks ms
  in aux toks' []

rmWhitespace =
  let f tok = case tok of
               TokWht{} -> False
               _ -> True
  in filter f



type Pos = (Int, Int)
  
data Token = TokNum  {tks::String, pos::Pos}
           | TokPrim {tks::String, pos::Pos}
           | TokId   {tks::String, pos::Pos}
           | TokCon  {tks::String, pos::Pos}
           | TokRsv  {tks::String, pos::Pos}
           | TokWht  {tks::String, pos::Pos}
           | TokBrc  {col :: Int} -- added only when parsing layout indicators (rmLayout)
           | TokSem  {col :: Int} -- added only when parsing layout indicators (rmLayout)
           | TokEOF  {pos::Pos}
             deriving (Eq)

instance PPrint Pos where
  pprint (l,c) = parens (int l <> comma <+> int c)

instance PPrint Token where
  pprint tk = case tk of
    TokNum s p  -> text "TokNum" <> braces
                   (text s <> comma <+> pprint p)
    TokPrim s p -> text "TokPrim" <> braces
                   (text s <> comma <+> pprint p)
    TokId s p   -> text "TokId" <> braces
                   (text s <> comma <+> pprint p)
    TokCon s p  -> text "TokCon" <> braces
                   (text s <> comma <+> pprint p)
    TokRsv s p  -> text "TokRsv" <> braces
                   (text s <> comma <+> pprint p)
    TokWht s p  -> text "TokWht" <> braces
                   (text s <> comma <+> pprint p)
    TokEOF p    -> text "TokEOF" <> braces (pprint p)
    TokBrc col  -> text "TokBrc" <> braces (int col)
    TokSem col  -> text "TokSem" <> braces (int col)

instance Show Token where
  show (TokEOF p) = bracketize "EOF" (showpos p)
  show (TokBrc col) = bracketize "TokBrc" (show col)
  show (TokSem col) = bracketize "TokSem" (show col)
  show tok          = bracketize (tks tok) (showpos $ pos tok)
bracketize a b = "{" ++ a ++ " @ " ++ b ++ "}"
showpos (l,c) = "(ln:" ++ show l ++ "," ++ "col:" ++ show c ++ ")"

specials =
  [  "{", "}", "(", ")", ";", "|", "=", "\\", "::", "->"]

reserveds =
  [
    "case", "of", "let", "in", "data", "unboxed"
  ] ++ specials


primops = fst $ unzip primopTable
isReserved = flip elem reserveds
isPrimop = flip elem primops


tokErr t m = error $ show $
             text "Tokenization error with token" <+> pprint t $+$
             text m
