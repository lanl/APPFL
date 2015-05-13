
module Scanner (
  ScanTag(..),
  Lexeme,
  scannerp,
  scanner,
  stgSyms,
  uncomments, -- for debugging
) where

import ParseComb

-- the unary minus problem must be handled at the parsing level
data ScanTag = ScanNum
             | ScanSym
             | ScanIdent
             | ScanJunk  -- white space, could include comments
             deriving(Eq, Show)

type Lexeme = (ScanTag, [Char])

identp :: Parser Char String
identp = alphap `thenp` manyp alphanumus `usingp` uncurry (:)

identdecorp :: Parser Char String
identdecorp = 
--    identp `thenp` ((optlp (literalp '#'))
    identp `thenp` ((listsubp "#" "_h")
                    `altp` (manyp (literalp '\'')))       
                    `usingp` uncurry (++)

natnump :: Parser Char String
natnump = somep numeralp `thenp` (optlp (literalp '#'))
          `usingp` uncurry (++)
     
whitespacep :: Parser Char String
whitespacep = somep whitep


scp :: Parser Char [Char] -> ScanTag -> Parser Char Lexeme
(p `scp` t) inp = [((t,xs),out) | (xs,out) <- p inp]

scanp :: [(Parser Char [Char], ScanTag)] -> Parser Char [Lexeme]
scanp = manyp . (foldr op failp)
       where (p,t) `op` xs = (p `scp` t) `altp` xs

stgSyms :: [String]
stgSyms = ["->", "{", "}", ";", "\\", "(", ")", "=", "|"]

scannerp :: Parser Char [Lexeme]
scannerp = 
    scanp [ (natnump, ScanNum),
            (whitespacep,  ScanJunk),
            (identdecorp,  ScanIdent),
            (anymapp listp stgSyms, ScanSym)
          ]

scanner :: [Char] -> [Lexeme]
scanner = filter ((/=ScanJunk) . fst) . fst . head . scannerp . uncomments

-- hack
uncomments :: String -> String
uncomments = unlines . (map uncommentLine) . lines

-- ministg style: # in col 0 is comment
uncommentLine :: String -> String
uncommentLine [] = []
uncommentLine ('#':_) = []
uncommentLine xs = uncommentInline xs

-- haskell style: "--" rest of line is comment
uncommentInline :: String -> String
uncommentInline [] = []
uncommentInline ('-':'-':_) = []
uncommentInline (x:xs) = x : uncommentInline xs



