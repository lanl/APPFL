
module Scanner (
  ScanTag(..),
  Lexeme,
  scannerp,
  scanner,
  stgSyms,
  uncomments, -- for debugging
) where

import ParseComb
import Data.Char(isAlpha,isDigit)


-- the unary minus problem must be handled at the parsing level
data ScanTag = ScanNum
             | ScanSym
             | ScanIdent
             | ScanJunk  -- white space, could include comments
             deriving(Eq, Show)

type Lexeme = (ScanTag, [Char])

identp = alphap `thenp` manyp alphaornumeralp `usingp` uncurry (:)

identdecorp = 
    identp `thenp` (       (listp "#")
                    `altp` (manyp (literalp '\'')))       
    `usingp` uncurry (++)

natnump = somep numeralp

whitespacep = somep whitep


scp :: Parser Char [Char] -> ScanTag -> Parser Char Lexeme
(p `scp` t) inp = [((t,xs),out) | (xs,out) <- p inp]

scanp :: [(Parser Char [Char], ScanTag)] -> Parser Char [Lexeme]
scanp = manyp . (foldr op failp)
       where (p,t) `op` xs = (p `scp` t) `altp` xs

stgSyms = ["->", "{", "}", ";", "\\", "(", ")", "="]

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
uncomments = unlines . (map uncomment) . lines

uncomment [] = []
uncomment ('#':xs) = []
uncomment (x:xs) | isAlpha x    = x : uncommenta xs
                 | otherwise    = x : uncomment xs

-- any character is allowed to follow an isAlpha
uncommenta [] = []
uncommenta (x:xs) | isAlpha x = x : uncommenta xs
                  | otherwise = x : uncomment xs


