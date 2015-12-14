{-# LANGUAGE BangPatterns #-}
module STGbits (
  npStrToBMStr
) where

-- C Bitmap64
-- [ unsigned 6-bit size | _ _ _ _ _ _ _ _ 1 0 1 ]
--
-- "---NPN" should give [ 3 | _ _ _ _ _ _ 0 1 0 0 0 0 ]
-- that is, string index 0 is bit index 0 from LSB
--                                             ^- leftmost in Payload
-- this make it easy for disjoint bitmaps to be added to obtain union

import Data.Word
import Data.Bits

abet = ['0'..'9'] ++ ['A'..'F'] -- no we are not exceeding base 16

indexOf :: (Eq a, Num a1, Show a) => a -> [a] -> a1
indexOf k xs = f xs 0
    where f []      _             = error $ "indexOf " ++ 
                                            show k ++ " not found"
          f (x:_)  !n | k == x    = n
          f (_:xs) !n | otherwise = f xs (n+1)

-- NOTE string index 0 is LSB!
strBaseNToWord64 :: String -> Int -> Word64
strBaseNToWord64 s b = f (reverse s) 0
    where
      lbet = take b abet -- slight sanity checking
      f ""     !n = n
      f (x:xs) !n = f xs ((fromIntegral b) * n + indexOf x lbet)

-- "NP string to bitmap C unsigned long string"
npStrToBMStr s = 
    let offset = length $ takeWhile (=='-') s
        bitstr = dropWhile (=='-') s
        len = length bitstr
        nptobin c = case c of 'N' -> '0'
                              'P' -> '1' 
                              _ -> error "not N or P"
        bin = map nptobin bitstr
        w1 = shiftL (strBaseNToWord64 bin 2) offset
        w2 = shiftL (fromIntegral len :: Word64) 58
    in word64ToCULHex (w1 .|. w2)

-- unneeded

word64ToBaseNStr :: Word64 -> Int -> String
word64ToBaseNStr 0 b = [abet !! 0]  -- degenerate case, no empty strings
word64ToBaseNStr n b = reverse $ f n
    where
      f :: Word64 -> String
      f 0 = ""
      f n = (abet !! fromIntegral (n `mod` (fromIntegral b))) : 
             f (n `div` (fromIntegral b))

strBaseNToM :: String -> Int -> Int -> String
strBaseNToM s n m = word64ToBaseNStr (strBaseNToWord64 s n) m

word64ToCULHex w64 =
    let hex = word64ToBaseNStr w64 16
    in "(Bitmap64)0x" ++ take (16 - length hex) (repeat '0') ++ hex ++ "UL"



