{-# LANGUAGE BangPatterns #-}
module STGbits (
  pnStrToBMCULStr
) where

import Data.Word
import Data.Bits

abet = ['0'..'9'] ++ ['A'..'F'] -- no we are not exceeding base 16

indexOf :: (Eq a, Num a1, Show a) => a -> [a] -> a1
indexOf k xs = f xs 0
    where f []      _             = error $ "indexOf " ++ show k ++ " not found"
          f (x:_)  !n | k == x    = n
          f (_:xs) !n | otherwise = f xs (n+1)

word64ToBaseNStr :: Word64 -> Int -> String
word64ToBaseNStr 0 b = [abet !! 0]  -- degenerate case, no empty strings
word64ToBaseNStr n b = reverse $ f n
    where
      f :: Word64 -> String
      f 0 = ""
      f n = (abet !! fromIntegral (n `mod` (fromIntegral b))) : f (n `div` (fromIntegral b))

strBaseNToWord64 :: String -> Int -> Word64
strBaseNToWord64 s b = f s 0
    where
      lbet = take b abet -- slight sanity checking
      f ""     !n = n
      f (x:xs) !n = f xs ((fromIntegral b) * n + indexOf x lbet)

strBaseNToM :: String -> Int -> Int -> String
strBaseNToM s n m = word64ToBaseNStr (strBaseNToWord64 s n) m

word64ToCULHex w64 =
    let hex = word64ToBaseNStr w64 16
    in "0x" ++ take (16 - length hex) (repeat '0') ++ hex ++ "UL"

-- "PN string to bitmap C unsigned long string"
pnStrToBMCULStr s = 
    let len = length s
        nptobin c = case c of 'N' -> '0'; 'P' -> '1'; _ -> error "not N or P"
        bin = map nptobin s
        w1 = strBaseNToWord64 bin 2
        w2 = shiftL (fromIntegral len :: Word64) 58
    in word64ToCULHex (w1 .|. w2)




