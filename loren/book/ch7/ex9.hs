import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w * b | (w, b) <- zip weights bits]
                where weights = iterate (*2) 1

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n`mod`2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = make9 (take 8 (bits ++ repeat 0))

chop9 :: [Bit] -> [[Bit]]
chop9 []   = []
chop9 bits = if check9 (take 9 bits) then take 8 bits : chop9 (drop 9 bits) else error "Bad Stuff" 

check9 :: [Bit] -> Bool
check9 bits = if (length [x|x<-(take 8 bits),x ==1] `mod` 2) == head (reverse bits) then True else False

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop9

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = tail

make9 :: [Bit] -> [Bit]
make9 xs |even (length [x|x<-xs,x ==1]) = xs ++ [0]
         |otherwise = xs ++ [1]
         
