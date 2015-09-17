import Data.List
import Data.Char
type Board = [Int]

start :: Board
start = [5,4,3,2,1]

test :: Board 
test = [-1,-2,3,4,-6]

cls :: IO()
cls = putStr "\ESC[2J"

printBoard :: (Num a, Ord a) => [a] -> IO ()
printBoard b = do putStr "1: "
                  printStar (b!!0)
                  putStr "2: "
                  printStar (b!!1)
                  putStr "3: "
                  printStar (b!!2)
                  putStr "4: "
                  printStar (b!!3)
                  putStr "5: "
                  printStar (b!!4)

printStar :: (Num a, Ord a) => a -> IO ()
printStar x | x <= 0 = putStrLn ""
            | otherwise = do putStr "*"
                             printStar (x-1) 

printPlayer :: (Eq a, Num a) => a -> IO ()
printPlayer x | x==1      = putStrLn "Player 1" 
              | otherwise = putStrLn "Player 2"

turn :: (Eq a, Num a, Num a1) => a -> a1
turn x | x == 1    = 2
       | otherwise = 1

nim :: (Eq a1, Num a1, Num a, Ord a, Read a) => [a] -> a1 -> IO ()
nim b x = do printPlayer x 
             printBoard b
             b1 <- move b x 
             let c = checkBoard b1
             if c then nim b1 (turn x) else ending x
      
startNim :: (Num a, Ord a, Read a) => [a] -> IO ()     
startNim b = do cls
                putStrLn "So begins the game of Nim!"
                nim b 1

move :: (Num a, Read a) => [a] -> t -> IO [a]
move b x = do putStrLn ""
              putStrLn "Which row would you like to take from?"
              z1 <- getLine
              let z = 0 + (read z1)
              putStrLn "How many would you like to take?" 
              y1 <- getLine
              let y = 0 + (read y1) 
              return (newBoard b z y)
   
newBoard :: Num a => [a] -> Int -> a -> [a]          
newBoard b x y = (take (x-1) b)++(((b!! (x-1))-y):[])++(drop (x) b)  

checkBoard :: (Num a, Ord a) => [a] -> Bool
checkBoard b  | (length [x|x<-b, x >=1])>=1 = True
              | otherwise                   = False

ending :: (Eq a, Num a) => a -> IO ()
ending x | x == 1    = putStrLn "Player 1 Wins!!!!"
         | otherwise = putStrLn "Player 2 Wins!!!!"





