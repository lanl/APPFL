--Interactive Modification
import Data.List
type Board = [Pos]
type Pos = (Int,Int)

glider :: Board
glider = [(4,2),(2,3), (4,3), (3,4),(4,4)]

test :: Board
test = [(2,2),(3,2),(4,2),(3,3),(4,4),(5,4),(4,3)]

showCells :: Board -> IO()
showCells b = seqn[writeat p "0" |p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not(isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = [(a,b)|(a,b)<- (neighbs1 (x,y))]

neighbs1 :: Pos -> [Pos]
neighbs1 (x,y) = [(x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)]

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b). neighbs

survivors :: Board -> [Pos]
survivors b = [p|p <- b, elem(liveneighbs b p) [2,3]]

births :: Board -> [Pos]
births b = [p|p <- rmdups (concat (map neighbs b)), isEmpty b p, liveneighbs b p ==3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x: rmdups (filter (/= x) xs)

nextgen :: Board -> Board
nextgen b = (survivors b ++ births b)

subX :: Int -> Pos -> Pos
subX a (x,y) = (x-a,y)

subY :: Int -> Pos -> Pos
subY a (x,y) = (x,y-a)

renormalize :: Board -> Board
renormalize b = map (subY w) (map (subX z) (b))
                where z = ((minimum [fst(p)|p<-b])-2)
                      w = ((minimum [snd(p)|p<-b])-2)

life :: Board -> IO()
life b = do cls
            showCells b
            printBorder b
            wait 5000
            if (length (nextgen b)) == 0 
               then do cls
                       putStrLn "EXTINCTION!!!!"
               else life (renormalize(nextgen b))
           
startLife = do cls
               printBorder [(50,50),(2,2)]
               goto (2,2)
               b <- getBoard [] (2,2)
               let b1 = renormalize b
               printBorder b1  
               life (b1)

wait :: Int -> IO()
wait n = seqn [return () | _<- [1..n]]

seqn :: [IO a] -> IO()
seqn [] = return ()
seqn (a:as) = do a 
                 seqn as

cls :: IO()
cls = putStr "\ESC[2J"

writeat :: Pos -> String -> IO()
writeat p xs = do goto p 
                  putStr xs

goto :: Pos -> IO()
goto (x,y) = putStr ("\ESC["++ show y ++";"++ show x ++"H")

printBorder b  = do printLeft x y
                    printTop x y
                    printBottom x y 
                    printRight x y
                    where x = ((maximum [fst(p)|p<-b])+1)
                          y = ((maximum [snd(p)|p<-b])+1)

printLeft x y = seqn[writeat (1,b) "|" | b<- [1..y]]
printTop x y = seqn[writeat (a,1) "-" | a <- [1..x]]
printRight x y = seqn[writeat (x,b) "|" | b<- [1..y]]
printBottom x y = seqn[writeat (a,y) "-" | a <- [1..x]]

getBoard ::Board -> Pos ->  IO Board
getBoard xs p = do goto p
                   x <- getChar
                   if (length [y|y <- xs, y==p])>=1 then writeat p "0" else writeat p " "
                   case x of 
                       'i' -> if (snd p) == 2 then getBoard xs p else getBoard xs ((fst p),(snd p)-1)
                       'j' -> if (fst p) == 2 then getBoard xs p else getBoard xs ((fst p)-1,(snd p))
                       'l' -> if (fst p) == 50 then getBoard xs p else getBoard xs ((fst p)+1,(snd p))
                       'm' -> if (snd p) == 50 then getBoard xs p else getBoard xs ((fst p),(snd p)+1)
                       'k' -> if (length [y|y<- xs, y==p])>=1
                                    then do writeat p " "
                                            getBoard (xs \\ (p:[])) p                            
                                    else do writeat p "0" 
                                            getBoard (p:xs) p                                  
                       'q' -> return xs 
                       _  -> getBoard xs p
