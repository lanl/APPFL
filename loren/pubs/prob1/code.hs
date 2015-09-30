import qualified Data.Map as Map  
import Data.List

type Node = (Int,([Int],Int))
type Graph = Map.Map Int ([Int],Int)

p3c7 :: [Node]
p3c7 = [(1,([2,3,4],0)),(2,([1,5,6],0)),(3,([1,4,7],0)),(4,([1,3,5,8],0)),(5,([2,4,6,8,9],0)),(6,([2,5,10,11],0)),(7,([3,8,11],0)),(8,([4,5,7,9,12],0)),(9,([4,5,8,10,13,14],0)),(10,([6,9,11,13,14],0)),(11,([6,10,15,16],0)),(12,([7,8,13,17],0)),(13,([9,10,12,14,17],0)),(14,([9,10,13,15,18,19],0)),(15,([11,14,16,18,19],0)),(16,([11,15,20,21],0)),(17,([12,13,18,22],0)),(18,([14,15,17,19,22],0)),(19,([14,15,18,20,23,24],0)),(20,([16,19,21,23,24],0)),(21,([16,20,25,26],0)),(22,([17,18,23,27],0)),(23,([19,20,22,24,27],0)),(24,([19,20,23,25,28,29],0)),(25,([21,24,26,28,29],0)),(26,([24,25,30],0)),(27,([22,23,28,31],0)),(28,([24,25,27,29,31],0)),(29,([24,25,28,30,32],0)),(30,([26,29,32],0)),(31,([27,28,32],0)),(32,([29,30,31],0))]

p3c7Map :: Graph
p3c7Map = Map.fromList p3c7

--Gets Degree
degree :: Int -> Graph -> Int
degree x g = case Map.lookup x g of 
               Just v -> length (fst v)
               Nothing -> 0
--Gets Color
color :: Int -> Graph -> Int 
color x g = case Map.lookup x g of 
              Just v -> (snd v)
              Nothing -> 0 
--Determines if vertex is colored
isColored :: Int-> Graph -> Bool
isColored x g = case Map.lookup x g of 
                  Just v -> if (snd v)==0 then False else True
                  Nothing -> True 
--Checks if a coloring is proper for set of vertices
checkProperColor :: [Int] -> Graph -> Bool
checkProperColor [] _ = True
checkProperColor (x:xs) g = checkProper (getNeighbs x g) (color x g) g && checkProperColor xs g
--Checks if a coloring is Proper for vertex
checkProper :: [Int] -> Int -> Graph -> Bool
checkProper  [] c g = True
checkProper  (x:xs) c g = if color x g == c then False else checkProper xs c g
--Changes color of vertex
changeColor :: Graph -> Int -> Int -> Graph
changeColor g y d = Map.insertWith addColor y ([],d) g
--helper function for changeColor
addColor :: ([Int],Int) -> ([Int],Int) -> ([Int],Int) 
addColor (xs,d) (zs,y) = (zs,d) 
--Determines if two vertices are neighbors 
neighbs :: Int -> Int -> Graph -> Bool
neighbs x y g = case Map.lookup y g of 
                  Just v -> if elem x (fst v) then True else False
                  Nothing -> True 
--Gets Neighbors of a certain vertex
getNeighbs :: Int -> Graph -> [Int]
getNeighbs x g = case Map.lookup x g of 
                  Just v ->  (fst v)
                  Nothing -> [] 
--Determines if neighbors are colored
fullyColored :: [Int] -> [Int] -> Bool
fullyColored xs [] = True
fullyColored xs (y:ys) =  elem y xs && fullyColored xs ys
--Determines if a set of vertices is Grundy
checkGrundyColor :: [Int] -> Graph -> Bool
checkGrundyColor [] _ = True
checkGrundyColor (x:xs) g = checkGrundy (getNeighbs x g) ((color x g)-1) g && checkGrundyColor xs g
--Determines if a vertex is Grundy
checkGrundy :: [Int] -> Int-> Graph -> Bool
checkGrundy xs 0 g = True
checkGrundy xs c g = isOne xs c g && checkGrundy xs (c-1) g
--HelperFunctionfor checkGrundy
isOne :: [Int] -> Int -> Graph -> Bool
isOne [] c g = False
isOne (x:xs) c g = c == (color x g) || isOne xs c g 
--Checks if a graph is Colored Properly and Grundy
goodGraph :: Graph -> [Int] -> [Int] -> Bool
goodGraph g xs (y:ys) = checkGrundyColor zs g && checkProperColor zs g
                        where zs = readyToCheck g xs (y:ys)
--Sees which vertices and neighbors are colored
readyToCheck :: Graph -> [Int] -> [Int] -> [Int]
readyToCheck g xs (y:ys) = [x | x<- xs, neighbs x y g, fullyColored (y:xs) (getNeighbs x g)]

grundy7 :: Graph -> Int
grundy7 g = nextStep g [] (Map.keys g)

nextStep :: Graph -> [Int] -> [Int] -> Int 
nextStep g _ [] = 1
nextStep g xs (y:ys) = cycleColors g xs (y:ys) ((degree y g)+1)

cycleColors :: Graph -> [Int] -> [Int] -> Int -> Int
cycleColors g _ _ 0 = 0
cycleColors g xs (y:ys) d | y==14 = checkEverything  (changeColor g 14 7) xs (14:ys)
                          | otherwise =  (checkEverything (changeColor g y d) xs (y:ys)) + (cycleColors g xs (y:ys) (d-1)) 

checkEverything :: Graph -> [Int] -> [Int] -> Int
checkEverything g xs (y:ys) = if goodGraph g xs (y:ys) then nextStep g (y:xs) ys else 0






test1 :: [Node]
test1 = [(1,([2,3,4],1)),(2,([1,5,6],2)),(3,([1,4,7],2)),(4,([1,3,5,8],4)),(5,([2,4,6,8,9],3)),(6,([2,5,10,11],1)),(7,([3,8,11],2)),(8,([4,5,7,9,12],0)),(9,([4,5,8,10,13,14],0)),(10,([6,9,11,13,14],0)),(11,([6,10,15,16],0)),(12,([7,8,13,17],0)),(13,([9,10,12,14,17],0)),(14,([9,10,13,15,18,19],0)),(15,([11,14,16,18,19],0)),(16,([11,15,20,21],0)),(17,([12,13,18,22],0)),(18,([14,15,17,19,22],0)),(19,([14,15,18,20,23,24],0)),(20,([16,19,21,23,24],0)),(21,([16,20,25,26],0)),(22,([17,18,23,27],0)),(23,([19,20,22,24,27],0)),(24,([19,20,23,25,28,29],0)),(25,([21,24,26,28,29],0)),(26,([24,25,30],0)),(27,([22,23,28,31],0)),(28,([24,25,27,29,31],0)),(29,([24,25,28,30,32],0)),(30,([26,29,32],0)),(31,([27,28,32],0)),(32,([29,30,31],0))]

t :: Graph
t = Map.fromList test1


test2 :: [Node]
test2 = [(1,([2,4],0)),(2,([1,3,5],0)),(3,([2,6],0)),(4,([1,5],0)),(5,([2,4,6],0)),(6,([3,5],0))]

t2 :: Graph
t2= Map.fromList test2

test3 :: [Node]
test3 = [(1,([2,3,4],0)),(2,([1,5,6],0)),(3,([1,4,7],0)),(4,([1,3,5,8],0)),(5,([2,4,6,8,9],0)),(6,([2,5,10,11],0)),(7,([3,8,11],0)),(8,([4,5,7,9,12],0)),(9,([4,5,8,10,13,14],0)),(10,([6,9,11,13,14],0)),(11,([6,10,15,16],0)),(12,([7,8,13,17],0)),(13,([9,10,12,14,17],0)),(14,([9,10,13,15,18,19],7)),(15,([11,14,16,18,19],0)),(16,([11,15,20,21],0)),(17,([12,13,18,22],0)),(18,([14,15,17,19,22],0)),(19,([14,15,18,20,23,24],0)),(20,([16,19,21,23,24],0)),(21,([16,20,25,26],0)),(22,([17,18,23,27],0)),(23,([19,20,22,24,27],0)),(24,([19,20,23,25,28,29],0)),(25,([21,24,26,28,29],0)),(26,([24,25,30],0)),(27,([22,23,28,31],0)),(28,([24,25,27,29,31],0)),(29,([24,25,28,30,32],0)),(30,([26,29,32],0)),(31,([27,28,32],0)),(32,([29,30,31],0))]

t3 :: Graph
t3 = Map.fromList test3
