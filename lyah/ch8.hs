-- learn you a haskell ch 8

data Shape1 = Circle1 Float Float Float | Rectangle1 Float Float Float Float 
                deriving (Show)  

surface1 :: Shape1 -> Float  
surface1 (Circle1 _ _ r) = pi * r ^ 2  
surface1 (Rectangle1 x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)  

data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- pattern match on Constructors
surface :: Shape -> Float  
surface (Circle _ r) = pi * r ^ 2  
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1) 

nudge :: Shape -> Float -> Float -> Shape  
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r  
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = 
        Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))  

baseCircle :: Float -> Shape  
baseCircle r = Circle (Point 0 0) r  
  
baseRect :: Float -> Float -> Shape  
baseRect width height = Rectangle (Point 0 0) (Point width height) 

-- record syntax
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)   

-- type parameters

data MyMaybe a = MyNothing | MyJust a 

data Vector a = Vector a a a deriving (Show)  
  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n  

-- Derived instances

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)  

-- Type synonyms

type PhoneNumber = String  
type Name = String  
type PhoneBook = [(Name,PhoneNumber)]  

type AssocList k v = [(k,v)]  

-- Recursive data structures

data MyList a = Empty | Cons a (MyList a) deriving (Show, Eq, Ord)  

mymap :: (a -> b) -> MyList a -> MyList b
mymap f Empty = Empty
mymap f (Cons x xs) = Cons (f x) (mymap f xs)

mycat :: MyList a -> MyList a -> MyList a
mycat (Cons x xs) ys = Cons x (mycat xs ys)

myconcat :: MyList (MyList a) -> MyList a 
myconcat Empty = Empty
myconcat (Cons x xs) = mycat x (myconcat xs)

-- Typeclasses 102

data TrafficLight = Red | Yellow | Green  

instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False  

instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"  

class YesNo a where  
    yesno :: a -> Bool  

instance YesNo Int where  
    yesno 0 = False  
    yesno _ = True 

instance YesNo [a] where  
    yesno [] = False  
    yesno _ = True  

instance YesNo Bool where  
    yesno = id 

instance YesNo (Maybe a) where  
    yesno (Just _) = True  
    yesno Nothing = False 

instance YesNo TrafficLight where  
    yesno Red = False  
    yesno _ = True  



