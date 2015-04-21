import Data.List(foldl')
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Function (Fun(..))
import Test.QuickCheck.Modifiers(NonEmptyList(..))

reverse1 = foldr (\x xs -> xs ++ [x]) []

reverse2 = foldl' (\xs x -> [x] ++ xs) []

reverse3 = foldl' (flip (:)) []

myfoldl1 f xs = foldl f (head xs) (tail xs)

myfoldr1 f xs = foldr f (last xs) (init xs)

type FoldlFun a b = Fun b (a -> b)
  
type FoldrFun a b = Fun a (b -> b)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [revProps, foldProps]

revProps = testGroup "(reverse QuickCheck)"
    [ QC.testProperty "rev12" $ 
    \list -> reverse1 (list :: [Int]) == reverse2 list
    , QC.testProperty "rev23" $  
    \list -> reverse2 (list :: [Int]) == reverse3 list
    , QC.testProperty "rev3x" $  
    \list -> reverse3 (list :: [Int]) == reverse list
    ]

foldProps = testGroup "(fold QuickCheck)"
    [ QC.testProperty "foldl1" $ prop_foldl
    , QC.testProperty "foldr1" $ prop_foldr
    ]

-- quickcheck is a bit of a pain with curried functions
prop_foldl :: Fun (Int,Int) Int -> NonEmptyList Int -> Bool
prop_foldl (Fun _ f) (NonEmpty list) = myfoldl1 (curry f) list == foldl1 (curry f) list

prop_foldr :: Fun (Int,Int) Int -> NonEmptyList Int -> Bool
prop_foldr (Fun _ f) (NonEmpty list) = myfoldr1 (curry f) list == foldr1 (curry f) list

