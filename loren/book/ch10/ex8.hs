
{-
instance Monad Maybe where 

return :: a -> Maybe a
return x = Just x

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>= _  = Nothing
(Just x) >>= f = f x


instance Monad [] where

return :: a -> [a]
return x = [x]

(>>=) :: [a] -> (a -> [b]) -> [b]
xs >>= f = concat (map f xs)
-}




