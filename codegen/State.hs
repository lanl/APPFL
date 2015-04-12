module State (
  State(..),
  state,
  runState,
  get,
  put,
  liftM,
  concatMapM
) where

import           Control.Applicative (Applicative (..))

instance Functor (State s) where
    fmap = liftM

instance Applicative (State s) where
    pure  = return
    (<*>) = ap

newtype State s a = State (s -> (a, s))

state :: (s -> (a, s)) -> State s a
state x = State x

runState :: State s a -> s -> (a, s)
runState (State f) x = f x

instance Monad (State s) where
    return x = state (\st -> (x, st))
    act >>= k = state $ \st ->
                          let (x, st') = runState act st
                          in runState (k x) st'

get :: State a a
get = State $ \s -> (s,s)

put :: s -> State s ()
put newState = State $ \s -> ((), newState)

liftM :: (Monad m) => (a -> r) -> m a -> m r
liftM f m = do { x <- m; return (f x) }

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf m = do { f <- mf; x <- m; return (f x) }
