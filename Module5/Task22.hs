-- https://stepik.org/lesson/8444/step/10
import Control.Monad.State

-- system
data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving (Show, Eq)

-- solution

numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (helper tree) 1
  where
    helper (Leaf _) = do
      count <- get
      put $ count + 1
      return $ Leaf count
    helper (Fork left val right) = do
      left' <- helper left
      count <- get
      put $ count + 1
      right' <- helper right
      return $ Fork left' count right'

--- tests

test1 = numberTree (Leaf ()) == Leaf 1

test2 = numberTree (Fork (Leaf ()) () (Leaf ())) == Fork (Leaf 1) 2 (Leaf 3)

tests = map (== True) [test1, test2]