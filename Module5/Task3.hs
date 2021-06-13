data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving (Show)

instance Functor Tree where
  fmap = fmap'
    where
      helper f Nothing = Nothing
      helper f (Just a) = Just $ f a

      fmap' f (Leaf a) = Leaf $ helper f a
      fmap' f (Branch a b c) = Branch (fmap' f a) (helper f b) (fmap' f c)
