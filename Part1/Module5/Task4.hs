data Entry k1 k2 v = Entry (k1, k2) v deriving (Show)

data Map k1 k2 v = Map [Entry k1 k2 v] deriving (Show)

instance Functor (Entry k1 k2) where
  fmap f (Entry a b) = Entry a (f b)

instance Functor (Map k1 k2) where
  fmap f (Map entries) = Map $ map (fmap f) entries

-- test1 = fmap (map toUpper) $ Map [Entry (0, 0) "origin", Entry (800, 0) "right corner"]
