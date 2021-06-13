import Prelude hiding (lookup)

class MapLike m where
  empty :: m k v
  lookup :: Ord k => k -> m k v -> Maybe v
  insert :: Ord k => k -> v -> m k v -> m k v
  delete :: Ord k => k -> m k v -> m k v
  fromList :: Ord k => [(k, v)] -> m k v

newtype ArrowMap k v = ArrowMap {getArrowMap :: k -> Maybe v}

instance MapLike ArrowMap where
  empty = ArrowMap (const Nothing)
  lookup k (ArrowMap map) = map k
  delete k (ArrowMap map) = ArrowMap (\x -> if x == k then Nothing else map x)
  insert k v (ArrowMap map) = ArrowMap (\x -> if x == k then Just v else map x)
  fromList [] = empty
  fromList ((k, v) : xs) = insert k v (fromList xs)