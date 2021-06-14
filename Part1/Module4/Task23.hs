import qualified Data.List as L
import Data.Maybe (isJust)
import Prelude hiding (lookup)

class MapLike m where
  empty :: m k v
  lookup :: Ord k => k -> m k v -> Maybe v
  insert :: Ord k => k -> v -> m k v -> m k v
  delete :: Ord k => k -> m k v -> m k v
  fromList :: Ord k => [(k, v)] -> m k v
  fromList [] = empty
  fromList ((k, v) : xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap {getListMap :: [(k, v)]}
  deriving (Eq, Show)

instance MapLike ListMap where
  empty = ListMap []
  lookup key (ListMap []) = Nothing
  lookup key (ListMap ((k, v) : xs)) = if key == k then Just v else lookup key $ ListMap xs
  insert key value listMap = if keyExists then replace key value listMap else push key value listMap
    where
      keyExists = isJust $ lookup key listMap
      replace key value (ListMap arr) = ListMap $ helper arr
        where
          helper [] = []
          helper ((k, v) : xs) = if k == key then (key, value) : xs else (k, v) : helper xs
      push key value (ListMap arr) = ListMap $ arr ++ [(key, value)]
  delete key (ListMap arr) = ListMap $ helper arr
    where
      helper [] = []
      helper ((k, v) : xs) = if k == key then xs else (k, v) : helper xs

-- Nice looking answer
-- instance MapLike ListMap where
--     empty = ListMap []
--     lookup k = L.lookup k . getListMap
--     delete k = ListMap . filter ((/= k) . fst) . getListMap
--     insert k v = ListMap . ((k,v):) . getListMap . delete k
