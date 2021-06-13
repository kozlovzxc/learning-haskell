import Data.Char
import Data.List (unfoldr)

nTimes :: a -> Int -> [a]
nTimes val n = helper [] n
  where
    helper acc 0 = acc
    helper acc n = helper (val : acc) (n -1)

--------------------------------------------------------

oddsOnly :: Integral a => [a] -> [a]
oddsOnly arr = reverse (helper [] arr)
  where
    helper ans [] = ans
    helper ans (cur : arr) = if odd cur then helper (cur : ans) arr else helper ans arr

-- nice solution
-- oddsOnly :: Integral a => [a] -> [a]
-- oddsOnly [] = []
-- oddsOnly (x : xs)
--   | odd x = x : oddsOnly xs
--   | otherwise = oddsOnly xs

--------------------------------------------------------

slice :: [a] -> Int -> Int -> [a]
slice arr from to = helper 0 arr
  where
    helper i [] = []
    helper i (x : xs)
      | i >= 0 && i < from = helper (i + 1) xs
      | i >= from && i < to = x : helper (i + 1) xs
      | otherwise = []

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome arr = left == reverse right
  where
    arrLen = length arr
    middle = arrLen `div` 2
    left = slice arr 0 middle
    right = slice arr (if odd arrLen then middle + 1 else middle) arrLen

-- simpler solution
-- isPalindrome :: Eq a => [a] -> Bool
-- isPalindrome lst = reverse lst == lst

--------------------------------------------------------

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 arr1 arr2 arr3 = currentSum : nextArr
  where
    zHead arr = if null arr then 0 else head arr
    zTail arr = if null arr then arr else tail arr
    currentSum = zHead arr1 + zHead arr2 + zHead arr3
    nextArr = sum3 (zTail arr1) (zTail arr2) (zTail arr3)

--------------------------------------------------------

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x : xs) = helper x [x] xs
  where
    helper :: (Eq a) => a -> [a] -> [a] -> [[a]]
    helper last lastArr [] = [lastArr]
    helper last lastArr (x : xs)
      | last == x = helper last (x : lastArr) xs
      | otherwise = lastArr : helper x [x] xs

--------------------------------------------------------

readDigits :: String -> (String, String)
readDigits = span isDigit

--------------------------------------------------------

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj pred1 pred2 = filter (\x -> pred1 x || pred2 x)

--------------------------------------------------------

partition p xs = (filter p xs, filter (not . p) xs)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort arr = qsort left ++ [ahead] ++ qsort right
  where
    ahead = head arr
    (left, right) = partition (< ahead) (tail arr)

--------------------------------------------------------

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x ^ 2, x ^ 3])

--------------------------------------------------------

perms :: [a] -> [[a]]
perms [] = [[]]
perms arr = shuffToMany (head arr) (perms $ tail arr)
  where
    shuffToMany :: a -> [[a]] -> [[a]]
    shuffToMany item arrays = concatMap (shuffToOne item) arrays

    shuffToOne :: a -> [a] -> [[a]]
    shuffToOne item = helper []
      where
        helper left [] = [left ++ [item]]
        helper left right = (left ++ item : right) : helper (left ++ [head right]) (tail right)

--------------------------------------------------------

delAllUpper :: String -> String
delAllUpper = unwords . filter (not . all isUpper) . words

--------------------------------------------------------

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\a b c -> max (max a b) (max a c))

--------------------------------------------------------

fibStream :: [Integer]
fibStream = [0, 1] ++ zipWith (+) (tail fibStream) fibStream

--------------------------------------------------------

data Odd = Odd Integer deriving (Eq, Show)

instance Enum Odd where
  succ (Odd a) = Odd $ (succ . succ) a
  pred (Odd a) = Odd $ (pred . pred) a
  toEnum a = Odd $ toInteger a
  fromEnum (Odd a) = fromInteger a

  enumFrom (Odd from) = map Odd [from, from + 2 ..]
  enumFromThen (Odd from) (Odd next) = map Odd [from, next ..]
  enumFromTo (Odd from) (Odd to) = map Odd [from, from + 2 .. to]
  enumFromThenTo (Odd from) (Odd next) (Odd to) = map Odd [from, next .. to]

--------------------------------------------------------

coins = [2, 3, 7]

-- change :: (Ord a, Num a) => a -> [[a]]
change :: Integer -> [[Integer]]
change n
  | n < 0 = []
  | n == 0 = [[]]
  | otherwise = [coin : _change | coin <- coins, _change <- change (n - coin)]

--------------------------------------------------------

concatList :: [[a]] -> [a]
concatList = foldr (++) []

--------------------------------------------------------

lengthList :: [a] -> Int
lengthList = foldr (\a b -> b + 1) 0

--------------------------------------------------------

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then x + s else s) 0

--------------------------------------------------------

meanList :: [Double] -> Double
meanList = (\(sum, len) -> if len == 0 then 0 else sum / len) . foldr (\cur (sum, len) -> (sum + cur, len + 1)) (0, 0)

--------------------------------------------------------

-- v1
-- evenOnly :: [a] -> [a]
-- evenOnly = reverse . fst . foldl (\(ans, pos) cur -> if even pos then (cur : ans, pos + 1) else (ans, pos + 1)) ([], 1)

-- v2
-- evenOnly :: [a] -> [a]
evenOnly = foldr (\(index, value) ans -> if even index then value : ans else ans) [] . zip [1 ..]

--------------------------------------------------------

revRange :: (Char, Char) -> [Char]
revRange (from, to) = helper to
  where
    helper = unfoldr (\x -> if x >= from then Just (x, pred x) else Nothing)
