import Data.Char
import Data.Function

main = putStrLn "Hello world!"

--------------------------------------------------------

lenVec3 x y z = sqrt (x * x + y * y + z * z)

--------------------------------------------------------

sign x
  | x > 0 = 1
  | x == 0 = 0
  | otherwise = -1

--------------------------------------------------------

infixl 7 |-|

a |-| b = abs (a - b)

--------------------------------------------------------

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y =
  if isDigit x && isDigit y
    then digitToInt y + 10 * digitToInt x
    else 100

--------------------------------------------------------

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt ((-) (fst p1) (fst p2) ** 2 + (-) (snd p1) (snd p2) ** 2)

--------------------------------------------------------

doubleFact :: Integer -> Integer
doubleFact 0 = 1
doubleFact 1 = 1
doubleFact n = n * doubleFact (n - 2)

--------------------------------------------------------

fibonacci n
  | n == 0 = 0
  | n == 1 = 1
  | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
  | n < 0 = fibonacci (n + 2) - fibonacci (n + 1)

--------------------------------------------------------

fibonacci' n
  | n == 0 = 0
  | n == 1 = 1
  | n > 0 = fibonacciPositiveHelper 1 0 n
  | n < 0 = fibonacciNegativeHelper 1 0 (abs n)

fibonacciPositiveHelper n2 n1 1 = n2
fibonacciPositiveHelper n2 n1 n = fibonacciPositiveHelper (n1 + n2) n2 (n - 1)

fibonacciNegativeHelper n2 n1 0 = n1
fibonacciNegativeHelper n2 n1 n = fibonacciNegativeHelper n1 (n2 - n1) (n - 1)

--------------------------------------------------------

seqA n =
  let helper k2 k1 k 0 = k
      helper k2 k1 k 1 = k1
      helper k2 k1 k 2 = k2
      helper k2 k1 k n = helper (k2 + k1 - 2 * k) k2 k1 (n - 1)
   in helper 3 2 1 n

--------------------------------------------------------

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count 0 = (0, 1)
sum'n'count n = helper (0, 0) (abs n)
  where
    helper :: (Integer, Integer) -> Integer -> (Integer, Integer)
    helper (sum, count) 0 = (sum, count)
    helper (sum, count) n = helper (sum + n `mod` 10, count + 1) (n `div` 10)

--------------------------------------------------------

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b
  | b > a = helper 0 a steps
  | otherwise = - helper 0 b steps
  where
    low = min a b
    high = max a b
    steps = 1000
    step = (high - low) / steps
    helper sum x 0 = sum
    helper sum x steps = helper (sum + (f x + f (x + step)) / 2 * step) (x + step) (steps - 1)

multSecond = g `on` h

g = (*)

h = snd

--------------------------------------------------------

class Printable a where
  toString :: a -> String

instance Printable Bool where
  toString True = "true"
  toString False = "false"

instance Printable () where
  toString () = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
  toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"

--------------------------------------------------------

class KnownToGork a where
  stomp :: a -> a
  doesEnrageGork :: a -> Bool

class KnownToMork a where
  stab :: a -> a
  doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
  stompOrStab :: a -> a
  stompOrStab a
    | doesEnrageMork a && doesEnrageGork a = (stomp . stab) a
    | doesEnrageMork a = stomp a
    | doesEnrageGork a = stab a
    | otherwise = a

--------------------------------------------------------

class (Eq a, Enum a, Bounded a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x
    | x == maxBound = minBound
    | otherwise = succ x

  spred :: a -> a
  spred x
    | x == minBound = maxBound
    | otherwise = pred x

--------------------------------------------------------

avg :: Int -> Int -> Int -> Double
avg x y z = fromInteger (toInteger x + toInteger y + toInteger z) / 3.0