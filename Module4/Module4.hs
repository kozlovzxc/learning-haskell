import Data.Char (isDigit)
import Data.List.Split
import Data.Time.Clock
import Data.Time.Format
import Text.Regex.Posix

--------------------------------------------------------

data Result = Fail | Success

doSomeWork :: a -> (Result, Int)
doSomeWork _ = (Fail, 1)

processData :: a -> String
processData someData =
  let (result, code) = doSomeWork someData
   in case result of
        Success -> "Success"
        Fail -> "Fail: " ++ show code

--------------------------------------------------------

data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

-- It is duplicating, so I've commented it out
-- distance :: Point -> Point -> Double
-- distance (Point x1 y1) (Point x2 y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

--------------------------------------------------------

data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * r ^ 2
area (Rectangle x y) = x * y

--------------------------------------------------------

data SomeData = Data

data Result' = Success1 | Fail1 Int

instance Show Result' where
  show (Fail1 code) = "Fail: " ++ show code
  show Success1 = "Success"

doSomeWork' :: SomeData -> Result'
doSomeWork' input = case doSomeWork input of
  (Success, 0) -> Success1
  (Fail, x) -> Fail1 x

--------------------------------------------------------

-- data Shape = Circle Double | Rectangle Double Double

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle a b) = a == b
isSquare _ = False

--------------------------------------------------------

-- data Bit = Zero | One

-- instance Show Bit where
--   show Zero = "0"
--   show One = "1"

-- data Sign = Minus | Plus

-- instance Show Sign where
--   show Minus = "Minus "
--   show Plus = "Plus "

-- data Z = Z Sign [Bit]

-- instance Show Z where
--   show (Z sign bits) = show sign ++ show bits

-- bitToInteger :: Bit -> Integer
-- bitToInteger Zero = 0
-- bitToInteger One = 1

-- bitsToInteger :: Z -> Integer
-- bitsToInteger (Z sign bits) = getSign sign * getNumber bits
--   where
--     getSign Minus = -1
--     getSign Plus = 1

--     getNumber :: [Bit] -> Integer
--     getNumber = sum . zipWith (\power bit -> bitToInteger bit * (2 ^ power)) [0 ..]

-- integerToBits :: Integer -> Z
-- integerToBits number = Z sign bits
--   where
--     sign = if number >= 0 then Plus else Minus

--     bits = helper $ abs number
--       where
--         getBit number = if even number then Zero else One

--         helper :: Integer -> [Bit]
--         helper 0 = [Zero]
--         helper 1 = [One]
--         helper number = getBit number : helper (number `div` 2)

-- add :: Z -> Z -> Z
-- add z1 z2 = integerToBits $ bitsToInteger z1 + bitsToInteger z2

-- mul :: Z -> Z -> Z
-- mul z1 z2 = integerToBits $ bitsToInteger z1 * bitsToInteger z2

--------------------------------------------------------

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info

data LogEntry = LogEntry {timestamp :: UTCTime, logLevel :: LogLevel, message :: String}

logLevelToString :: LogLevel -> String
logLevelToString Error = "Error"
logLevelToString Warning = "Warning"
logLevelToString Info = "Info"

logEntryToString :: LogEntry -> String
logEntryToString (LogEntry time level message) = timeToString time ++ ": " ++ logLevelToString level ++ ": " ++ message

--------------------------------------------------------

data Person = Person {firstName :: String, lastName :: String, age :: Int}

updateLastName :: Person -> Person -> Person
updateLastName person1 person2 = person2 {lastName = lastName person1}

--------------------------------------------------------

-- data Person = Person { firstName :: String, lastName :: String, age :: Int }

abbrFirstName :: Person -> Person
abbrFirstName p@Person {firstName = fn} = p {firstName = shortName}
  where
    shortName :: String
    shortName = if length fn > 2 then head fn : "." else fn

--------------------------------------------------------

data Coord a = Coord a a deriving (Show)

distance :: Coord Double -> Coord Double -> Double
distance (Coord a1 b1) (Coord a2 b2) = sqrt ((a1 - a2) ^ 2 + (b1 - b2) ^ 2)

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord a1 b1) (Coord a2 b2) = abs (a1 - a2) + abs (b1 - b2)

--------------------------------------------------------

-- data Coord a = Coord a a

-- getCenter 2.2 (Coord 2 1) = Coord 5.5 3.3
getCenter :: Double -> Coord Int -> Coord Double
getCenter width (Coord a b) = Coord (width * (fromIntegral a + 0.5)) (width * (fromIntegral b + 0.5))

-- getCell 2.2 (Coord 3.2 1.6) = Coord 1 0
getCell :: Double -> Coord Double -> Coord Int
getCell width (Coord a b) = Coord (truncate (a / width)) (truncate (b / width))

--------------------------------------------------------

findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x : xs) = if isDigit x then Just x else findDigit xs

--------------------------------------------------------

findDigitOrX :: [Char] -> Char
findDigitOrX string = case findDigit string of
  Nothing -> 'X'
  Just x -> x

--------------------------------------------------------

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : xs) = Just x

--------------------------------------------------------

data Error = ParsingError | IncompleteDataError | IncorrectDataError String

-- data Person = Person { firstName :: String, lastName :: String, age :: Int }

-- personRegex = "firstName *= *\\w+\nlastName *= *\\w+\nage *= *[0-9]+"
-- personTest = "firstName = John\nlastName = Connor\nage = 30"
-- parsePerson :: String -> Either Error Person
-- parsePerson = answer where
--   sentences = splitOn "\n"
--   xyPattern = "\\w+ *= *\\w+"
--   checkXYPattern text = (text == xyPattern :: Bool) || ParsingError
--   answer = map checkXYPattern

--------------------------------------------------------

data List a = Nil | Cons a (List a) deriving (Show)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons a b) = a : fromList b

toList :: [a] -> List a
toList [] = Nil
toList (x : xs) = Cons x $ toList xs

--------------------------------------------------------

data Nat = Zero | Suc Nat deriving (Show)

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = Suc (toNat (n -1))

add :: Nat -> Nat -> Nat
add a b = toNat (fromNat a + fromNat b)

mul :: Nat -> Nat -> Nat
mul a b = toNat (fromNat a * fromNat b)

fac :: Nat -> Nat
fac Zero = Suc Zero
fac a@(Suc Zero) = a
fac a@(Suc a') = mul a $ fac a'

--------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf a) = 0
height (Node a b) = 1 + max (height a) (height b)

size :: Tree a -> Int
size (Leaf a) = 1
size (Node a b) = 1 + size a + size b

--------------------------------------------------------

-- data Tree a = Leaf a | Node (Tree a) (Tree a)

avg :: Tree Int -> Int
avg t =
  let (s, c) = go t
   in s `div` c
  where
    go :: Tree Int -> (Int, Int)
    go (Leaf a) = (a, 1)
    go (Node a b) = (leftSum + rightSum, leftCount + rightCount)
      where
        (leftSum, leftCount) = go a
        (rightSum, rightCount) = go b

--------------------------------------------------------

newtype Xor = Xor {getXor :: Bool}
  deriving (Eq, Show)

instance Semigroup Xor where
  a <> b = Xor (a `xor` b)
    where
      xor a b = a /= b

instance Monoid Xor where
  mempty = Xor False

--------------------------------------------------------

newtype Maybe' a = Maybe' {getMaybe :: Maybe a}
  deriving (Eq, Show)

instance Monoid a => Semigroup (Maybe' a) where
  (Maybe' Nothing) <> b = Maybe' Nothing
  a <> (Maybe' Nothing) = Maybe' Nothing
  (Maybe' a) <> (Maybe' b) = Maybe' $ a <> b

instance Monoid a => Monoid (Maybe' a) where
  mempty = Maybe' (Just mempty)