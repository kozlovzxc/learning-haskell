-- https://stepik.org/lesson/8439/step/6?unit=1574

data Board = Board Int deriving (Show, Eq)

instance Enum Board where
  succ (Board x) = Board $ succ x
  pred (Board x) = Board $ pred x
  toEnum = Board
  fromEnum (Board x) = x

nextPositions :: Board -> [Board]
nextPositions b = [b, succ b]

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred
  | n < 0 = []
  | n == 0 = filter pred $ return b
  | otherwise = nextPositions b >>= \x -> nextPositionsN x (n -1) pred

-- possible solution
-- nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
-- nextPositionsN b n pred
--   | n < 0 = []
--   | n == 0 = filter pred [b]
--   | otherwise = do
--     move      <- nextPositions b
--     restMoves <- nextPositionsN move (n - 1) pred
--     return restMoves


board = Board 1

test1 = nextPositions board == [Board 1, Board 2]

test2 = nextPositionsN board 1 (const True) == [Board 1, Board 2]

test3 = nextPositionsN board 2 (const True) == [Board 1, Board 2, Board 2, Board 3]

test4 = nextPositionsN board 2 (\(Board x) -> odd x) == [Board 1, Board 3]

test5 = nextPositionsN board 0 (const True) == [board]

test6 = nextPositionsN board (-1) (const True) == []

tests = map (== True) [test1, test2, test3, test4, test5, test6]