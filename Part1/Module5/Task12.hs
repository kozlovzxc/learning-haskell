pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x
  | x <= 0 = []
  | otherwise = do
    c <- [1 .. x]
    b <- [1 .. c]
    a <- [1 .. b]
    [True | c * c == a * a + b * b]
    return (a, b, c)