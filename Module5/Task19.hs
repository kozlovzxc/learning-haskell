import Control.Monad.Writer
import Data.Monoid

type Shopping = Writer ([String], Sum Integer) ()

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans" 19200
  purchase "Water" 180
  purchase "Lettuce" 328

purchase :: String -> Integer -> Shopping
purchase item cost = writer ((), ([item], Sum cost))

total :: Shopping -> Integer
total = getSum . snd . execWriter

items :: Shopping -> [String]
items = fst . execWriter