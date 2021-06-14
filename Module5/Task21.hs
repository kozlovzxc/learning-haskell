import Control.Monad.State

fibStep :: State (Integer, Integer) ()
fibStep = state $ \(prev, cur) -> ((), (cur, prev + cur))

execStateN :: Int -> State s a -> s -> s
execStateN n m = execState (replicateM n m)

fib :: Int -> Integer
fib n = fst $ execStateN n fibStep (0, 1)
