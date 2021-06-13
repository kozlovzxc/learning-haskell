data Log a = Log [String] a deriving (Show)

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log aMsg a) f = Log newMsg b
  where
    (Log bMsg b) = f a
    newMsg = aMsg ++ bMsg

returnLog :: a -> Log a
returnLog = Log []

instance Functor Log where
  fmap f (Log msg x) = Log msg (f x)

instance Applicative Log where
  pure = Log []
  (Log fMsg f) <*> (Log xMsg x) = Log (fMsg ++ xMsg) (f x)

instance Monad Log where
  return = returnLog
  (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList x = foldl (>>=) (return x)