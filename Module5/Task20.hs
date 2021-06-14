import Control.Monad.Reader
import Control.Monad.State

readerToState :: Reader r a -> State r a
readerToState m = state $ \x -> (runReader m x, x)
