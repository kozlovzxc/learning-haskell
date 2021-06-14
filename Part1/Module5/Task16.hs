type User = String

type Password = String

type UsersTable = [(User, Password)]

data Reader r a = Reader {runReader :: (r -> a)}

instance Functor (Reader e) where
  fmap f (Reader r) = Reader $ f . r

instance Applicative (Reader e) where
  pure a = Reader $ \x -> a
  (Reader f) <*> (Reader g) = Reader $ \x -> f x (g x)

instance Monad (Reader r) where
  return x = Reader $ \_ -> x
  m >>= k = Reader $ \r -> runReader (k (runReader m r)) r

asks :: (r -> a) -> Reader r a
asks = Reader

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = do
  asks $ map fst . filter (\user -> snd user == "123456")