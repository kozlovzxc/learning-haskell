data SomeType a = SomeType a

instance Functor SomeType where
    fmap f x = x >>= (return.f)
