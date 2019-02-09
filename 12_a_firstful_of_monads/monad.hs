module T where
  class Monad m where
  -- class (Applicative m) = > Monad m where
    return :: a -> m a -- equivalient to Applicative pure

    (>>=) :: m a -> (a -> m b) -> m b -- bind

    (>>) :: m a -> m b -> m b
    x >> y = x T.>>= \_ -> y

    fail :: String -> m a
    fail msg = error msg

  instance T.Monad Maybe where
      return x = Just x
      Nothing >>= f = Nothing -- same as applyMaybe
      Just x >>= f  = f x
      fail _ = Nothing

  instance T.Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
    fail _ = []
