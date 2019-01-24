module Test where
  class (Functor f) => Applicative f where
      pure :: a -> f a
      (<*>) :: f (a -> b) -> f a -> f b
  -- (<$>) :: (Functor f) => (a -> b) -> f a -> f b

  instance Test.Applicative Maybe where
      pure = Just
      Nothing <*> _ = Nothing
      (Just f) <*> something = fmap f something

  instance Test.Applicative [] where
      pure x = [x]
      fs <*> xs = [f x | f <- fs, x <- xs]

  instance Test.Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)

  instance Test.Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)

  {-
  newtype ZipList a = ZipList { getZipList :: [a] }
  instance Test.Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
  -}
