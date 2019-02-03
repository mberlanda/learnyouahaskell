{-# LANGUAGE MultiParamTypeClasses #-}

module M where
  class Monoid m where
    -- mempty represents the identity value for a particular monoid
    -- it does not take any paramenter, it behaves like polymorphic constant
    mempty :: m
    -- binary function that takes two monoid values and returns a third
    mappend :: m -> m -> m
    -- reduces a list of monoid values to a single value by doing mappend
    mconcat :: [m] -> m
    mconcat = foldr M.mappend M.mempty

  -- Lists are monoids
  instance M.Monoid [a] where
    mempty = []
    mappend = (++)

  -- Product and Sum
  newtype Product a =  Product { getProduct :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

  instance Num a => M.Monoid (M.Product a) where
    mempty = M.Product 1
    M.Product x `mappend` M.Product y = M.Product (x * y)

  newtype Sum a =  Sum { getSum :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

  instance Num a => M.Monoid (M.Sum a) where
    mempty = M.Sum 0
    M.Sum x `mappend` M.Sum y = M.Sum (x + y)

    {-
      *M> getSum $ M.Sum 2 `M.mappend` M.Sum 9
        11
      *M> getSum . M.mconcat . map  M.Sum $ [1,2,3,4]
        10
    -}

  -- Any and All

  newtype Any = Any { getAny :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

  instance M.Monoid M.Any where
        mempty = M.Any False
        M.Any x `mappend` M.Any y = M.Any (x || y)

  newtype All = All { getAll :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

  instance M.Monoid M.All where
        mempty = M.All True
        M.All x `mappend` M.All y = M.All (x && y)

  -- Ordering
  instance M.Monoid Ordering where
      mempty = EQ
      LT `mappend` _ = LT
      EQ `mappend` y = y
      GT `mappend` _ = GT

  -- in the Monoid instance for Ordering, x `mappend` y doesn't equal y `mappend` x

  -- Maybe the monoid
  instance M.Monoid a => M.Monoid (Maybe a) where
    mempty = Nothing
    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `M.mappend` m2)

  newtype First a = First { getFirst :: Maybe a }
    deriving (Eq, Ord, Read, Show)

  instance M.Monoid (M.First a) where
    mempty = M.First Nothing
    M.First (Just x) `mappend` _ = M.First (Just x)
    M.First Nothing `mappend` x = x

  newtype Last a = Last { getLast :: Maybe a }
    deriving (Eq, Ord, Read, Show)

  instance M.Monoid (M.Last a) where
    mempty = M.Last Nothing
    _ `mappend` M.Last (Just x) = M.Last (Just x)
    x`mappend` M.Last Nothing = x
