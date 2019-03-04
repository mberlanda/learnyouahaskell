module DList where
  import Data.Monoid
  import Data.Semigroup
  import Control.Monad.Writer

  newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

  toDiffList :: [a] -> DiffList a
  toDiffList xs = DiffList (xs++)

  fromDiffList :: DiffList a -> [a]
  fromDiffList (DiffList f) = f []

  -- instance Semigroup DiffList where
  --  DiffList m1 <> DiffList m2 = DiffList (union m1 m2)
  instance Semigroup (DiffList a) where
    (<>) (DiffList m1) (DiffList m2) = DiffList (m1 . m2)
    --  DiffList p <> DiffList q = DiffList $ \a -> p . q a

  instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

  gcd' :: Int -> Int -> Writer (DiffList String) Int
  gcd' a b
    | b == 0 = do
        tell (toDiffList ["Finished with " ++ show a])
        return a
    | otherwise = do
        result <- gcd' b (a `mod` b)
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        return result

  finalCountDown :: Int -> Writer (DiffList String) ()
  finalCountDown 0 = do
      tell (toDiffList ["0"])
  finalCountDown x = do
      finalCountDown (x-1)
      tell (toDiffList [show x])

  reverseFinalCountDown :: Int -> Writer [String] ()
  reverseFinalCountDown 0 = do
      tell ["0"]
  reverseFinalCountDown x = do
      reverseFinalCountDown (x-1)
      tell [show x]
