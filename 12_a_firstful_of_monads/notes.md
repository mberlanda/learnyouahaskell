# A Firstful of Monads

## Getting our feet wet with Maybe

```hs
fmap :: (Functor f) => (a -> b) -> f a -> f b
Prelude> fmap Just(2 *) 8
Just 16
Prelude> fmap (++"!") (Just "wisdom")
Just "wisdom!"
Prelude> fmap (++"!") Nothing
Nothing

(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
Prelude> Just (2*) <*> Just 8
Just 16
Prelude> (*) <$> Just 2 <*> Just 8
Just 16

(>>=) :: (Monad m) => m a -> (a -> m b) -> m b -- bind
```

```hs
*Main> applyMaybe (Just 3) (\x -> Just (x+1))
Just 4
*Main> applyMaybe Nothing (\x -> Just (x+1))
Nothing
*Main> Just 3 >>= \x -> Just (x+1)
Just 4
*Main> Nothing >>= \x -> Just (x+1)
Nothing
```
