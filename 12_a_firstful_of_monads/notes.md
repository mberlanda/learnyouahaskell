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

## The Monad type class

```hs
Prelude> return "WHAT"
"WHAT"
Prelude> return "WHAT" :: Maybe String
Just "WHAT"
Prelude> Just 9 >>= \x -> return (x*10)
Just 90
Prelude> Nothing >>= \x -> return (x*10)
Nothing
Prelude> Just 9 >> Just "a"
Just "a"
Prelude> Just 9 >> Just 5
Just 5
```
