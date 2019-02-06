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

## Walk the line

```hs
*Main> landLeft 2 (0,0)
(2,0)
*Main> landRight (-1) (1,2)
(1,1)
*Main>  landLeft 2 (landRight 1 (landLeft 1 (0,0)))
(3,1)
*Main> 100 -: (*3)
300
*Main> (0,0) -: landLeft 1 -: landRight 1 -: landLeft 2
(3,1)
*Main> (/2) $ (/5) 100
10.0
*Main> 100 -: (/5) -: (/2)
10.0
--
*Main> landLeft 2 (0,0)
(2,0)
*Main> mlandLeft 2 (0,0)
Just (2,0)
*Main> landLeft 10 (0,3)
(10,3)
*Main> mlandLeft 10 (0,3)
Nothing
*Main> Just (1, 3) >>= mlandLeft 4
Just (5,3)
*Main> Nothing >>= mlandLeft 4
Nothing
*Main> (0,0) -: landLeft 1 -: landRight 4 -: landLeft (-1) -: landRight (-2)
(0,2)
*Main> return (0,0) >>= mlandLeft 1 >>= mlandRight 4 >>= mlandLeft (-1) >>= mlandRight (-2)
Nothing
--
*Main> return (0,0) >>= mlandLeft 2 >>= banana  >>= mlandLeft (1)
Nothing
*Main> return (0,0) >>= mlandLeft 2 >> Nothing  >>= mlandLeft (1)
Nothing
*Main> return (0,0) >>= mlandLeft 2 >> Just (4, 3)  >>= mlandLeft (1)
Just (5,3)
```