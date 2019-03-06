# For a Few Monads More

## Writer? I hardly know her!

```hs
[1 of 1] Compiling Writer           ( 13_for_a_few_monads_more/log.hs, interpreted )
Ok, one module loaded.
*Log> (3, "Smallish gang.") `applyLog` isBigGang
(False,"Smallish gang.Compared gang size to 9.")
*Log> (30, "A freaking platoon.") `applyLog` isBigGang
(True,"A freaking platoon.Compared gang size to 9.")
*Log> ("Tobin","Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))
(5,"Got outlaw name.Applied length.")
*Log> ("Bathcat","Got outlaw name.") `applyLog` (\x -> (length x, "Applied length"))
(7,"Got outlaw name.Applied length")
```

```hs
[1 of 1] Compiling Main             ( 13_for_a_few_monads_more/food.hs, interpreted )
Ok, one module loaded.
*Main> ("beans", Sum 10) `applyLog` addDrink
("milk",Sum {getSum = 35})
*Main>  ("jerky", Sum 25) `applyLog` addDrink
("whiskey",Sum {getSum = 124})
*Main>  ("dogmeat", Sum 5) `applyLog` addDrink
("beer",Sum {getSum = 35})
*Main> ("dogmeat", Sum 5) `applyLog` addDrink `applyLog` addDrink
("beer",Sum {getSum = 65})
```

Writer implementation:

```hs
newtype Writer w a = Writer { runWriter :: (a, w) }

instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
```

```hs
[1 of 1] Compiling Main             ( 13_for_a_few_monads_more/logW.hs, interpreted )
Ok, one module loaded.
*Main> runWriter multWithLog
(15,["Got number: 3","Got number: 5","Gonna multiply these 2"])
```

```
13_for_a_few_monads_more $ stack runhaskell gcd.hs
1
8 mod 3 = 2
3 mod 2 = 1
2 mod 1 = 0
Finished with 1
```

```hs
DList> fromDiffList (toDiffList [1,2,3,4] `mappend` toDiffList [1,2,3])
[1,2,3,4,1,2,3]
DList> mapM_ putStrLn . fromDiffList . snd . runWriter $ gcd' 110 34
Finished with 2
8 mod 2 = 0
34 mod 8 = 2
110 mod 34 = 8

DList> mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown 500
DList> mapM_ putStrLn . snd . runWriter $ reverseFinalCountDown 500
```

## Reader? Ugh, not this joke again.

```hs
-- functors
let f = (*5)
let g = (+3)
(fmap f g) 8
-- applicative functors
(+) <$> (*2) <*> (+10) $ 8
```

```hs
instance Monad ((->) r) where
    return x = \_ -> x
    h >>= f = \w -> f (h w) w
```
