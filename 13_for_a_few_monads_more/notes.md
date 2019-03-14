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

## Tasteful stateful computations

```hs
*Main System.Random> threeCoins $ mkStdGen 1000

-- Stacks and Stones
*Main System.Random> stackManip [5,8,2,1]
(5,[8,2,1])
*Main System.Random> stackManip' [5,8,2,1]
(5,[8,2,1])
```

`Control.Monad.State` is defined as:

```hs
newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where
    return x = State $ \s -> (x,s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in  g newState

-- book definition
get :: MonadState s m => m s
get = State $ \s -> (s,s)

put :: MonadState s m => s -> m ()
put newState = State $ \s -> ((),newState)
```

```hs
-- https://hackage.haskell.org/package/mtl-2.2.1/docs/src/Control.Monad.State.Class.html#state
-- | Minimal definition is either both of @get@ and @put@ or just @state@
class Monad m => MonadState s m | m -> s where
    -- | Return the state from the internals of the monad.
    get :: m s
    get = state (\s -> (s, s))

    -- | Replace the state inside the monad.
    put :: s -> m ()
    put s = state (\_ -> ((), s))

    -- | Embed a simple state action into the monad.
    state :: (s -> (a, s)) -> m a
    state f = do
      s <- get
      let ~(a, s') = f s
      put s'
      return a
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
    {-# MINIMAL state | get, put #-}
#endif
```

```hs
-- stateful sstack.hs
*Main System.Random> runState stackManip [5,8,2,1]
(5,[8,2,1])
-- stateful scoin.hs
Prelude> :l 13_for_a_few_monads_more/scoins.hs
*Scoin> runState threeCoins (mkStdGen 33)
((True,False,True),680029187 2103410263)
```

## Error error on the wall

The `Control.Monad.Error` implementation looks like:

```hs
instance (Error e) => Monad (Either e) where
    return x = Right x
    Right x >>= f = f x
    Left err >>= f = Left err
    fail msg = Left (strMsg msg)
```

The module is deprecated:

```
Prelude Control.Monad> import Control.Monad.Error

<interactive>:1:1: warning: [-Wdeprecations]
    Module ‘Control.Monad.Error’ is deprecated:
      Use "Control.Monad.Except" instead
```

<https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Except.html>
does not implement `strMsg` used in the examples.

Haskell implementation seems to have changed a lot since the book was written:

```hs
Prelude Control.Monad.Except>  Left "boom" >>= \x -> return (x+1)
Left "boom"
Prelude Control.Monad.Except> Right 100 >>= \x -> Left "no way!"
Left "no way!"
Prelude Control.Monad.Except> Right 3 >>= \x -> return (x + 100)
Right 103
Prelude Control.Monad.Except> Right 3 >>= \x -> return (x + 100) :: Either String Int
Right 103
```
