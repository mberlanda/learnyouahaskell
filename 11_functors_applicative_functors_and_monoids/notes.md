# Functors, Applicative Functors and Monoids

## Functors redux

> Functors are things that can be mapped over, like lists, Maybes, trees, and such. In Haskell, they're described by the typeclass `Functor`, which has only one typeclass method, namely `fmap`, which has a type of `fmap :: (a -> b) -> f a -> f b`.

```
$ stack runhaskell backwards.hs
abcd
You said dcba backwards!
Yes, you really said dcba backwards!
```

```
$ stack runhaskell fmapping_io.hs
ABCD EFGH
H-G-F-E- -D-C-B-A
```

```hs
Prelude> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
Prelude> :t fmap (*3)
fmap (*3) :: (Functor f, Num b) => f b -> f b
Prelude> :t fmap (*3) (+100)
fmap (*3) (+100) :: Num b => b -> b
Prelude> :t (*3) . (+100)
(*3) . (+100) :: Num c => c -> c
```

*The first functor law states that if we map the id function over a functor, the functor that we get back should be the same as the original functor.*

`fmap id = id`

```
Prelude> fmap id [1..5]
[1,2,3,4,5]
```

*The second law says that composing two functions and then mapping the resulting function over a functor should be the same as first mapping one function over the functor and then mapping the other one*

`fmap (f . g) = fmap f . fmap g`
