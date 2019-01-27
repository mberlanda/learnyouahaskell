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

## Applicative functors

`Applicative` typeclass found in the `Control.Applicative` module.

```hs
Prelude> let a = fmap (*) [1,2,3,4]
Prelude> :t a
a :: Num a => [a -> a]
Prelude> fmap (\f -> f 9) a
[9,18,27,36]
```

If we try with `Maybe`:

```hs
Prelude> :t fmap Just(3 *)
fmap Just(3 *) :: Num a => a -> Maybe a
Prelude> :t fmap Just(3 *) 5
fmap Just(3 *) 5 :: Num t => Maybe t
Prelude> fmap Just(3 *) 5
Just 15
Prelude> fmap Just(3 *) Just 5

<interactive>:15:1: error:
    • Couldn't match type ‘Maybe (a0 -> Maybe a0)’ with ‘Integer -> t’
      Expected type: (a0 -> Maybe a0) -> Integer -> t
        Actual type: (a0 -> Maybe a0) -> Maybe (a0 -> Maybe a0)
    • The function ‘fmap’ is applied to four arguments,
      but its type ‘((a0 -> Maybe a0) -> Maybe (a0 -> Maybe a0))
                    -> ((a0 -> Maybe a0) -> a0 -> Maybe a0)
                    -> (a0 -> Maybe a0)
                    -> Maybe (a0 -> Maybe a0)’
      has only three
      In the expression: fmap Just (3 *) Just 5
      In an equation for ‘it’: it = fmap Just (3 *) Just 5
    • Relevant bindings include it :: t (bound at <interactive>:15:1)
```

Applicative functor:

```hs
Prelude> :m Control.Applicative
Prelude Control.Applicative> :t pure
pure :: Applicative f => a -> f a
Prelude Control.Applicative> :t (<*>)
(<*>) :: Applicative f => f (a -> b) -> f a -> f b

Prelude Control.Applicative> Just(3 *) <*> Just 5
Just 15
Prelude Control.Applicative> pure Just(3 *) <*> Just 5

<interactive>:26:1: error:
    • Couldn't match type ‘a0 -> Maybe a0’ with ‘Maybe (Integer -> b)’
      Expected type: (Integer -> Integer) -> Maybe (Integer -> b)
        Actual type: (Integer -> Integer) -> a0 -> Maybe a0
    • The function ‘pure’ is applied to two arguments,
      but its type ‘(a0 -> Maybe a0)
                    -> (Integer -> Integer) -> a0 -> Maybe a0’
      has only three
      In the first argument of ‘(<*>)’, namely ‘pure Just (3 *)’
      In the expression: pure Just (3 *) <*> Just 5
    • Relevant bindings include
        it :: Maybe b (bound at <interactive>:26:1)
Prelude Control.Applicative> pure (*) <*> Just(3) <*> Just 5
Just 15

Prelude Control.Applicative> :t (<$>)
(<$>) :: Functor f => (a -> b) -> f a -> f b
```

Note that:

```hs
pure (*) <*> Just(3) <*> Just 5
-- is equivalent to
(*) <$> Just(3) <*> Just 5
```

List type constructor:

```hs
Prelude Control.Applicative> pure "Hey" :: [String]
["Hey"]
Prelude Control.Applicative> pure "Hey" :: Maybe String
Just "Hey"
Prelude Control.Applicative> [(*0),(+100),(^2)] <*> [1,2,3]
[0,0,0,101,102,103,1,4,9]
Prelude Control.Applicative> [(+),(*)] <*> [1,2] <*> [3,4]
[4,5,5,6,3,4,6,8]
Prelude Control.Applicative> (*) <$> [2,5,10] <*> [8,10,11]
[16,20,22,40,50,55,80,100,110]
Prelude Control.Applicative> pure (*) <*> [2,5,10] <*> [8,10,11]
[16,20,22,40,50,55,80,100,110]
Prelude Control.Applicative> (*) <$> [2,5,10] <*> [8,10,11]
[16,20,22,40,50,55,80,100,110]
Prelude Control.Applicative> filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]
[55,80,100,110
```

Considering `IO`:
```hs
myAction :: IO String
myAction = do
    a <- getLine
    b <- getLine
    return $ a ++ b

-- can be rewritten as
myAction :: IO String
myAction = (++) <$> getLine <*> getLine
```

```
$ stack runhaskell 11_functors_applicative_functors_and_monoids/app_io.hs
ccccc
ddddd
The two lines concatenated turn out to be: cccccddddd
```

Considering `(->) r`
```hs
pure :: a -> (r -> a)
pure x = (\_ -> x)
f <*> g = \x -> f x (g x)
```
```
Prelude> (pure 3) "blah"
3
Prelude> pure 3 "blah"
3
Prelude> :t (+) <$> (+3) <*> (*100)
(+) <$> (+3) <*> (*100) :: Num b => b -> b
Prelude> (+) <$> (+3) <*> (*100) $ 5
508
Prelude> ((+3) 5) + ((*100) 5)
508
Prelude> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5
[8.0,10.0,2.5]
Prelude> (\x y -> [x,y]) <$> (+3) <*> (*2) $ 5
[8,10]
Prelude> :t (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2)
(\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) :: Fractional a => a -> [a]
Prelude> (\x y w z -> [x,y,w,z]) <$> (+3) <*> (*2) <*> (/2) <*> (/4) $ 5
[8.0,10.0,2.5,1.25]
```

Additional functors and functions defined in `Control.Applicative`:

```hs
Prelude> :m Control.Applicative
Prelude Control.Applicative> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]
[101,102,103]
Prelude Control.Applicative> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]
[101,102,103]
Prelude Control.Applicative> getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"
[('d','c','r'),('o','a','a'),('g','t','t')]
Prelude Control.Applicative> getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]
[5,3,3,4]

liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b
Prelude Control.Applicative> fmap (\x -> [x]) (Just 4)
Just [4]
Prelude Control.Applicative> liftA2 (:) (Just 3) (Just [4])
Just [3,4]
Prelude Control.Applicative> (:) <$> Just 3 <*> Just [4]
Just [3,4]

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])

Prelude Control.Applicative> sequenceA [Just 3, Just 2, Just 1]
Just [3,2,1]
Prelude Control.Applicative>  sequenceA [Just 3, Nothing, Just 1]
Nothing
Prelude Control.Applicative> sequenceA [(+3),(+2),(+1)] 3
[6,5,4]
Prelude Control.Applicative> sequenceA [[1,2,3],[4,5,6]]
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
Prelude Control.Applicative> sequenceA [[1,2,3],[4,5,6],[3,4,4],[]]
[]

Prelude Control.Applicative> map (\f -> f 7) [(>4),(<10),odd]
[True,True,True]
Prelude Control.Applicative> and $ map (\f -> f 7) [(>4),(<10),odd]
True
Prelude Control.Applicative> sequenceA [(>4),(<10),odd] 7
[True,True,True]
Prelude Control.Applicative> and $ sequenceA [(>4),(<10),odd] 7
True
```

Summary of applicative functor's laws:

* `pure f <*> x = fmap f x`
* `pure id <*> v = v`
* `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`
* `pure f <*> pure x = pure (f x)`
* `u <*> pure y = pure ($ y) <*> u`

## The newtype keyword

```hs
Prelude> data ZipList a = ZipList [a]
Prelude> :t ZipList
ZipList :: [a] -> ZipList a
Prelude> data ZipList a = ZipList { getZipList :: [a] }
Prelude> :t ZipList
ZipList :: [a] -> ZipList a
Prelude> :t getZipList
getZipList :: ZipList a -> [a]
```

```hs
data CoolBool = CoolBool { getCoolBool :: Bool }
helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"
{-
Prelude> helloMe undefined
"*** Exception: Prelude."
-}
newtype CoolBool = CoolBool { getCoolBool :: Bool }
-- helloMe undefined
-- "hello"
```

type vs. newtype vs. data :

* `type` keyword is for making type synonyms
```hs
type IntList = [Int]
-- ([1,2,3] :: IntList) ++ ([1,2,3] :: [Int])
-- [1,2,3,1,2,3]
```
* `newtype` keyword is for taking existing types and wrapping them in new types
```hs
newtype CharList = CharList { getCharList :: [Char] }
{-
Prelude> getCharList $ CharList "abc"
"abc"
Prelude> getCharList $ ( CharList "abc" ++ Charlist "def")

<interactive>:24:35: error:
    • Data constructor not in scope: Charlist :: [Char] -> [a0]
    • Perhaps you meant ‘CharList’ (line 20)
-}
```
* `data` keyword is for making new data types
