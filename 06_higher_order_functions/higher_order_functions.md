# Higher Order Functions

#### Curried functions

> Every function in Haskell officially only takes one parameter
> Putting a space between two things is simply function application.
> The space is sort of like an operator and it has the highest precedence.

Example of max function
```
Prelude> max 6 8
8
Prelude> (max 6) 8
8
```
```hs
max :: (Ord a) => a -> a -> a
max :: (Ord a) => a -> (a -> a)
```

```hs
multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z
multThree :: (Num a) => a -> (a -> (a -> a))
```

>  If we call a function with too few parameters, we get back a partially applied function, meaning a function that takes as many parameters as we left out. Using partial application (calling functions with too few parameters, if you will) is a neat way to create functions on the fly so we can pass them to another function or to seed them with some data.

```
ghci> let multTwoWithNine = multThree 9  
ghci> multTwoWithNine 2 3  
54  
ghci> let multWithEighteen = multTwoWithNine 2  
ghci> multWithEighteen 10  
180
```

#### Some higher-orderism is in order

```hs
applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)
```
```
ghci> applyTwice (+3) 10  
16  
ghci> applyTwice (++ " HAHA") "HEY"  
"HEY HAHA HAHA"  
ghci> applyTwice ("HAHA " ++) "HEY"  
"HAHA HAHA HEY"  
ghci> applyTwice (multThree 2 2) 9  
144  
ghci> applyTwice (3:) [1]  
[3,3,1]
ghci> zipWith' (+) [4,2,5,6] [2,6,2,3]  
[6,8,7,9]
ghci> flip' zip [1,2,3,4,5] "hello"  
[('h',1),('e',2),('l',3),('l',4),('o',5)] 
```
#### Lambdas
> Normally, we make a lambda with the sole purpose of passing it to a higher­order
function. To make a lambda, we write a  \ (because it kind of looks like the greek letter
lambda if you squint hard enough) and then we write the parameters, separated by spaces.
After that comes a  ­> and then the function body. We usually surround them by
parentheses, because otherwise they extend all the way to the right.

#### Only folds and horses
> The foldl1 and foldr1 functions work much like foldl and foldr, only you don't need to provide them with an explicit starting value. They assume the first (or last) element of the list to be the starting value and then start the fold with the element next to it.
> scanl and scanr are like foldl and foldr, only they report all the intermediate accumulator states in the form of a list. There are also scanl1 and scanr1, which are analogous to foldl1 and foldr1.
```
ghci> scanl (+) 0 [3,5,2,1]  
[0,3,8,10,11]  
ghci> scanr (+) 0 [3,5,2,1]  
[11,8,3,1,0]  
ghci> scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]  
[3,4,5,5,7,9,9,9]  
ghci> scanl (flip (:)) [] [3,2,1]  
[[],[3],[2,3],[1,2,3]]  
```
```
ghci> sqrtSums  
131  
ghci> sum (map sqrt [1..131])  
1005.0942035344083  
ghci> sum (map sqrt [1..130])  
993.6486803921487  
```