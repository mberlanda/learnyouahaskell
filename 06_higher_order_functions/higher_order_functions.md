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