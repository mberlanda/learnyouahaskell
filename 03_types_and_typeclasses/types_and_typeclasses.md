# 03. Types and Typeclasses

#### Believe the type
```bash
ghci> :t 'a'  
'a' :: Char 
```
#### Type variables
```bash
*Main> :t circumference 
circumference :: Float -> Float
```
#### Typeclasses 101
```bash
ghci> :t (==)
(==) :: (Eq a) => a -> a -> Bool
ghci> :t (>)  
(>) :: (Ord a) => a -> a -> Bool
ghci> :t show
show :: Show a => a -> String
ghci> :t read
read :: Read a => String -> a
ghci> read "[1,2,3,4]" :: [Int]  
[1,2,3,4]
ghci> :t [1..4]
[1..4] :: (Enum t, Num t) => [t]
ghci> :t 20
20 :: Num a => a
ghci> maxBound :: (Bool, Int, Char) 
(True,9223372036854775807,'\1114111')
ghci> :t fromIntegral 
fromIntegral :: (Integral a, Num b) => a -> b
ghci> :t length
length :: [a] -> Int
```