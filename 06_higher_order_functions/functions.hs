-- file: 06_higher_order_functions
-- Curried functions

compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred x = compare 100 x

compareWithHundred' :: (Num a, Ord a) => a -> Ordering  
compareWithHundred' = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool  
isUpperAlphanum = (`elem` ['A'..'Z'])

-- Some higher-orderism is in order
applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)

-- zipWith: It takes a function and two lists as parameters and then joins the two lists by applying the function between corresponding elements
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- flip: it simply takes a function and returns a function that is like our original function, only the first two arguments are flipped. 
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g  
    where g x y = f y x 
-- because of curried functions, we can write it as follows
flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f x y = f y x

-- maps and filters

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x       = x : filter' p xs
  | otherwise = filter' p xs

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = smSorted ++ [x] ++ bgSorted
  where smSorted = qsort (filter' (<=x) xs)
        bgSorted = qsort (filter' (>x) xs)

chain :: (Integral a) => a => [a]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n  = n : chain (n*3 + 1)