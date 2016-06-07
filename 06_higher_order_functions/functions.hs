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