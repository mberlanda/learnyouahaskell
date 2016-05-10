removeNonUppercase :: [Char] -> [Char]  
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z
-- an Integer function 
factorial :: Integer -> Integer  
factorial n = product [1..n]
-- a Float function
circumference :: Float -> Float  
circumference r = 2 * pi * r
-- a Double function
circumference' :: Double -> Double  
circumference' r = 2 * pi * r  