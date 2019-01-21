import System.IO
import Data.List

{-
  We need to describe:
   - A road time
   - B road time
   - C cost of switching between roads
-}
data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem
    in  if sum (map snd bestAPath) <= sum (map snd bestBPath)
            then reverse bestAPath
            else reverse bestBPath

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let priceA = sum $ map snd pathA -- pathA is a tuple (Label, Int)
        priceB = sum $ map snd pathB -- pathB is a tuple (Label, Int)
        forwardPriceToA = priceA + a
        crossPriceToA   = priceB + b + c
        forwardPriceToB = priceB + b
        crossPriceToB   = priceB + a + c
        newPathToA = if forwardPriceToA <= crossPriceToA
                        then (A,a):pathA
                        else (C,c):(B,b):pathB
        newPathToB = if forwardPriceToB <= crossPriceToB
                        then (B,b):pathB
                        else (C,c):(A,a):pathA
    in  (newPathToA, newPathToB)

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

main = do
    contents <- getContents
    let threes = groupsOf 3 (map read $ lines contents)
        roadSystem = map (\[a,b,c] -> Section a b c) threes
        path = optimalPath roadSystem
        pathString = concat $ map (show . fst) path
        pathPrice = sum $ map snd path
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "The price is: " ++ show pathPrice

