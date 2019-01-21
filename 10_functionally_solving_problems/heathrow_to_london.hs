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
    let (bestAPath, bestBPath, sumAPath, sumBPath) = foldl' roadStep ([],[], 0, 0) roadSystem
    in  if sumAPath <= sumBPath
            then reverse bestAPath
            else reverse bestBPath

roadStep :: (Path, Path, Int, Int) -> Section -> (Path, Path, Int, Int)
roadStep (pathA, pathB, priceA, priceB) (Section a b c) =
    let forwardPriceToA = priceA + a
        crossPriceToA   = priceB + b + c
        forwardPriceToB = priceB + b
        crossPriceToB   = priceB + a + c
        (newPathToA, newPriceA) = if forwardPriceToA <= crossPriceToA
                                    then ((A,a):pathA, forwardPriceToA)
                                    else ((C,c):(B,b):pathB, crossPriceToB)
        (newPathToB, newPriceB) = if forwardPriceToB <= crossPriceToB
                                  then ((B,b):pathB, forwardPriceToB)
                                  else ((C,c):(A,a):pathA, crossPriceToA)
    in  (newPathToA, newPathToB, newPriceA, newPriceB)

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

{-
  -- I did not manage to write a working generator of [Section]
  import Data.Random -- stack install random-fu
  import System.Random

  randomRoadSystem :: StdGen -> Int -> RoadSystem -> RoadSystem
  randomRoadSystem g n xs = fst $ sampleState (sample n xs) g
-}

main = do
    contents <- getContents
    let threes = groupsOf 3 (map read $ lines contents)
        roadSystem = map (\[a,b,c] -> Section a b c) threes
        path = optimalPath roadSystem
        pathString = concat $ map (show . fst) path
        pathPrice = sum $ map snd path
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "The price is: " ++ show pathPrice

