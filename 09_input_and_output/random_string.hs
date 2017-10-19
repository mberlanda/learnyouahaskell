-- file: 09_input_and_output/random_string.hs

import System.Random

main = do
    gen <- getStdGen
    putStrLn $ take 20 (randomRs ('a','z') gen)
