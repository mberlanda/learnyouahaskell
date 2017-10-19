-- file: 09_input_and_output/random_string2.hs

import System.Random

main = do
    gen <- getStdGen
    putStrLn $ take 20 (randomRs ('a','z') gen)
    -- gen2 <- getStdGen
    gen2 <- newStdGen
    putStr $ take 20 (randomRs ('a','z') gen2)
