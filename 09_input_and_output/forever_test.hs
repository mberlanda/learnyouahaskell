-- file: 09_input_and_output/forever_test.hs

import Control.Monad
import Data.Char
  
main = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l 