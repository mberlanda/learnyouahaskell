-- file: 09_input_and_output/when_test.hs

import Control.Monad   
  
main = do  
    c <- getChar  
    when (c /= ' ') $ do  
        putChar c  
        main 