-- file: 09_input_and_output/getchar_test.hs
main = do     
    c <- getChar  
    if c /= ' '  
        then do  
            putChar c  
            main  
        else return ()
