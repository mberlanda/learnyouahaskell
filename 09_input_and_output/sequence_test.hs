-- file: 09_input_and_output/sequence_test.hs
main = do  
    rs <- sequence [getLine, getLine, getLine]  
    print rs