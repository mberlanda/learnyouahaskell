# Input and Output

#### Hello, world!

```
# -- file: 09_input_and_output/helloworld.hs
main = putStrLn "hello, world"

$ ghc --make helloworld
[1 of 1] Compiling Main             ( helloworld.hs, helloworld.o )
Linking helloworld ...
$ ./helloworld 
hello, world

ghci> :t putStrLn  
putStrLn :: String -> IO ()  
ghci> :t putStrLn "hello, world"  
putStrLn "hello, world" :: IO () 

main = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!")

$ runhaskell putstr_test.hs 
```

function | type |example
---------|------|-------
`putStr` | `putStr :: String -> IO ()` | putstr_test.hs
`putChar` | `putChar :: Char -> IO ()` | putchar_test.hs
`print` | `print :: Show a => a -> IO ()` | print_test.hs 
`getChar` | `getChar :: IO Char` | getchar_test.hs
`when` | | when_test.hs
`sequence` | `sequence :: [IO a] -> IO [a]` | sequence_test.hs
`mapM` | |
`mapM_` | |
`forever`| | forever_test.hs
`forM` | | form_test.hs

```hs
# putStr recursive definition

    putStr :: String -> IO ()  
    putStr [] = return ()  
    putStr (x:xs) = do  
        putChar x  
        putStr xs  
```

> mapM takes a function and a list, maps the function over the list and then sequences it. mapM_ does the same, only it throws away the result later.

```
ghci> mapM print [1,2,3]  
1  
2  
3  
[(),(),()]  
Prelude Control.Monad> forM [1,2,3] print
1
2
3
[(),(),()]
ghci> mapM_ print [1,2,3]  
1  
2  
3
```