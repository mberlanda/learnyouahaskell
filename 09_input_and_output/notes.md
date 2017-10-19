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

#### Files and streams

> `getContents` is an I/O action that reads everything from the standard input until it encounters an end-of-file character.

```
$ cat haiku.txt
I'm a lil' teapot
What's with that airplane food, huh?
It's so small, tasteless
$ ghc --make capslocker.hs
[1 of 1] Compiling Main             ( capslocker.hs, capslocker.o )
Linking capslocker ...
$ cat haiku.txt | ./capslocker
I'M A LIL' TEAPOT
WHAT'S WITH THAT AIRPLANE FOOD, HUH?
IT'S SO SMALL, TASTELESS
capslocker: <stdin>: hGetLine: end of file

cat shortlines.txt | runhaskell shortlinesonly.hs
```

> `interact` takes a function of type `String -> String` as a parameter and returns an I/O action that will take some input, run that function on it and then print out the function's result. Let's modify our program to use that.

```hs
# e.g. shortlinesonly refactoring
main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in  result

main = interact $ unlines . filter ((<10) . length) . lines
```
```
# palindromes.hs
$ cat words.txt | runhaskell palindromes.hs
$ runhaskell girlfriend.hs
```

```
openFile :: FilePath -> IOMode -> IO Handle

type FilePath = String
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

hGetContents :: Handle -> IO String
hClose :: Handle -> IO ()

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result
```

`hGetLine`, `hPutStr`, `hPutStrLn`, `hGetChar` ...

```
readFile :: FilePath -> IO String
import System.IO

main = do
    contents <- readFile "girlfriend.txt"
    putStr contents
```

`writeFile`, `appendFile` ...

```
$ runhaskell appendtodo.hs
Iron the dishes
$ runhaskell appendtodo.hs
Dust the dog
$ runhaskell appendtodo.hs
Take salad out of the oven
$ cat todo.txt
Iron the dishes
Dust the dog
Take salad out of the oven
```

```hs
main = do
    withFile "something.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)

main = do
    withFile "something.txt" ReadMode (\handle -> do
        hSetBuffering handle $ BlockBuffering (Just 2048)
        contents <- hGetContents handle
        putStr contents)
```

> You can control how exactly buffering is done by using the hSetBuffering function. It takes a handle and a BufferMode and returns an I/O action that sets the buffering. BufferMode is a simple enumeration data type and the possible values it can hold are: NoBuffering, LineBuffering or BlockBuffering (Maybe Int). The Maybe Int is for how big the chunk should be, in bytes. If it's Nothing, then the operating system determines the chunk size. NoBuffering means that it will be read one character at a time. NoBuffering usually sucks as a buffering mode because it has to access the disk so much.

`$ runhaskell deletetodo.hs`


#### Command line arguments

> The System.Environment module has two cool I/O actions. One is getArgs, which has a type of getArgs :: IO [String] and is an I/O action that will get the arguments that the program was run with and have as its contained result a list with the arguments. getProgName has a type of getProgName :: IO String and is an I/O action that contains the program name.

```
$ runhaskell arg-test.hs abc 123 "multi word arg" w00t
The arguments are:
abc
123
multi word arg
w00t
The program name is:
arg-test.hs

# todo.hs
$ runhaskell todo.hs view "todo.txt"
0 - Iron the dishes
1 - Dust the dog
2 - Take salad out of the oven
$ runhaskell todo.hs add "todo.txt" "Another task to add"
$ runhaskell todo.hs view "todo.txt"
0 - Iron the dishes
1 - Dust the dog
2 - Take salad out of the oven
3 - Another task to add
$ runhaskell todo.hs remove "todo.txt" 3
$ runhaskell todo.hs view "todo.txt"
0 - Iron the dishes
1 - Dust the dog
2 - Take salad out of the oven
```

#### Randomness

We could define a random function such as the following:

```hs
randomNumber :: (Num a) => a
randomNumber = 4
```

However, this would not be very useful. We can use instead `System.Random`:

```
Prelude> :m System.Random
Prelude System.Random> :t random
random :: (RandomGen g, Random a) => g -> (a, g)
Prelude System.Random> :t mkStdGen
mkStdGen :: Int -> StdGen
Prelude System.Random> mkStdGen 100
101 1

Prelude System.Random> random (mkStdGen 100) :: (Int, StdGen)
(-3633736515773289454,693699796 2103410263)
Prelude System.Random> random (mkStdGen 100) :: (Int, StdGen)
(-3633736515773289454,693699796 2103410263)
Prelude System.Random> random (mkStdGen 100) :: (Int, StdGen)
(-3633736515773289454,693699796 2103410263)

Prelude System.Random> random (mkStdGen 949488) :: (Int, StdGen)
(9159618695640234475,587416689 2103410263)
Prelude System.Random> random (mkStdGen 949488) :: (Float, StdGen)
(0.3718226,1597344447 1655838864)
Prelude System.Random> random (mkStdGen 949488) :: (Bool, StdGen)
(False,1485632275 40692)
Prelude System.Random> random (mkStdGen 949488) :: (Integer, StdGen)
(9159618695640234475,587416689 2103410263)

```

Three coins example:

```
-- file: 09_input_and_output/three_coins.hs
Prelude System.Random> :l three_coins.hs
[1 of 1] Compiling Main             ( three_coins.hs, interpreted )
Ok, modules loaded: Main.
*Main System.Random> threeCoins (mkStdGen 11)
(True,True,True)
*Main System.Random> threeCoins (mkStdGen 12)
(True,False,True)
*Main System.Random> threeCoins (mkStdGen 13)
(True,True,False)
```

Randoms:
```
*Main System.Random> :t randoms
randoms :: (RandomGen g, Random a) => g -> [a]
*Main System.Random> take 5 $ randoms (mkStdGen 11) :: [Int]
[5260538044923710387,4361398698747678847,-8221315287270277529,7278185606566790575,1652507602255180489]
*Main System.Random> take 5 $ randoms (mkStdGen 11) :: [Bool]
[True,True,True,True,False]
*Main System.Random> take 5 $ randoms (mkStdGen 11) :: [Float]
[0.26201087,0.1271351,0.31857032,0.1921351,0.31495118]

Prelude> :l randoms.hs
*Main> take 5 $ randoms' (mkStdGen 11)
[5260538044923710387,4361398698747678847,-8221315287270277529,7278185606566790575,1652507602255180489]
*Main> finiteRandoms 5 (mkStdGen 11)
([5260538044923710387,4361398698747678847,-8221315287270277529,7278185606566790575,1652507602255180489],912247095 2118231989)
```

Other randoms:
```
Prelude> :m System.Random
-- random in range
Prelude System.Random> :t randomR
randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
Prelude System.Random> randomR (1,6) (mkStdGen 359353)
(6,1494289578 40692)
Prelude System.Random> randomR (1,6) (mkStdGen 35935335)
(3,1250031057 40692)

Prelude System.Random> :t randomRs
randomRs :: (RandomGen g, Random a) => (a, a) -> g -> [a]
Prelude System.Random> take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]
"xnuhlfwywq"

```
