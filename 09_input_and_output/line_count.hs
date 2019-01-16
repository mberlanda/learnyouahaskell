-- file: 09_input_and_output/line_count.hs

import System.Environment
import System.IO
import System.Directory

{- Original Implementation
main = do (fileName:_) <- getArgs
          contents <- readFile fileName
          putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
-}

main = do (fileName:_) <- getArgs
          -- doesFileExist
          fileExists <- doesFileExist fileName
          if fileExists
            then do
              contents <- readFile fileName
              printContentLenght contents
            else do putStrLn "The file doesn't exist!"

printContentLenght :: String -> IO ()
printContentLenght contents = putStrLn $ message
    where message      = "The file has " ++ (contentCount contents) ++ " lines!"
          contentCount = show . length . lines
