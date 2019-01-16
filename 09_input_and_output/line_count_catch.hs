-- file: 09_input_and_output/line_count_catch.hs

import System.Environment
import System.IO
import System.IO.Error
import Control.Exception

main = toTry `catch` handler

toTry :: IO()
toTry = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"
    | otherwise = ioError e
