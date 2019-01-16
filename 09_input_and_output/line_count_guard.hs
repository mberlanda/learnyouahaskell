-- file: 09_input_and_output/line_count_catch.hs

import System.Environment
import System.IO
import System.IO.Error
import Control.Exception
import Control.Monad

main = do
        (fileName:_) <- getArgs
        r <- tryJust (guard . isDoesNotExistError) $ readFile fileName
        case r of
            Left  e -> putStrLn "The file doesn't exist!"
            Right contents -> putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
