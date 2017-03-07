-- file: 09_input_and_output/todo.hs
import System.Environment(getArgs, getProgName)
import System.IO(
                  openFile,
                  appendFile,
                  IOMode(..),
                  openTempFile,
                  hGetContents,
                  hPutStr,
                  hClose
                )
import System.Directory(removeFile, renameFile)
import Data.List(delete)

dispatch:: [(String,[String] -> IO())]
dispatch =  [ ("add", add)
            , ("remove", remove)
            , ("view", view)
            ]

add :: [String] -> IO ()  
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")  

remove :: [String] -> IO ()  
remove [fileName, numberString] = do  
    handle <- openFile fileName ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let number = read numberString  
        todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! number) todoTasks  
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile fileName  
    renameFile tempName fileName  

view :: [String] -> IO ()  
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args