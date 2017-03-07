-- file: 09_input_and_output/deletetodo.hs
import System.IO(
                  openFile,
                  IOMode(..),
                  openTempFile,
                  hGetContents,
                  hPutStr,
                  hClose
                )
import System.Directory(removeFile, renameFile)
import Data.List(delete)

main = do
  handle <- openFile "todo.txt" ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle

  let todoTasks = lines contents
      numberedTasks = zipWith(\n line -> show n ++ " - " ++ line) [0..] todoTasks
  putStrLn "These are your TO-DO items:"
  putStr $ unlines numberedTasks
  putStrLn "Which one do you want to delete?"
  numberString <- getLine
  let number = read numberString
      newTodoItems = delete (todoTasks !! number) todoTasks

  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile "todo.txt"
  renameFile tempName "todo.txt"
