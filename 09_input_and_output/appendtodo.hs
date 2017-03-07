-- file: 09_input_and_output/appendtodo.hs
import System.IO

main = do
  todoItem <- getLine
  appendFile "todo.txt" (todoItem ++ "\n")