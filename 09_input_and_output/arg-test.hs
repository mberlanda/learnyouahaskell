-- file: 09_input_and_output/arg-test.hs
import System.Environment(getArgs, getProgName)

main = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The arguments are:"
  mapM_ putStrLn args
  putStrLn "The program name is:"
  putStrLn progName
