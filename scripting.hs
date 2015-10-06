import System.Process
import System.Environment


main = do args <- getArgs
          r <- readProcess "ls" args ""
          putStr r
