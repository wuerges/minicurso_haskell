import System.Environment

main1 = do [a, b, c] <- getArgs
           putStrLn a
           putStrLn b
           putStrLn c

main2 = do args <- getArgs
           mapM_ putStrLn args


main :: IO ()
main = do getArgs >>= mapM_ putStrLn
