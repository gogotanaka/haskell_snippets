import System.Environment (getArgs)

main :: IO ()
main = mapM_ putStrLn =<< getArgs
