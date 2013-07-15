import System.Environment (getArgs)

import Parser (runExpr)


main :: IO ()
main = do
        args <- getArgs
        putStrLn (runExpr (head args))
