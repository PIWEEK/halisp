import System.Environment (getArgs)

import Parser (readExpr)


main :: IO ()
main = do
        args <- getArgs
        putStrLn (readExpr (head args))
