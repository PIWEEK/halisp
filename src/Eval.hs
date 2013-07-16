import System.Environment (getArgs)

import Control.Monad (liftM)

import Core (eval, trapError, extractValue)
import Parser (readExpr)


main :: IO ()
main = do
        args <- getArgs
        evaled <- return $ liftM show $ readExpr (head args) >>= eval
        putStrLn $ extractValue $ trapError evaled
