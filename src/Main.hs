import System.IO
import System.Environment (getArgs)
import Control.Monad (liftM)

import Parser (readExpr)
import Core (eval, trapError, extractValue)


-- Helper functions

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout


readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine


evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)


evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn


-- |The 'until' function takes a predicate over 'a', a monadic 'a' value, and a
-- function from 'a' to a monadic unit. It runs the monadic action until the
-- result makes the predicate 'True'.
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action =
    do result <- prompt
       if pred result
        then return ()
        else action result >> until_ pred prompt action


runREPL :: IO ()
runREPL = until_ (== "quit") (readPrompt "Halisp :: λ ") evalAndPrint


main :: IO ()
main = do
        args <- getArgs
        case length args of
            0 -> runREPL
            1 -> evalAndPrint $ head args
            otherwise -> print "Incorrect number of arguments, must be 0 or 1"
