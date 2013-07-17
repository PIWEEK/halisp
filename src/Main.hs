import System.IO
import System.Environment (getArgs)

import Control.Monad (liftM)

import Core

-- Helper functions

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout


readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine


evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env


evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn


-- |The 'until_' function takes a predicate over 'a', a monadic 'a' value, and
-- a function from 'a' to a monadic unit. It runs the monadic action until the
-- result makes the predicate 'True'.
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action =
    do result <- prompt
       if pred result
        then return ()
        else action result >> until_ pred prompt action


runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String $ head args])) >>= hPutStrLn stderr


runREPL :: IO ()
runREPL = primitiveBindings >>= until_ (== "quit") (readPrompt "Halisp :: λ ") . evalAndPrint


main :: IO ()
main = do
        args <- getArgs
        if null args then runREPL else runOne $ args
