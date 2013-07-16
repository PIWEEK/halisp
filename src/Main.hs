import System.IO
import System.Environment (getArgs)

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error (runErrorT, throwError, ErrorT)

import Data.IORef

import Parser (readExpr)
import Core


type Env = IORef [(String, IORef LispVal)]


-- Environment

nullEnv :: IO Env
nullEnv = newIORef []


isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var


getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable " var)
                             (liftIO . readIORef)
                             (lookup var env)


setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable " var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value


defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do alreadyDefined <- liftIO $ isBound envRef var
                                if alreadyDefined
                                    then setVar envRef var value >> return value
                                    else liftIO $ do
                                        valueRef <- newIORef value
                                        env <- readIORef envRef
                                        writeIORef envRef ((var, valueRef) : env)
                                        return value


-- IO and error handling

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue


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
