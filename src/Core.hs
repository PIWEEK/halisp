module Core where

import Text.ParserCombinators.Parsec.Error (ParseError)

import Control.Monad.Error


-- |The different values that can appear in our Lisp programs.
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal -- represents the Scheme form (a b . c)
             | Number Integer
             | String String
             | Bool Bool


-- |The different errors that can occur in our Lisp programs.
data LispError = NumArgs String [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String


instance Show LispVal where
    show (Atom name) = name
    show (List xs) = "(" ++ showLispValList xs ++ ")"
    show (DottedList head tail) = "(" ++ showLispValList head ++ " . " ++ show tail ++ ")"
    show (Number n) = show n
    show (String s) = "\"" ++ s ++ "\""
    show (Bool True) = "#t"
    show (Bool False) = "#f"


showLispValList :: [LispVal] -> String
showLispValList = unwords . map show


instance Show LispError where
    show (NumArgs expected found) = "Expected " ++ show expected ++ " arguments; found " ++ show (length found)
    show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
    show (Parser parseErr) = "Parse error at " ++ show parseErr
    show (BadSpecialForm msg form) = msg ++ ": " ++ show form
    show (NotFunction msg func) = msg ++ ": " ++ func
    show (UnboundVar msg varname) = msg ++ ": " ++ varname


instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default


-- |A type for including the possibility of a 'LispError' in a computation.
type ThrowsError = Either LispError

-- |A function for capturing errors and always returning a 'Right' value as a
-- string.
trapError action = catchError action (return . show)


-- |A function for extracting the 'Right' value from a 'ThrowsError'
-- computation.
extractValue :: ThrowsError a -> a
extractValue (Right val) = val


-- |The 'eval' function reduces an arbitrarily complex 'LispVal' to a basic
-- 'LispVal'.
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func: args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


-- |The 'apply' function takes a string representing a function, a list of
-- arguments and returns the value resulting from applying the function to the
-- arguments.
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function" func)
                        ($ args)
                        (lookup func primitives)


primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ -- Numeric
              ("+", numericBinaryOp (+)),
              ("-", numericBinaryOp (-)),
              ("*", numericBinaryOp (*)),
              ("/", numericBinaryOp div),
              ("mod", numericBinaryOp mod),
              ("quotient", numericBinaryOp quot),
              ("remainder", numericBinaryOp rem),
              ("number?", singleArg isNumber),
               -- Boolean
              ("not", singleArg negateLispVal),
              ("boolean?", singleArg isBoolean),
               -- Symbols
              ("symbol?", singleArg isSymbol),
               -- Strings
              ("string?", singleArg isString),
               -- Lists
              ("list?", singleArg isList)]


-- TODO: more generic, works with any number!
-- |The 'numericBinaryOp' takes a binary numeric function and applies it to a
-- list of 'LispVal' instances returning a number 'LispVal'.
numericBinaryOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinaryOp f singleVal@[_]= throwError $ NumArgs "two or more" singleVal
numericBinaryOp f args = mapM unpackNumber args >>= return . Number . foldl1 f


-- TODO: more generic
-- |The 'unpackNumber' takes a 'LispVal' and returns a numeric representation
-- of it.
unpackNumber :: LispVal -> ThrowsError Integer
unpackNumber (Number n) = return n
unpackNumber notNum = throwError $ TypeMismatch "Number" notNum


singleArg :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
singleArg f [x] = return (f x)
singleArg _ args = throwError $ NumArgs "one" args


negateLispVal :: LispVal -> LispVal
negateLispVal (Bool v) = Bool (not v)


isBoolean :: LispVal -> LispVal
isBoolean (Bool _) = Bool True
isBoolean _ = Bool False


-- FIXME: Strings aren't symbols!
isSymbol :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _ = Bool False


isString :: LispVal -> LispVal
isString (String _) = Bool True
isString _ = Bool False


-- TODO: more generic
isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _ = Bool False


isList :: LispVal -> LispVal
isList (List _) = Bool True
isList (DottedList _ _) = Bool True
isList _ = Bool False
