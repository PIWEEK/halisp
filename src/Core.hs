{-# LANGUAGE ExistentialQuantification #-}

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


data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

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
eval (List [Atom "if", pred, conseq, alt]) =
    do result <- eval pred
       case result of
         Bool False -> eval alt
         Bool True -> eval conseq
         otherwise -> throwError $ TypeMismatch "Boolean" result

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
primitives = [ -- Comparisons
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal),
               -- Numeric
              ("+", numericBinaryOp (+)),
              ("-", numericBinaryOp (-)),
              ("*", numericBinaryOp (*)),
              ("/", numericBinaryOp div),
              ("mod", numericBinaryOp mod),
              ("quotient", numericBinaryOp quot),
              ("remainder", numericBinaryOp rem),
              ("number?", singleArg isNumber),
              ("=", numericBooleanBinaryOp (==)),
              ("<", numericBooleanBinaryOp (<)),
              (">", numericBooleanBinaryOp (>)),
              ("/=", numericBooleanBinaryOp (/=)),
              (">=", numericBooleanBinaryOp (>=)),
              ("<=", numericBooleanBinaryOp (<=)),
               -- Boolean
              ("not", singleArg negateLispVal),
              ("boolean?", singleArg isBoolean),
              ("&&", booleanBinaryOp (&&)),
              ("||", booleanBinaryOp (||)),
               -- Symbols
              ("symbol?", singleArg isSymbol),
               -- Strings
              ("string?", singleArg isString),
              ("string=?", stringBinaryOp (==)),
              ("string<?", stringBinaryOp (<)),
              ("string>?", stringBinaryOp (>)),
              ("string<=?", stringBinaryOp (<=)),
              ("string>=?", stringBinaryOp (>=)),
               -- Lists
              ("list?", singleArg isList),
              ("cons", cons),
              ("car", car),
              ("cdr", cdr)]


-- TODO: more generic, works with any number!
-- |The 'numericBinaryOp' takes a binary numeric function and applies it to a
-- list of 'LispVal' instances returning a number 'LispVal'.
numericBinaryOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinaryOp f singleVal@[_]= throwError $ NumArgs "two or more" singleVal
numericBinaryOp f args = mapM unpackNumber args >>= return . Number . foldl1 f


-- |The 'unpackEquals' functions takes a couple of 'LispVal's, a 'Unpacker'
-- function and compares the unpacked values.
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals x y (AnyUnpacker unpacker) =
    do unpackedX <- unpacker x
       unpackedY <- unpacker y
       return $ unpackedX == unpackedY
    `catchError` (const $ return False)


-- |The 'eqv' function takes two arbitrary 'LispVal's and returns a Boolean
-- 'LispVal' with the result of their comparison. Two values are 'eqv' if they
-- print the same.
eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool x), (Bool y)] = return $ Bool $ x == y
eqv [(String x), (String y)] = return $ Bool $ x == y
eqv [(Number x), (Number y)] = return $ Bool $ x == y
eqv [(Atom x), (Atom y)] = return $ Bool $ x == y
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]

eqv [(List xs), (List ys)] = return $ Bool $ (length xs == length ys) && (all eqvPair $ zip xs ys)
    where eqvPair (x, y) = case eqv [x, y] of
                            Left err -> False
                            Right (Bool v) -> v

eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs "two" badArgList


-- |The 'equal' function takes two arbitrary 'LispVal's and returns a Boolean
-- 'LispVal' with the result of their comparison. Two values are 'equal' if
-- they have the same type and value.
equal :: [LispVal] -> ThrowsError LispVal
equal [x, y] = do
                let unpackers = [AnyUnpacker unpackNumber, AnyUnpacker unpackBool, AnyUnpacker unpackString]
                -- TODO: doesn't interpret two equivalent values of different
                -- types as true. E.g.: (equals? 1 "1")
                primitiveEquals <- liftM or $ mapM (unpackEquals x y) unpackers
                eqvEquals <- eqv [x, y]
                return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs "two" badArgList


-- TODO: more generic
-- |The 'unpackNumber' takes a 'LispVal' and returns a numeric representation
-- of it.
unpackNumber :: LispVal -> ThrowsError Integer
unpackNumber (Number n) = return n
unpackNumber notNum = throwError $ TypeMismatch "Number" notNum


unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool v) = return v
unpackBool notBool = throwError $ TypeMismatch "Boolean" notBool


unpackString :: LispVal -> ThrowsError String
unpackString (String v) = return v
unpackString notString = throwError $ TypeMismatch "String" notString


boolBinOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinOp unpacker f args = if length args /= 2
                            then throwError $ NumArgs "two" args
                            else do left <- unpacker $ args !! 0
                                    right <- unpacker $ args !! 1
                                    return $ Bool $ f left right


numericBooleanBinaryOp = boolBinOp unpackNumber
booleanBinaryOp = boolBinOp unpackBool
stringBinaryOp = boolBinOp unpackString


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


-- Lists
isList :: LispVal -> LispVal
isList (List _) = Bool True
isList (DottedList _ _) = Bool True
isList _ = Bool False


car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs "one" badArgList


cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [_] xs] = return xs
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs "one" badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List $ x:xs
cons [x, DottedList xs xstail] = return $ DottedList (x:xs) xstail
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs "two" badArgList
