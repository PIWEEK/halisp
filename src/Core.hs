module Core where


-- |The different values that can appear in our Lisp programs.
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal -- represents the Scheme form (a b . c)
             | Number Integer
             | String String
             | Bool Bool


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



-- |The 'eval' function reduces an arbitrarily complex 'LispVal' to a basic
-- 'LispVal'.
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func: args)) = apply func $ map eval args
eval x = x


-- |The 'apply' function takes a string representing a function, a list of
-- arguments and returns the value resulting from applying the function to the
-- arguments.
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (String "[ERROR]") ($ args) $ lookup func primitives


primitives :: [(String, [LispVal] -> LispVal)]
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
numericBinaryOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinaryOp f args = Number $ foldl1 f $ map unpackNumber args


-- TODO: more generic
-- |The 'unpackNumber' takes a 'LispVal' and returns a numeric representation
-- of it.
unpackNumber :: LispVal -> Integer
unpackNumber (Number n) = n
unpackNumber _ = 0


singleArg :: (LispVal -> LispVal) -> [LispVal] -> LispVal
singleArg f (x:_) = f x


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
