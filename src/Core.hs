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
