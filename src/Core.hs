module Core where


-- |The different values that can appear in our Lisp programs.
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal -- represents the Scheme form (a b . c)
             | Number Integer
             | String String
             | Bool Bool
             deriving (Show)
