import System.Environment (getArgs)

import Text.ParserCombinators.Parsec hiding (spaces)


-- |The different values that can appear in our Lisp programs.
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal -- represents the Scheme form (a b . c)
             | Number Integer
             | String String
             | Bool Bool


-- |The 'parseString' parses a String 'LispVal'.
parseString :: Parser LispVal
parseString = do
                strDelim
                str <- many (noneOf "\"")
                strDelim
                return $ String str

strDelim = char '"'


-- |The 'symbol' function recognizes a symbol allowed in Scheme identifiers.
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- |The 'spaces' functions parses and ignores one or more space characters. The
-- 'spaces' function that comes with Parsec parses zero or more.
spaces :: Parser ()
spaces = skipMany1 space


-- |The 'readExpr' function takes an input text and tries to parse it.
readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"


main :: IO ()
main = do
        args <- getArgs
        putStrLn (readExpr (head args))
