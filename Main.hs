import System.Environment (getArgs)

import Text.ParserCombinators.Parsec hiding (spaces)

-- |The 'symbol' function recognizes a symbol allowed in Scheme identifiers.
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- |The 'readExpr' function takes an input text and tries to parse it.
readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"


main :: IO ()
main = do
        args <- getArgs
        putStrLn (readExpr (head args))
