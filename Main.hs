import System.Environment (getArgs)

import Control.Monad (liftM)

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

-- |The 'parseAtom' parses a Atom 'LispVal'. The '#t' and '#f' atoms are
-- interpreted as the true and false boolean values. An atom can't start with a
-- number, but it may contain them.
parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol -- an atom can't start with a number
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _ -> Atom atom

-- |The 'parseNumber' parses a Number 'LispVal'.
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- Exercise 1
parseNumber' :: Parser LispVal
parseNumber' = do
                 ds <- many1 digit
                 return $ Number (read ds)

parseNumber'' :: Parser LispVal
parseNumber'' = many1 digit >>= return . toNumber
    where toNumber  = Number . read


-- |The 'parseExpr' parses either a string, atom or number.
parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber''
        <?> "Atom, string or number"

-- |The 'symbol' function recognizes a symbol allowed in Scheme identifiers.
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- |The 'spaces' functions parses and ignores one or more space characters. The
-- 'spaces' function that comes with Parsec parses zero or more.
spaces :: Parser ()
spaces = skipMany1 space


-- |The 'readExpr' function takes an input text and tries to parse it.
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"


main :: IO ()
main = do
        args <- getArgs
        putStrLn (readExpr (head args))
