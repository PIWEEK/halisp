module Parser where

import Control.Monad (liftM)
import Control.Monad.Error (throwError)

import Text.ParserCombinators.Parsec hiding (spaces)

import Core


-- |The 'parseString' parses a String 'LispVal'.
parseString :: Parser LispVal
parseString = do
                strDelim
                str <- many $ noneOf escapeCharOptions <|> escapedChar
                strDelim
                return $ String str


strDelim = char '"'


-- |A parser that is able to process escaped characters for a string, trimming
-- the backslash used to escape it.
escapedChar = do
                char '\\'
                x <- oneOf escapeCharOptions
                return $ case x of
                    '\\' -> x
                    '"'  -> x
                    'n' -> '\n'
                    't' -> '\t'
                    'r' -> '\r'


-- |A string with the various characters used as escape codes.
escapeCharOptions = "\\\"ntr"


-- |The 'parseAtom' function parses a Atom 'LispVal'. The '#t' and '#f' atoms
-- are interpreted as the true and false boolean values. An atom can't start
-- with a number, but it may contain them.
parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol -- an atom can't start with a number
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _ -> Atom atom


-- |The 'parseNumber' function parses a Number 'LispVal'.
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


-- |The 'parseExpr' function parses either a string, atom or number.
parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> parseParens


parseParens :: Parser LispVal
parseParens = do
                char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x


-- |The 'parseList' function parses a list of expressions.
parseList :: Parser LispVal
parseList = parseExpr `sepBy` spaces >>= return . List


-- |The 'parseList' function parses a dotted list of expressions.
parseDottedList :: Parser LispVal
parseDottedList = do
                    first <- parseExpr `endBy1` spaces
                    tail <- char '.' >> spaces >> parseExpr
                    return $ DottedList first tail


-- |The 'parseQuoted' function parses a quoted expression.
parseQuoted :: Parser LispVal
parseQuoted = do
                char '\''
                x <- parseExpr
                return $ List [Atom "quote", x]


-- |The 'symbol' function recognizes a symbol allowed in Scheme identifiers.
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


-- |The 'spaces' functions parses and ignores one or more space characters. The
-- 'spaces' function that comes with Parsec parses zero or more.
spaces :: Parser ()
spaces = skipMany1 space


-- |The 'readExpr' function takes an input text and returns a 'LispVal'
-- representation of the input. If it encounters an error, it will return a
-- string 'LispVal' with an error message.
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val
