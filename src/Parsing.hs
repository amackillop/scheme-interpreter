{-# LANGUAGE OverloadedStrings #-}

module Parsing
  ( readExpr
  )
where


import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )
import           Data.Text                     as T

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | Character Char
             | String String
             | Bool Bool deriving Show


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:"

spaces :: Parser ()
spaces = skipMany1 space

escaped :: Parser Char
escaped = oneOf "\\" >> oneOf "nrt\\\""

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escaped <|> noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val
