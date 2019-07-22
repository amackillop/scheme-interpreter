-- {-# LANGUAGE OverloadedStrings #-}

module Parsing where


import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )

import qualified Data.Text                     as T
import qualified Data.Char                     as C
import           Numeric

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
escaped = tab <|> newline <|> (string "\\" >> oneOf "r\\\"")

radixNum :: Parser Integer
radixNum = do
  char '#'
  base <- oneOf "bodx"
  digits <- many1 $ parser base
  return $ reader base digits
 where
  parser :: Char -> Parser Char
  parser b = case b of
    'b' -> oneOf "01"
    'o' -> octDigit
    'd' -> digit
    'x' -> hexDigit
  reader :: Char -> String -> Integer
  reader b = case b of
    'b' -> fst . head . readInt 2 (`elem` "01") C.digitToInt
    'o' -> fst . head . readOct
    'd' -> read
    'x' -> fst . head . readHex

-- Add floats, complex numbers etc.

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
parseNumber = (Number . read <$> many1 digit) <|> (Number <$> radixNum)

parseExpr :: Parser LispVal
parseExpr = parseNumber <|> parseAtom <|> parseString

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parse
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val
