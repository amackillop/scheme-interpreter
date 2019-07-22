-- {-# LANGUAGE OverloadedStrings #-}

module Parsing where


import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )

import qualified Data.Text                     as T
import qualified Data.Char                     as C
import qualified Data.Vector                   as V
import           Numeric

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (V.Vector LispVal)
             | Number Integer
             | Float Float
             | Character Char
             | String String
             | Bool Bool

instance Show LispVal where show = showVal

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:"

spaces :: Parser ()
spaces = skipMany1 space

escaped :: Parser Char
escaped = tab <|> newline <|> (string "\\" >> oneOf "r\\\"")

radixNum :: Parser Integer
radixNum = do
  char '#'
  base   <- oneOf "bodx"
  digits <- many1 $ parser base
  return $ fst $ head $ reader base digits
 where
  parser :: Char -> Parser Char
  parser b = case b of
    'b' -> oneOf "01"
    'o' -> octDigit
    'd' -> digit
    'x' -> hexDigit
  reader :: Char -> String -> [(Integer, String)]
  reader b = case b of
    'b' -> readInt 2 (`elem` "01") C.digitToInt
    'o' -> readOct
    'd' -> readDec
    'x' -> readHex

-- Add floats, complex numbers etc.

parseString :: Parser LispVal
parseString =
  String <$> (char '"' *> many (escaped <|> noneOf ['"']) <* char '"')

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
parseExpr =
  try parseNumber
    <|> parseString
    <|> parseQuoted
    <|> do
          char '('
          x <-
            try parseList
            <|> try parseDottedList
            <|> try parseBackQuotes
            <|> try parseVector
          char ')'
          return x
    <|> parseAtom

inParens :: Parser a -> Parser a
inParens parser = char '(' *> parser <* char ')'

sepBySpaces :: Parser Char -> Parser String
sepBySpaces parser = sepBy parser spaces

parseList :: Parser LispVal
parseList = List <$> inParens (sepBy parseExpr spaces)

parseDottedList :: Parser LispVal
parseDottedList = inParens $ do
  head <- endBy parseExpr spaces
  tail <- char '.' *> spaces *> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted =
  char '\'' >> parseExpr >>= (\x -> return $ List [Atom "quote", x])

parseBackQuotes :: Parser LispVal
parseBackQuotes =
  char '`' >> parseExpr >>= (\x -> return $ List [Atom "backquote", x])

parseVector :: Parser LispVal
parseVector = Vector . V.fromList <$> inParens (sepBy parseExpr spaces)

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom   name    ) = name
showVal (Number contents) = show contents
showVal (Bool   True    ) = "#t"
showVal (Bool   False   ) = "#f"
showVal (List   contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) =
  "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
