{-# LANGUAGE FlexibleContexts #-}


module Parse
  ( readExpr,
  )
where

import Control.Monad.Except
import qualified Data.Char as C
import Data.Functor (($>))
import Data.Text (pack, replace, unpack)
import Data.Vector (fromList)
import Error (ThrowsError)
import qualified Numeric as N
import Text.ParserCombinators.Parsec
import Types

-- emptyList :: Parser String
-- emptyList = try string "()" <|>

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

escaped :: Parser String
escaped = char '\\' <:> many (oneOf "\\\"ntr")

parseString :: Parser LispVal
parseString =
  String . concat <$> unquote (many (many1 (noneOf "\"\'\\") <|> escaped))

unquote :: Parser a -> Parser a
unquote = between (char '"') (char '"')

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseChar :: Parser LispVal
parseChar = Character <$> (string "#\\" >> (parseCharName <|> anyChar))

parseCharName :: Parser Char
parseCharName = string "space" $> ' ' <|> string "newline" $> '\n'

parseNumber :: Parser LispVal
parseNumber =
  Number
    <$> choice [try parseFloat, try parseRational, parseInteger, parseRadix]

parseInteger :: Parser Number
parseInteger = Integer . read <$> signedInteger

parseRational :: Parser Number
parseRational = Rational . read . replaceSlash <$> rational
  where
    replaceSlash = strReplace "/" " % "

strReplace :: String -> String -> String -> String
strReplace old new = unpack . replace (pack old) (pack new) . pack

parseFloat :: Parser Number
parseFloat = Float . read <$> float

integer :: Parser String
integer = many1 digit

plus :: Parser String
plus = char '+' *> integer

minus :: Parser String
minus = char '-' <:> integer

signedInteger :: Parser String
signedInteger = plus <|> minus <|> integer

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) a b = (++) <$> a <*> b

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b

float :: Parser String
float = signedInteger <++> (char '.' <:> integer)

rational :: Parser String
rational = signedInteger <++> (char '/' <:> integer)

parseRadix :: Parser Number
parseRadix = do
  base <- char '#' >> oneOf "bodx"
  digits <- many1 $ parser base
  return . fst . head $ reader base digits
  where
    parser :: Char -> Parser Char
    parser b = case b of
      'b' -> oneOf "01"
      'o' -> octDigit
      'd' -> digit
      'x' -> hexDigit
    reader :: Char -> String -> [(Number, String)]
    reader b = case b of
      'b' -> N.readInt 2 (`elem` "01") C.digitToInt
      'o' -> N.readOct
      'd' -> N.readDec
      'x' -> N.readHex

parseExpr :: Parser LispVal
parseExpr =
  choice
    [ try parseNumber,
      try parseString,
      try parseQuoted,
      try parseList,
      try parseDottedList,
      -- try parseSingletonList,
      try parseBackQuotes,
      try parseVector,
      try parseChar,
      parseAtom
    ]

inParens :: Parser a -> Parser a
inParens parser = char '(' *> parser <* char ')'

sepBySpaces :: Parser a -> Parser [a]
sepBySpaces parser = sepBy parser spaces

parseList :: Parser LispVal
parseList = List <$> inParens (sepBySpaces parseExpr)

parseDottedList :: Parser LispVal
parseDottedList = inParens $ do
  hd <- endBy parseExpr spaces
  tl <- char '.' *> spaces *> parseExpr
  return $ DottedList hd tl

parseQuoted :: Parser LispVal
parseQuoted = char '\'' >> parseExpr >>= (\x -> pure $ List [Atom "quote", x])

parseBackQuotes :: Parser LispVal
parseBackQuotes =
  char '`' >> parseExpr >>= (\x -> pure $ List [Atom "backquote", x])

parseVector :: Parser LispVal
parseVector =
  Vector . fromList <$> (char '#' >> inParens (sepBySpaces parseExpr))

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> pure val
