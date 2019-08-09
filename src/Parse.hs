{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Parse
  ( readExpr
  )
where


import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )
import           Control.Monad.Except


import qualified Data.Char                     as C
import qualified Data.Vector                   as V
                                                ( Vector
                                                , fromList
                                                )
import qualified Numeric                       as N
import           Types
import           Error

type ThrowsError = Either String

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

escaped :: Parser String
escaped = char2Str <$> (char '\\' >> oneOf "\\\"ntr")
 where
  char2Str = \case
    '\\' -> "\\"
    '"'  -> "\""
    't'  -> "\t"
    'n'  -> "\n"
    'r'  -> "\r"

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
    'b' -> N.readInt 2 (`elem` "01") C.digitToInt
    'o' -> N.readOct
    'd' -> N.readDec
    'x' -> N.readHex

-- Add floats, complex numbers etc.

parseString :: Parser LispVal
parseString =
  String . concat
    <$> (char '"' *> many (many1 (noneOf "\"\\") <|> escaped) <* char '"')

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseChar :: Parser LispVal
parseChar = Character <$> (string "#\\" >> (parseCharName <|> anyChar))

parseCharName :: Parser Char
parseCharName = str2Char <$> (string "space" <|> string "newline")
 where
  str2Char = \case
    "space"   -> ' '
    "newline" -> '\n'

parseNumber :: Parser LispVal
parseNumber = (Number . read <$> many1 digit) <|> (Number <$> radixNum)

parseExpr :: Parser LispVal
parseExpr =
  try parseNumber
    <|> try parseString
    <|> try parseQuoted
    <|> try parseList
    <|> try parseDottedList
    <|> try parseBackQuotes
    <|> try parseVector
    <|> try parseChar
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
parseVector =
  Vector . V.fromList <$> (char '#' >> inParens (sepBy parseExpr spaces))

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> throwError . show $ Parser err
  Right val -> return val
