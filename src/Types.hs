module Types
  ( LispVal(..)
  , Number(..)
  , LispError(..)
  ) where

import qualified Data.Vector                   as V
import           GHC.Real                       ( Ratio((:%)) )
import           Text.ParserCombinators.Parsec  ( ParseError )

data LispVal = Atom String
             | Bool Bool
             | Character Char
             | DottedList [LispVal] LispVal
             | List [LispVal]
             | Number Number
             | String String
             | Unit ()
             | Vector (V.Vector LispVal) deriving (Eq, Ord)

instance Show LispVal where
  show (String    val) = show val
  show (Atom      val) = val
  show (Character val) = "#\\" ++ case val of
    ' '  -> "space"
    '\n' -> "newline"
    _    -> [val]
  show (Number val      ) = show val
  show (Bool   val      ) = if val then "#t" else "#f"
  show (List   contents ) = "(" ++ unwordsList contents ++ ")"
  show (DottedList hd tl) = "(" ++ unwordsList hd ++ " . " ++ show tl ++ ")"
  show (Vector contents ) = "#(" ++ unwordsList (V.toList contents) ++ ")"
  show (Unit val) = show val

data Number = Integer Integer
            | Rational Rational
            | Float Float deriving (Eq, Read, Ord)


instance Num Number where
  num1 + num2 = case (num1, num2) of
    (Integer  n , Integer i   ) -> Integer (n + i)
    (Integer  n , Rational ra ) -> Rational (fromInteger n + ra)
    (Integer  n , Float x     ) -> Float (fromInteger n + x)
    (Rational ra, Integer n   ) -> Rational (ra + fromInteger n)
    (Rational ra, Rational ra') -> Rational (ra + ra')
    (Rational ra, Float x     ) -> Float (fromRational ra + x)
    (Float    x , Integer n   ) -> Float (x + fromInteger n)
    (Float    x , Rational ra ) -> Float (x + fromRational ra)
    (Float    x , Float y     ) -> Float (x + y)
  num1 - num2 = case (num1, num2) of
    (Integer  n , Integer i   ) -> Integer (n - i)
    (Integer  n , Rational ra ) -> Rational (fromInteger n - ra)
    (Integer  n , Float x     ) -> Float (fromInteger n - x)
    (Rational ra, Integer n   ) -> Rational (ra - fromInteger n)
    (Rational ra, Rational ra') -> Rational (ra - ra')
    (Rational ra, Float x     ) -> Float (fromRational ra - x)
    (Float    x , Integer n   ) -> Float (x - fromInteger n)
    (Float    x , Rational ra ) -> Float (x - fromRational ra)
    (Float    x , Float y     ) -> Float (x - y)
  num1 * num2 = case (num1, num2) of
    (Integer  n , Integer i   ) -> Integer (n * i)
    (Integer  n , Rational ra ) -> Rational (fromInteger n * ra)
    (Integer  n , Float x     ) -> Float (fromInteger n * x)
    (Rational ra, Integer n   ) -> Rational (ra * fromInteger n)
    (Rational ra, Rational ra') -> Rational (ra * ra')
    (Rational ra, Float x     ) -> Float (fromRational ra * x)
    (Float    x , Integer n   ) -> Float (x * fromInteger n)
    (Float    x , Rational ra ) -> Float (x * fromRational ra)
    (Float    x , Float y     ) -> Float (x * y)
  abs num = case num of
    Integer  a -> Integer (abs a)
    Rational a -> Rational (abs a)
    Float    a -> Float (abs a)
  signum (Integer a) | a > 0     = Integer 1
                     | a < 0     = Integer (-1)
                     | otherwise = Integer 0
  signum (Rational a) | a > 0     = Rational 1
                      | a < 0     = Rational (-1)
                      | otherwise = Rational 0
  signum (Float a) | a > 0     = Float 1
                   | a < 0     = Float (-1)
                   | otherwise = Float 0
  fromInteger = Integer

instance Fractional Number where
  fromRational = Rational
  num1 / num2 = case (num1, num2) of
    (Integer  n , Integer i   ) -> Rational (fromInteger n / fromInteger i)
    (Integer  n , Rational ra ) -> Rational (fromInteger n / ra)
    (Integer  n , Float x     ) -> Float (fromInteger n / x)
    (Rational ra, Integer n   ) -> Rational (ra / fromInteger n)
    (Rational ra, Rational ra') -> Rational (ra / ra')
    (Rational ra, Float x     ) -> Float (fromRational ra / x)
    (Float    x , Integer n   ) -> Float (x / fromInteger n)
    (Float    x , Rational ra ) -> Float (x / fromRational ra)
    (Float    x , Float y     ) -> Float (x / y)

instance Show Number where
  show (Integer val) = show val
  show (Rational (n :% d)) | n == d    = show n
                           | d == 1    = show n
                           | otherwise = show n ++ "/" ++ show d
  show (Float val) = show val

data LispError = NumArgs Integer [LispVal]
               | NumArgsExact Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
  show (UnboundVar     message varname) = message ++ ": " ++ varname
  show (NotFunction    message func   ) = message ++ ": `" ++ func ++ "`"
  show (BadSpecialForm message form   ) = message ++ ": " ++ show form
  show (TypeMismatch expected found) =
    "Invalid type: expected " ++ expected ++ ", found " ++ show found
  show (NumArgs expected found) = let values = if not (null found) then "Values: " ++ unwordsList found else "" in
    "Expected at least " ++ show expected ++ " args: found " ++ show (length found) ++ ". " ++ values
  show (NumArgsExact expected found) = "Expected exactly" ++ show expected ++ " args: found " ++ show (length found) ++ ". Values: " ++ unwordsList found
  show (Parser  parseErr) = "Parse error at " ++ show parseErr
  show (Default message ) = message

unwordsList :: Show a => [a] -> String
unwordsList = unwords . map show
