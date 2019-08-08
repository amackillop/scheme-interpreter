module Types
  ( LispVal(..)
  , LispError(..)
  )
where

import           Text.ParserCombinators.Parsec  ( ParseError )
import qualified Data.Vector                   as V

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (V.Vector LispVal)
             | Number Integer
             | Float Float
             | Character Char
             | String String
             | Bool Bool deriving Eq

instance Show LispVal where
  show (String val) = "\"" ++ val ++ "\""
  show (Character val)   = "#\\" ++ case val of
    ' '  -> "space"
    '\n' -> "newline"
    _    -> [val]
  show (Atom   val ) = val
  show (Number val)  = show val
  show (Bool   val) = if val then "#t" else "#f"
  show (List   contents) = "(" ++ unwordsList contents ++ ")"
  show (DottedList head tail) =
    "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
  show (Vector contents) = "#(" ++ unwordsList (V.toList contents) ++ ")"

data LispError = NumArgs Integer [LispVal]
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
  show (NumArgs expected found) =
      "Expected " ++ show expected ++ " args: found values " ++ unwordsList found
  show (Parser parseErr) = "Parse error at " ++ show parseErr

unwordsList :: Show a => [a] -> String
unwordsList = unwords . map show
