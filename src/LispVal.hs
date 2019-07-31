module LispVal
  ( LispVal (..)
  )
where

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
