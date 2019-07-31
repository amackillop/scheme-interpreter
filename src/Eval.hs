module Eval where

import qualified Data.Vector                   as V
                                                ( Vector
                                                , toList
                                                )
import           LispVal                        ( LispVal(..) )

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

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Character _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval val@(Vector _) = val
eval _ = String "Eval Error"

apply :: String -> [LispVal] -> LispVal
apply func args =
  maybe (String $ "Operation " ++ func ++ " not yet supported") ($ args)
    $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+"             , numericBinOp (+))
  , ("-"             , numericBinOp (-))
  , ("*"             , numericBinOp (*))
  , ("/"             , numericBinOp div)
  , ("mod"           , numericBinOp mod)
  , ("quotient"      , numericBinOp quot)
  , ("remainder"     , numericBinOp rem)
  , ("symbol?"       , isSymbol)
  , ("string?"       , isString)
  , ("number?"       , isNumber)
  , ("="             , equals)
  , ("eq?"           , equals)
  , ("string=?"      , equals)
  , ("string->symbol", str2Sym)
  , ("symbol->string", sym2str)
  ]

equals :: [LispVal] -> LispVal
equals (x : xs) = Bool $ all (== x) xs

str2Sym :: [LispVal] -> LispVal
str2Sym [String x] = Atom x

sym2str :: [LispVal] -> LispVal
sym2str [Atom x] = String x

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp op params = Number $ foldl1 op $ map unpackNum params

isSymbol :: [LispVal] -> LispVal
isSymbol [Atom _] = Bool True
isSymbol _        = Bool False

isString :: [LispVal] -> LispVal
isString [String _] = Bool True
isString _          = Bool False

isNumber :: [LispVal] -> LispVal
isNumber [Number _] = Bool True
isNumber _          = Bool False

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _          = 0
