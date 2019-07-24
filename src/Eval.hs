module Eval where

import Parse (LispVal (..))

instance Show LispVal where
    show (String contents) = "\"" ++ contents ++ "\""
    show (Atom   name    ) = name
    show (Number contents) = show contents
    show (Bool   True    ) = "#t"
    show (Bool   False   ) = "#f"
    show (List   contents) = "(" ++ unwordsList contents ++ ")"
    show (DottedList head tail) =
      "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
      
unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval _ = String "Eval Error"

apply :: String -> [LispVal] -> LispVal
apply func args =
  maybe (String $ "Operation " ++ func ++ " not yet supported") ($ args)
    $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+"        , numericBinOp (+))
  , ("-"        , numericBinOp (-))
  , ("*"        , numericBinOp (*))
  , ("/"        , numericBinOp div)
  , ("mod"      , numericBinOp mod)
  , ("quotient" , numericBinOp quot)
  , ("remainder", numericBinOp rem)
  , ("symbol?"  , isSymbol)
  , ("string?"  , isString)
  , ("number?"  , isNumber)
  ]

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
