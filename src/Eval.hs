module Eval where

import qualified Data.Vector                   as V
                                                ( Vector
                                                , toList
                                                )
import           Control.Monad.Except
import           Types
import           Error

type ThrowsError = Either String

eval :: LispVal -> ThrowsError LispVal
eval val@(String    _                  ) = return val
eval val@(Number    _                  ) = return val
eval val@(Bool      _                  ) = return val
eval val@(Character _                  ) = return val
eval (    List      [Atom "quote", val]) = return val
eval (    List      (Atom func : args) ) = mapM eval args >>= apply func
eval val@(Vector    _                  ) = return val
eval badForm =
  throwError . show $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe
      (throwError . show $ NotFunction "Unrecognized function/unsupported" func)
      ($ args)
    $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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
