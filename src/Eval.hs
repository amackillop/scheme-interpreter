{-# LANGUAGE FlexibleContexts #-}
module Eval where

import qualified Data.Vector                   as V
                                                ( Vector
                                                , toList
                                                )
import           Control.Monad.Except
import           Types
import           Error

eval :: MonadError LispError m => LispVal -> m LispVal
eval val@(String    _                  ) = return val
eval val@(Number    _                  ) = return val
eval val@(Bool      _                  ) = return val
eval val@(Character _                  ) = return val
eval (    List      [Atom "quote", val]) = return val
eval (    List      (Atom func : args) ) = mapM eval args >>= apply func
eval val@(Vector    _                  ) = return val
eval badForm =
  throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: MonadError LispError m => String -> [LispVal] -> m LispVal
apply func args =
  maybe
      (throwError $ NotFunction "Unrecognized function/unsupported" func)
      ($ args)
    $ lookup func primitives

primitives :: MonadError LispError m => [(String, [LispVal] -> m LispVal)]
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

equals :: MonadError LispError m => [LispVal] -> m LispVal
equals (x : xs) = return $ Bool $ all (== x) xs

str2Sym :: MonadError LispError m => [LispVal] -> m LispVal
str2Sym [String x] = return $ Atom x
str2Sym [notString] = throwError $ TypeMismatch "string" notString

sym2str :: MonadError LispError m => [LispVal] -> m LispVal
sym2str [Atom x] = return $ String x
sym2str [notAtom] = throwError $ TypeMismatch "symbol" notAtom

numericBinOp :: MonadError LispError m => (Integer -> Integer -> Integer) -> [LispVal] -> m LispVal
numericBinOp op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinOp op params        = Number . foldl1 op <$> mapM unpackNum params

isSymbol :: MonadError LispError m => [LispVal] -> m LispVal
isSymbol [Atom _] = return $ Bool True
isSymbol _        = return $ Bool False

isString :: MonadError LispError m => [LispVal] -> m LispVal
isString [String _] = return $ Bool True
isString _          = return $ Bool False

isNumber :: MonadError LispError m => [LispVal] -> m LispVal
isNumber [Number _] = return $ Bool True
isNumber _          = return $ Bool False

unpackNum :: MonadError LispError m => LispVal -> m Integer
unpackNum (Number n) = return n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum
