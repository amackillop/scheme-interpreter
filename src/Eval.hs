{-# LANGUAGE FlexibleContexts #-}
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
  [ ("+"             , numericOp (+))
  , ("-"             , numericOp (-))
  , ("*"             , numericOp (*))
  , ("/"             , numericOp div)
  , ("mod"           , numericOp mod)
  , ("quotient"      , numericOp quot)
  , ("remainder"     , numericOp rem)
  , ("symbol?"       , isSymbol)
  , ("string?"       , isString)
  , ("number?"       , isNumber)
  , ("="             , equals)
  , ("eq?"           , equals)
  , ("string=?"      , equals)
  , ("string->symbol", str2Sym)
  , ("symbol->string", sym2str)
  ]

equals :: [LispVal] -> ThrowsError LispVal
equals (x : xs) = return $ Bool $ all (== x) xs

str2Sym :: [LispVal] -> ThrowsError LispVal
str2Sym [String x] = return $ Atom x
str2Sym [notString] = throwError . show $ TypeMismatch "string" notString

sym2str :: [LispVal] -> ThrowsError LispVal
sym2str [Atom x] = return $ String x
sym2str [notAtom] = throwError . show $ TypeMismatch "symbol" notAtom

numericOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOp op params        = Number . foldl1 op <$> mapM unpackNum params

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [Atom _] = return $ Bool True
isSymbol _        = return $ Bool False

isString :: [LispVal] -> ThrowsError LispVal
isString [String _] = return $ Bool True
isString _          = return $ Bool False

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [Number _] = return $ Bool True
isNumber _          = return $ Bool False

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum     = throwError . show $ TypeMismatch "number" notNum
