{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Eval where

import           Control.Monad.Except
import           Types
import qualified Control.Lens.Combinators      as L

type ThrowsError = Either String

$(L.makePrisms ''LispVal)

eval :: LispVal -> ThrowsError LispVal
eval val@(String    _                  ) = pure val
eval val@(Number    _                  ) = pure val
eval val@(Bool      _                  ) = pure val
eval val@(Character _                  ) = pure val
eval (    List      [Atom "quote", val]) = pure val
eval (    List      (Atom func : args) ) = mapM eval args >>= apply func
eval val@(Vector    _                  ) = pure val
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
  , ("symbol?"       , pure . isSymbol)
  , ("string?"       , pure . isString)
  , ("number?"       , pure . isNumber)
  , ("="             , pure . equals)
  -- , ("<"             , pure . lessThan)
  , ("eq?"           , pure . equals)
  , ("string=?"      , pure . equals)
  , ("string->symbol", str2Sym)
  , ("symbol->string", sym2str)
  ]

equals :: [LispVal] -> LispVal
equals (x : xs) = Bool $ all (== x) xs

-- lessThan :: [LispVal] -> LispVal
-- lessThan [x] = Bool True
-- lessThan (x : xs) = x > 

str2Sym :: [LispVal] -> ThrowsError LispVal
str2Sym [String x ] = pure $ Atom x
str2Sym [notString] = throwError . show $ TypeMismatch "string" notString

sym2str :: [LispVal] -> ThrowsError LispVal
sym2str [Atom x ] = pure $ String x
sym2str [notAtom] = throwError . show $ TypeMismatch "symbol" notAtom

numericOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOp op params = Number . foldl1 op <$> mapM unpackNum params
 where
  unpackNum (Number n) = pure n
  unpackNum notNum     = throwError . show $ TypeMismatch "number" notNum

-- Probably overkill to use a lens here but this is for my own learning.
isTypeOf :: L.Fold s a -> [s] -> LispVal
isTypeOf lispType = Bool <$> L.has (L._head . lispType)

isString :: [LispVal] -> LispVal
isString = isTypeOf _String

isSymbol :: [LispVal] -> LispVal
isSymbol = isTypeOf _Atom

isNumber :: [LispVal] -> LispVal
isNumber = isTypeOf _Number
