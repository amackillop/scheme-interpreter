{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Eval where

import qualified Data.Vector                   as V
                                                ( Vector
                                                , toList
                                                )
import           Control.Monad.Except
import           Types
import           Error
import qualified Control.Lens.Combinators as L

type ThrowsError = Either String

$(L.makePrisms ''LispVal)

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
  , ("symbol?"       , return . isSymbol)
  , ("string?"       , return . isString)
  , ("number?"       , return . isNumber)
  , ("="             , return . equals)
  , ("eq?"           , return . equals)
  , ("string=?"      , return . equals)
  , ("string->symbol", str2Sym)
  , ("symbol->string", sym2str)
  ]

equals :: [LispVal] -> LispVal
equals (x : xs) = Bool $ all (== x) xs

str2Sym :: [LispVal] -> ThrowsError LispVal
str2Sym [String x ] = return $ Atom x
str2Sym [notString] = throwError . show $ TypeMismatch "string" notString

sym2str :: [LispVal] -> ThrowsError LispVal
sym2str [Atom x ] = return $ String x
sym2str [notAtom] = throwError . show $ TypeMismatch "symbol" notAtom

numericOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOp op params = Number . foldl1 op <$> mapM unpackNum params
 where
  unpackNum (Number n) = return n
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
