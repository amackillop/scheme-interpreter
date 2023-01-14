{-# LANGUAGE LambdaCase #-}

-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE Rank2Types #-}
-- {-# LANGUAGE NoImplicitPrelude #-}

module Eval where

import Control.Monad.Except
import Error (ThrowsError)
import Types

-- import qualified Data.List.NonEmpty as L

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = pure val
eval val@(Number _) = pure val
eval val@(Bool _) = pure val
eval val@(Character _) = pure val
eval val@(Vector _) = pure val
eval (List []) = pure $ Unit ()
eval (List [Atom _]) = throwError $ NumArgs 1 []
eval (List [Atom "quote", val]) = pure val
eval (List [Atom "backquote", val]) = pure val
eval (List [Atom "if", predicate, conseq, alt]) =
  eval predicate >>= \case
    Bool False -> eval alt
    _ -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply _ [] = throwError $ NumArgs 0 []
apply func args =
  maybe
    (throwError $ NotFunction "Unrecognized function/unsupported" func)
    ($ args)
    $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericOp (+)),
    ("-", numericOp (-)),
    ("*", numericOp (*)),
    ("/", numericOp (/)),
    ("mod", integerOp mod),
    ("quotient", integerOp quot),
    ("remainder", integerOp rem),
    ("symbol?", return . isSymbol),
    ("string?", return . isString),
    ("number?", return . isNumber),
    ("=", numericBoolOp (==)),
    ("<", numericBoolOp (<)),
    (">", numericBoolOp (>)),
    ("/=", numericBoolOp (/=)),
    (">=", numericBoolOp (>=)),
    ("<=", numericBoolOp (<=)),
    ("and", boolBoolBinOp (&&)),
    ("or", boolBoolBinOp (||)),
    ("string=?", strBoolBinOp (==)),
    ("string<?", strBoolBinOp (==)),
    ("string>?", strBoolBinOp (>)),
    ("string<=?", strBoolBinOp (<=)),
    ("string>=?", strBoolBinOp (>=)),
    ("eq?", pure . eq),
    ("eqv?", eqv),
    ("string->symbol", str2Sym),
    ("symbol->string", sym2str)
  ]

lispOp ::
  (LispVal -> ThrowsError a) -> (a -> a -> a) -> [LispVal] -> ThrowsError a
lispOp _ _ [] = throwError $ NumArgs 1 []
lispOp unpacker op args = foldr1 op <$> mapM unpacker args

boolOp ::
  (LispVal -> ThrowsError a) ->
  (a -> a -> Bool) ->
  [LispVal] ->
  ThrowsError LispVal
boolOp _ _ [] = return $ Bool True
boolOp _ _ [_] = return $ Bool True
boolOp unpacker op (arg : rest) = do
  first <- unpacker arg
  second <- unpacker $ head rest
  if first `op` second then boolOp unpacker op rest else return $ Bool False

binOp ::
  (LispVal -> ThrowsError a) -> (a -> a -> a) -> [LispVal] -> ThrowsError a
binOp unpacker op args =
  if length args /= 2
    then throwError $ NumArgsExact 2 args
    else lispOp unpacker op args

boolBinOp ::
  (LispVal -> ThrowsError a) ->
  (a -> a -> Bool) ->
  [LispVal] ->
  ThrowsError LispVal
boolBinOp unpacker op args =
  if length args /= 2
    then throwError $ NumArgsExact 2 args
    else boolOp unpacker op args

numericOp :: (Number -> Number -> Number) -> [LispVal] -> ThrowsError LispVal
numericOp op args = Number <$> lispOp unpackNum op args

integerOp ::
  (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
integerOp op args = Number . Integer <$> lispOp unpackInt op args

numericBoolOp :: (Number -> Number -> Bool) -> [LispVal] -> ThrowsError LispVal
numericBoolOp = boolOp unpackNum

boolBoolBinOp :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinOp = boolBinOp unpackBool

strBoolBinOp :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinOp = boolBinOp unpackStr

eq :: [LispVal] -> ThrowsError LispVal
eq (arg : rest) = return . Bool $ all (== arg) rest

eqv :: [LispVal] -> ThrowsError LispVal
eqv [arg1, arg2] = return . Bool $ arg1 == arg2
eqv badArgList = throwError $ NumArgsExact 2 badArgList

-- equal :: [LispVal] -> ThrowsError LispVal


str2Sym :: [LispVal] -> ThrowsError LispVal
str2Sym [String x] = pure $ Atom x
str2Sym [notString] = throwError $ TypeMismatch "string" notString
str2Sym _ = throwError $ Default "Should not happen."

sym2str :: [LispVal] -> ThrowsError LispVal
sym2str [Atom x] = pure $ String x
sym2str [notAtom] = throwError $ TypeMismatch "symbol" notAtom
sym2str _ = throwError $ Default "Should not happen."

unpackNum :: LispVal -> ThrowsError Number
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackInt :: LispVal -> ThrowsError Integer
unpackInt (Number (Integer n)) = pure n
unpackInt (Number notInt) = throwError $ TypeMismatch "integer" (Number notInt)
unpackInt notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
  unpacked1 <- unpacker arg1
  unpacked2 <- unpacker arg2
  return $ unpacked1 == unpacked2 `catchError` const (return False)

isString :: [LispVal] -> LispVal
isString [String _] = Bool True
isString _ = Bool False

isSymbol :: [LispVal] -> LispVal
isSymbol [Atom _] = Bool True
isSymbol _ = Bool False

isNumber :: [LispVal] -> LispVal
isNumber val = Bool $ isInteger val || isFloat val

isInteger :: [LispVal] -> Bool
isInteger [Number (Integer _)] = True
isInteger _ = False

isFloat :: [LispVal] -> Bool
isFloat [Number (Float _)] = True
isFloat _ = False

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return . List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgsExact 2 badArgList