module Error where

import           Control.Monad.Except
import           Text.Parsec.Error              ( ParseError )
import           LispVal
import           Eval                           ( unwordsList )

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String


instance Show LispError where
    show (UnboundVar     message varname) = message ++ ": " ++ varname
    show (NotFunction    message func   ) = message ++ ": " ++ show func
    show (BadSpecialForm message form   ) = message ++ ": " ++ show form
    show (TypeMismatch expected found) =
        "Invalid type: expected " ++ expected ++ ", found " ++ show found
    show (NumArgs expected found) =
        "Expected " ++ show expected ++ " args: found values " ++ unwordsList found
    show (Parser parseErr) = "Parse error at " ++ show parseErr

type ThrowsError = Either String

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: Either a b -> b
extractValue (Right val) = val
