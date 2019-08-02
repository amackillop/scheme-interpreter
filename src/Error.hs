module Error where

import           Control.Monad.Except
import           Types

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: Either a b -> b
extractValue (Right val) = val
