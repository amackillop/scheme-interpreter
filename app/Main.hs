{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           System.Console.Haskeline
import           Control.Monad                  (void
                                                )
import Control.Monad.Except
import           Parse
import Error
import           Eval
import Types

instance MonadError LispError IO where
  

main :: IO ()
main = runInputT defaultSettings loop
 where
  loop = ignoreCtrlC $ getInputLine "scheme > " >>= \case
    Nothing    -> void $ outputStrLn "Moriturus te saluto."
    Just ""    -> loop
    Just input -> outputStrLn (readExpr input >>= eval >>= show) >> loop
  ignoreCtrlC = handleInterrupt loop . withInterrupt

