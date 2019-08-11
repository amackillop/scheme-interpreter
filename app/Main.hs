{-# LANGUAGE LambdaCase #-}

module Main where

import           System.Console.Haskeline
import           Control.Monad                  ( void )
import           Control.Monad.Except
import           Parse
import           Error
import           Eval
import           Types

main :: IO ()
main = runInputT defaultSettings loop
 where
  loop = ignoreCtrlC $ getInputLine "scheme > " >>= \case
    Nothing    -> void $ outputStrLn "Moriturus te saluto."
    Just ""    -> loop
    Just input -> outputStrLn (readEvalPrint input) >> loop
  ignoreCtrlC = handleInterrupt loop . withInterrupt

readEvalPrint :: String -> String
readEvalPrint input =
  extractValue $ trapError (show <$> (readExpr input >>= eval))
