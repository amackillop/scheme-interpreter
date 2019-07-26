{-# LANGUAGE LambdaCase #-}

module Main where

import           System.Environment
import           System.Console.Haskeline
import           System.IO                      ( hFlush
                                                , stdout
                                                )
import           Control.Monad                  ( unless
                                                , forever
                                                , void
                                                )

import           Parse
import           Eval

main :: IO ()
main = runInputT defaultSettings $ loop
 where
  loop =  ignoreCtrlC $ getInputLine "scheme > " >>= \case
    Nothing    -> void $ outputStrLn "Moriturus te saluto."
    Just ""    -> loop
    Just input -> outputStrLn (show . eval . readExpr $ input) >> loop
  ignoreCtrlC = handleInterrupt loop . withInterrupt

