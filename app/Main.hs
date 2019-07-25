-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

-- import           Data.Text                     as T
import           System.Environment
import           System.Console.Haskeline
import           System.IO                      ( hFlush
                                                , stdout
                                                )
import           Control.Monad                  ( unless
                                                , forever
                                                )

import           Parse
import           Eval

main :: IO ()
main = runInputT defaultSettings $ forever $ getInputLine "scheme > " >>= \case
  Nothing      -> return ()
  Just ":q"    -> return ()
  Just ":quit" -> return ()
  Just input   -> outputStrLn $ show . eval . readExpr $ input
