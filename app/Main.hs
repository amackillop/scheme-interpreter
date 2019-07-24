-- {-# LANGUAGE OverloadedStrings #-}

module Main where

-- import           Data.Text                     as T
import           System.Environment
import           System.IO                      ( hFlush
                                                , stdout
                                                )
import           Control.Monad                  ( unless
                                                , forever
                                                )

import           Parse
import           Eval

main :: IO ()
main = forever $ input >>= print . eval . readExpr
  where input = putStr "scheme > " >> hFlush stdout >> getLine
