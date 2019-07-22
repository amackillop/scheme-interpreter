-- {-# LANGUAGE OverloadedStrings #-}

module Main where
    
-- import           Data.Text                     as T
import           System.Environment
import           Parsing

main :: IO ()
main = getArgs >>= putStr . readExpr . head
