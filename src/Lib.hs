module Lib
    ( someFunc
    ) where


import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

someFunc :: IO ()
someFunc = getLine >>= print . sum . map read . words
            
-- someFunc = do
    -- args <- getArgs
    -- let ints = map read args :: [Int]
    -- print $ sum ints