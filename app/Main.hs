module Main where

import Lib
import SimpleParser
import System.Environment

main :: IO ()
main = do
    (expr:_) <- getArgs
    print . eval . readExpr $ expr
