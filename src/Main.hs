module Main where

import           Parser

import           System.Environment

main :: IO ()
main = do
    options <- getOptions
    print options
