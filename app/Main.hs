module Main where

import Calc
import Parser
import Output
import Implementation
import Control.Monad     (unless)
import System.IO

read_ :: IO String
read_ = putStr "DERIVE> "
     >> hFlush stdout
     >> getLine

-- update to apply parser to expr, then take in file name for laws
eval_ :: String -> String
eval_ input = input

print_ :: String -> IO ()
print_ = putStrLn


main :: IO ()
main = do
    input <- read_
  
    unless (input == ":quit")
        $ print_ (eval_ input) >> main