module Main where

import Calc
import Parser
import Laws
import Output
import Implementation
import Control.Monad     (unless)
import System.IO
import Text.Megaparsec
import Control.Monad.Combinators.Expr
import Data.Foldable

--https://blogg.bekk.no/creating-a-repl-in-haskell-efcdef1deec2
--https://wiki.haskell.org/Tutorials/Programming_Haskell/String_IO

-- prompts user and takes in their response input
promptLine :: String -> IO String
promptLine prompt
 =do
    putStr prompt
    hFlush stdout
    getLine

-- prints steplist correctly
doPrint :: Err [Step] -> IO()
doPrint calc = putStrLn (show calc)

printLaws :: Laws -> IO()
printLaws laws = putStrLn (show laws)

-- helper to handle parse result with Err and Expr
parseExpr :: String -> Err Expr
parseExpr s = case parse (pExpr <* eof) "<stdin>" s of 
  Left err -> Error "You entered your expression incorrectly. Check the readme for proper syntax!"
  Right ex -> Expr ex


main :: IO ()
main = do
    -- get string content from our law file
    fileContent <- readFile "src/Laws.txt"
    -- separate content by each line into list of laws
    let lawslist = lines fileContent
    let lawsl = map parseLaw lawslist
    -- uncomment below to view parsed laws
    -- printLaws lawsl
    expression <- promptLine "Expression to derive: "
    let expr = parseExpr expression
    -- call final from Output to get steps
    let result = final lawsl expr
    doPrint(result)
    