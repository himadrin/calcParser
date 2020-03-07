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

promptLine :: String -> IO String
promptLine prompt
 =do
    putStr prompt
    hFlush stdout
    getLine
{-
parseExpr :: Parser Calc.Expr 
parseExpr = do {
                expr <- pExpr;
                _ <- space;
                return (Expr expr)}
-}

doPrint :: Err [Step] -> IO()
doPrint calc = putStrLn (show calc)

printLaws :: Laws -> IO()
printLaws laws = putStrLn (show laws)

parseExpr :: String -> Err Expr
parseExpr s = case parse (pExpr <* eof) "<stdin>" s of 
  Left err -> Error "something went wrong"
  Right ex -> Expr ex


main :: IO ()
main = do
    --laws <- promptLine "File for laws: "
    fileContent <- readFile "src/Laws.txt"
    let lawslist = lines fileContent
    --forM_ lawslist parseLaw
    -- forM_ lawslist putStrLn
    -- let lawsl = map parseLaw lawslist
   
    -- forM_ lawsl printLaws

    let lawsl = map parseLaw lawslist
    -- printLaws lawsl
    --forM_ lawsl (putStrLn show)
    --putStrLn(show lawsl)
    expression <- promptLine "Expression to derive: "
    --expression <- Parser.pExpr
    let expr = parseExpr expression
    --putStrLn("hi" ++ (show expr))
    let result = final lawsl expr
    doPrint(result)
    