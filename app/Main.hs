module Main where

import Calc
import Parser
import Output
import Implementation
import Control.Monad     (unless)
import System.IO
import Text.Megaparsec
import Control.Monad.Combinators.Expr
import Data.Foldable

promptLine :: String -> IO String
promptLine prompt
 = do putStr prompt
      hFlush stdout
      getLine
{-
parseExpr :: Parser Calc.Expr 
parseExpr = do {
                expr <- pExpr;
                _ <- space;
                return (Expr expr)}
-}

doPrint :: Err Calc -> IO()
doPrint calc = putStrLn (show calc)

-- printLaws :: Laws -> IO()
-- printLaws laws = putStrLn show laws

main :: IO ()
main = do
    --laws <- promptLine "File for laws: "
    -- fileContent <- readFile "src/Laws.txt"
    -- let lawslist = lines fileContent
    --forM_ lawslist parseLaw
    -- forM_ lawslist putStrLn
    -- let lawsl = map parseLaw lawslist
    -- printLaws lawsl
    -- forM_ lawsl printLaws

    --let lawsl = (concat . (map parseLaw)) lawslist
    --forM_ lawsl putStrLn
    --putStrLn(show lawsl)
    expression <- promptLine "Expression to derive: "
    --expression <- Parser.pExpr
    let expr = parseExpr expression
    let result = final law_list expr
    doPrint (final law_list expr)
    