module Parser where

-- Data Structures help from textbook and from inclass slides
-- parsing help from https://jakewheat.github.io/intro_to_parsing/#getting-started

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad.Combinators.Expr -- DOCUMENTATION: http://docs.restyled.io/restyler/parser-combinators-1.1.0/Control-Monad-Combinators-Expr.html
import qualified Text.Megaparsec.Char.Lexer as L

data BOp = Add 
          | Mult
          | Div 
          | Subt
          | Power
          deriving Show
data UOp = Sin 
          | Cos
          | Tan
          | Ln 
          | Neg 
          | Pos 
          deriving Show
data Expr = TwoOp BOp Expr Expr
            | OneOp UOp Expr
            | Derive Expr Expr
            | Var Char
            | Const Char
            deriving Show

type Parser = Parsec Void String 

-- Parsing
-- lots of help from this: https://markkarpov.com/tutorial/megaparsec.html
-- makes the parser using our opTable and pTerm
pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

pTerm :: Parser Expr
pTerm = (try $ do  {_ <- space;
                _ <- string "derive";
                _ <- string "(";
                var <- letterChar;
                _ <- space;
                _ <- string ",";
                _ <- space;
                expr <- pExpr;
                _ <- string ")";
                _ <- space;
                return (Derive (Var var) expr)}) <|>
            (try $ do  {_ <- space;
                _ <- string "(";
                _ <- space;
                expr <- pExpr;
                _ <- space;
                _ <- string ")";
                _ <- space;
                return (expr)}) <|>
            (try $ do  {_ <- space;
                one <- parseOneOp;
                _ <- space;
                return (one)}) <|>
            (try $ do  {_ <- space;
                d <- some alphaNumChar;
                _ <- space;
                return (Const (read d))}) <|>
            do  {_ <- space;
                v <- letterChar;
                _ <- space;
                return (Var v)}

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "sin" (OneOp Sin)
    , prefix "cos" (OneOp Cos)
    , prefix "tan" (OneOp Tan)
    , prefix "ln" (OneOp Ln)
    , prefix "-" (OneOp Neg)
    , prefix "+" (OneOp Pos)
    ]
  ,
    [ binary "*" (TwoOp Mult)
    , binary "/" (TwoOp Div)
    ]
  , [ binary "+" (TwoOp Add)
    , binary "-" (TwoOp Subt)
    ]
  ,
    [ binary "^" (TwoOp Power)]
  ]

-- code help from markkarpov.com tutorial 
lineComment :: Parser ()
lineComment = L.skipLineComment "#"

symbol :: String -> Parser String
symbol = L.symbol (L.space space1 lineComment empty)

binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL  (f <$ symbol name)

prefix, postfix :: String -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

--helper funcs to do parsing
parseOneOp :: Parser Expr --finds a one operator and then continues looking for expr
parseOneOp = do {_ <- space;
                un <- parseUOp;
                _ <- space;
                expr <- pExpr;
                return (OneOp un expr)}

parseUOp :: Parser UOp --simple string parse for operations on one expr
parseUOp =  do  {_ <- string "sin";
                return Sin} <|>
            do  {_ <- string "cos";
                return Cos} <|>
            do  {_ <- string "tan";
                return Tan} <|>
            do  {_ <- string "ln";
                return Ln} <|>
            do  {_ <- string "-";
                return Neg} <|>
            do  {_ <- string "+";
                return Pos}

parseBOp :: Parser BOp --simple string parse for operations on two exprs
parseBOp =  do  {_ <- string "*";
                return Mult} <|>
            do {_ <- string "/";
                return Div} <|>
            do  {_ <- string "+";
                return Add} <|>
            do  {_ <- string "-";
                return Subt} <|>
            do  {_ <- string "^";
                return Power}
