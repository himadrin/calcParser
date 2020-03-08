module Parser (pExpr, parseLaw) where
--module Parser (pExpr, parseExpr) where

-- Data Structures help from textbook and from inclass slides
-- parsing help from https://jakewheat.github.io/intro_to_parsing/#getting-started
import Calc
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr -- DOCUMENTATION: http://docs.restyled.io/restyler/parser-combinators-1.1.0/Control-Monad-Combinators-Expr.html
import qualified Text.Megaparsec.Char.Lexer as L

-- Parsing
-- lots of help from this: https://markkarpov.com/tutorial/megaparsec.html
-- makes the parser using our opTable and pTerm

upto :: Token [Char] -> Parser String
upto c = (char c *> return []) <|> ((:) <$> anySingle <*> upto c)

parseLaw :: String -> Law
parseLaw s = case parse (pLaw <* eof) "<stdin>" s of 
  Left _ -> Law "error law" (Const 0, Const 0)
  Right law -> law

pLaw :: Parser Law
pLaw = do {
        name <- upto ':' ; 
        e1 <- pExpr;
        _ <- space;
        _ <- string "=";
        _ <- space; 
        e2 <- pExpr; 
        return (Law name (e1,e2))}

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
                d <- some digitChar;
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

prefix :: String -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix  (f <$ symbol name)

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