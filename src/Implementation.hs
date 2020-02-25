module Implementation where

    -- import Parser as P
    import Text.Megaparsec
    import Text.Megaparsec.Char
    import Data.Void
    import Control.Monad.Combinators.Expr
    import qualified Text.Megaparsec.Char.Lexer as L
    
    data Law = Law String Equation 
        deriving Show
    type Equation = (Expr,Expr)
    type Parser = Parsec Void String 

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
          | Const Int
          deriving Show

    pExprL :: Parser Expr
    pExprL = makeExprParser pTermL operatorTable

    pTermL :: Parser Expr
    pTermL = (try $ do  {_ <- space;
                _ <- string "derive";
                _ <- string "(";
                var <- letterChar;
                _ <- space;
                _ <- string ",";
                _ <- space;
                expr <- pExprL;
                _ <- string ")";
                _ <- space;
                return (Derive (Var var) expr)}) <|>
            (try $ do  {_ <- space;
                _ <- string "(";
                _ <- space;
                expr <- pExprL;
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
                -- should be parsed by Expr
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
                        expr <- pExprL;
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

        -- TODO: PARSE LAWS!
    pLaw :: Implementation.Parser Law
    pLaw = do {
        name <- upto ':' ; 
        e1 <- pExprL;
        _ <- space;
        _ <- string "=";
        _ <- space; 
        e2 <- pExprL; 
        return (Law name (e1,e2))}

    upto :: Token [Char] -> Implementation.Parser String
    upto c = (char c *> return []) <|> ((:) <$> anySingle <*> upto c) 

        -- TODO: HAVE LAWS RECOGNIZE EXPRESSIONS!
