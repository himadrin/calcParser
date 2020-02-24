module Implementation where

    import Parser
    import Text.Megaparsec
    import Text.Megaparsec.Char
    import Data.Void
    
    data Law = Law String Equation 
    type Equation = (Expr,Expr)

    -- TODO: PARSE LAWS!
    pLaw :: Parser Law
    pLaw = do {
            name <- upto ':' ; 
            e1 <- pExpr; 
            e2 <- pExpr; 
            return (Law name (e1,e2))}

    upto :: Token [Char] -> Parser String
    upto c = (char c *> return []) <|> ((:) <$> anySingle <*> upto c) 

    -- TODO: HAVE LAWS RECOGNIZE EXPRESSIONS!