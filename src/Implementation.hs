{-# LANGUAGE FlexibleContexts #-}
module Implementation where

    -- import Parser as P
    import Calc
    import Text.Megaparsec
    import Text.Megaparsec.Char
    import Data.Void
    import Control.Monad.Combinators.Expr
    import qualified Text.Megaparsec.Char.Lexer as L
    import System.IO()
    import Prelude hiding (exp)

    --test expressions! need more
    test1 :: Expr
    test1 = (Derive (Var 'x') (TwoOp Power (Var 'x') (Const '3')))

    e4 :: Expr
    e4 = Derive (Var 'x') (TwoOp Power (Var 'q') (Const 'p'))

    e5 :: Expr
    e5 = TwoOp Mult (Const 'p') (TwoOp Power (Var 'q') (TwoOp Subt (Const 'p')(Const '1')))

    test8 :: Expr
    test8 = (Derive (Var 'x') (TwoOp Power (Var 'x') (Var 'y')))

    test2 :: Expr
    test2 = (Derive (Var 'x') (TwoOp Mult (Var 'a') (Var 'b')))

    test3 :: Expr
    test3 = (Var 'x')

    test4 :: Expr
    test4 = (Var 'y')

    test5 :: Subst
    test5 = [(Var 'x',Var 'x'),(Var 'a',Const '2'),(Var 'b',OneOp Ln (Var 'x'))]

    test6 :: Expr
    test6 = (Derive (Var 'x') (TwoOp Mult (Var 'x')(Var 'y')))

    test7 :: Expr
    test7 = (Derive (Var 'x') (TwoOp Div (Var 'x')(Var 'x')))

    --parsing functions
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
                d <- some alphaNumChar;
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
-- **COULD NOT GET READFILE TO WORK**
    --getlaws :: [String]
    -- getlaws = do {
        -- lawstext <- liftM lines (readFile "src/laws/Laws.txt");
        --let list = lines lawstext; 
        -- return lawstext}

        -- TODO: PARSE LAWS! (rip)
    
    pLaw :: Parser Law
    pLaw = do {
            name <- upto ':' ; 
            e1 <- pExprL;
            _ <- space;
            _ <- string "=";
            _ <- space; 
            e2 <- pExprL; 
            return (Law name (e1,e2))}

    upto :: Token [Char] -> Parser String
    upto c = (char c *> return []) <|> ((:) <$> anySingle <*> upto c)

    -- laws essentially from parser results
    law_addition = Law "law addition" (Derive (Var 'x') (TwoOp Add (Var 'a') (Var 'b')),TwoOp Add (Derive (Var 'x') (Var 'a')) (Derive (Var 'x') (Var 'b')))
    law_product = Law "law_product" (Derive (Var 'x') (TwoOp Mult (Var 'a') (Var 'b')), TwoOp Add (TwoOp Mult (Derive (Var 'x')(Var 'a'))(Var 'b'))(TwoOp Mult (Var 'a')(Derive (Var 'x')(Var 'b'))))
    law_sin = Law "law_sin" (Derive (Var 'x') (OneOp Sin (Var 'a')),TwoOp Mult (OneOp Cos (Var 'a')) (Derive (Var 'x') (Var 'a')))
    law_cos = Law "law_cos" (Derive (Var 'x') (OneOp Cos (Var 'a')),TwoOp Mult (OneOp Neg (OneOp Sin (Var 'a'))) (Derive (Var 'x') (Var 'a')))
    law_ln = Law "law_ln" (Derive (Var 'x') (OneOp Ln (Var 'a')),TwoOp Mult (TwoOp Div (Const '1') (Var 'a')) (Derive (Var 'x') (Var 'a')))
    law_power = Law "law_power" (Derive (Var 'x') (TwoOp Power (Var 'a') (Var 'b')),TwoOp Mult (TwoOp Power (Var 'a') (Var 'b')) (Derive (Var 'x') (TwoOp Mult (Var 'b') (OneOp Ln (Var 'a')))))
    law_self = Law "law_self" (Derive (Var 'x') (Var 'x'),Const '1')
    law_power_const = Law "law_power_const" (Derive (Var 'x') (TwoOp Power (Var 'q') (Const 'p')), TwoOp Mult (Const 'p') (TwoOp Power (Var 'q') (TwoOp Subt (Const 'p')(Const '1'))))
    law_list = [law_addition, law_power_const, law_cos, law_power, law_product, law_sin, law_ln, law_self]

    --matches expressions to come up with a list of substitutions
    matchFunc :: Expr -> Expr -> [Subst]
    matchFunc (Derive varL exprL) (Derive varE exprE) = [vars ++ exprs | vars <- matchFunc varL varE, exprs <- matchFunc exprL exprE, compatible vars exprs]
    matchFunc (TwoOp op1 expLeftL expRightL) (TwoOp op2 expLeftE expRightE) | op1 == op2 = if matchFunc expLeftL expLeftE == [] || matchFunc expRightL expRightE == [] then []
        else [left ++ right | left <- matchFunc expLeftL expLeftE, right <- matchFunc expRightL expRightE,  compatible left right] | otherwise = []
    matchFunc (OneOp op1 exprL) (OneOp op2 exprE) | op1 == op2 = matchFunc exprL exprE | otherwise = []
    matchFunc (Var v) expr@(Const _) = [[(Var v, expr)]]
    matchFunc (Var v) expr | v /= 'p' && v /= 'q' = [[(Var v, expr)]] -- TODO: document in readme that p and q are special
    --write for constants
    matchFunc (Const i) (Const j) | i == j = [[]]
    matchFunc _ _ = []

    --TO DO: WRITE THIS FUNC! 
    compatible :: Subst -> Subst -> Bool
    compatible sub1 sub2 = and [e1 == e2 | (v1, e1) <- sub1, (v2, e2) <-sub2, v1==v2]


    sub = [((Var 'x'),(Const '3')), ((Var 'y'),(Const '4'))]
    exp = (TwoOp Add (Var 'x')(Var 'y'))

    -- applies substitutions to expressions
    apply :: Subst -> Expr -> Expr
    apply substitution (Derive var exp) = Derive (apply substitution var) (apply substitution exp)
    apply substitution (TwoOp op left right) = TwoOp op (apply substitution left) (apply substitution right)
    apply substitution (OneOp op exp) = OneOp op (apply substitution exp)
    apply ((var,exp): substitution) (Var v) | var == (Var v) = exp | otherwise = apply substitution (Var v)
    apply [] (Var v) = (Var v)
    apply _ (Const c) = (Const c)
    --recombines expression

    e1 = Derive (Var 'x') (TwoOp Add (Var 'a') (Var 'b'))
    e2 = TwoOp Add (Derive (Var 'x') (Var 'a')) (Derive (Var 'x') (Var 'b'))
    e3 = Derive (Var 'x') (TwoOp Add (Var 'x') (Const '3'))

    rewrites :: Expr -> Expr -> Expr -> [Expr]
    rewrites e1 e2 expression
        = helperRW e1 e2 expression ++
            case expression of
                (Derive var exp) -> [Derive var new_exp | new_exp <- rewrites e1 e2 exp]
                (TwoOp op left right) -> [TwoOp op new_expL right | new_expL <- rewrites e1 e2 left] 
                                        ++ [TwoOp op left new_expR | new_expR <- rewrites e1 e2 right]
                (OneOp op exp) -> [OneOp op new_exp | new_exp <- rewrites e1 e2 exp]
                _ -> []


    helperRW :: Expr -> Expr -> Expr -> [Expr]
    helperRW e1 e2 exp = [apply substitution e2 | substitution <- matchFunc e1 exp]
