{-# LANGUAGE FlexibleContexts #-}
module Implementation where

    -- import Parser as P
    import Calc
    import Laws
    import Text.Megaparsec
    import Text.Megaparsec.Char
    import Data.Void
    import Control.Monad.Combinators.Expr
    import qualified Text.Megaparsec.Char.Lexer as L
    import System.IO()
    import Prelude hiding (exp)

    --test expressions! need more
    test1 :: Expr
    test1 = (Derive (Var 'x') (TwoOp Power (Var 'x') (Const 3)))

    test10 :: Expr
    test10 = Derive (Var 'x') (TwoOp Add (TwoOp Add (TwoOp Mult (Const 3) (Var 'x')) (TwoOp Power (Var 'x') (Const 2))) (TwoOp Mult (Const 2) (Var 'x')))

    test11 :: Expr
    test11 = Derive (Var 'x') (TwoOp Power (Var 'x') (TwoOp Add (Const 2) (Const 3)))

    e4 :: Expr
    e4 = Derive (Var 'x') (TwoOp Power (Var 'q') (Var 'p'))

    e5 :: Expr
    e5 = TwoOp Mult (Var 'p') (TwoOp Power (Var 'q') (TwoOp Subt (Var 'p')(Const 1)))

    test8 :: Expr
    test8 = (Derive (Var 'x') (TwoOp Power (Var 'x') (Var 'y')))

    test2 :: Expr
    test2 = (Derive (Var 'x') (TwoOp Mult (Var 'a') (Var 'b')))

    test3 :: Expr
    test3 = (Var 'x')

    test4 :: Expr
    test4 = (Var 'y')

    test5 :: Subst
    test5 = [(Var 'x',Var 'x'),(Var 'a',Const 2),(Var 'b',OneOp Ln (Var 'x'))]

    test6 :: Expr
    test6 = (Derive (Var 'x') (TwoOp Mult (Var 'x')(Var 'y')))

    test7 :: Expr
    test7 = (Derive (Var 'x') (TwoOp Div (Var 'x')(Var 'x')))

-- **COULD NOT GET READFILE TO WORK**
    --getlaws :: [String]
    -- getlaws = do {
        -- lawstext <- liftM lines (readFile "src/laws/Laws.txt");
        --let list = lines lawstext; 
        -- return lawstext}

        -- TODO: PARSE LAWS! (rip)
 
    --matches expressions to come up with a list of substitutions
    matchFunc :: Expr -> Expr -> [Subst]
    matchFunc (Derive varL exprL) (Derive varE exprE) = [vars ++ exprs | vars <- matchFunc varL varE, exprs <- matchFunc exprL exprE, compatible vars exprs]
    matchFunc (TwoOp op1 expLeftL expRightL) (TwoOp op2 expLeftE expRightE) | op1 == op2 = if matchFunc expLeftL expLeftE == [] || matchFunc expRightL expRightE == [] then []
        else [left ++ right | left <- matchFunc expLeftL expLeftE, right <- matchFunc expRightL expRightE,  compatible left right] | otherwise = []
    matchFunc (OneOp op1 exprL) (OneOp op2 exprE) | op1 == op2 = matchFunc exprL exprE | otherwise = []
    matchFunc (Var v) expr@(Const _) = [[(Var v, expr)]]
    matchFunc (Var v) expr | v /= 'p' && v /= 'q' = [[(Var v, expr)]] -- TODO: document in readme that p and q are special
    --write for constants
    matchFunc (Var 'p') (Const c) = [[((Var 'p'), (Const c))]]
    matchFunc (Var 'q') (Const c) = [[((Var 'p'), (Const c))]]
    matchFunc (Const i) (Const j) | i == j = [[]]
    matchFunc _ _ = []

    --TO DO: WRITE THIS FUNC! 
    compatible :: Subst -> Subst -> Bool
    compatible sub1 sub2 = and [e1 == e2 | (v1, e1) <- sub1, (v2, e2) <-sub2, v1==v2]


    sub = [((Var 'x'),(Const 3)), ((Var 'y'),(Const 4))]
    exp = (TwoOp Add (Var 'x')(Var 'y'))

    -- applies substitutions to expressions
    apply :: Subst -> Expr -> Expr
    apply substitution (Derive v expr) = Derive (apply substitution v) (apply substitution expr)
    apply substitution (TwoOp bop l r) = TwoOp bop (apply substitution l) (apply substitution r)
    apply substitution (OneOp uop expr) = OneOp uop (apply substitution expr)
    apply ((var,expr): substitution) (Var v) | var == (Var v) = expr | otherwise = apply substitution (Var v)
    apply [] (Var v) = (Var v)
    apply _ (Const c) = (Const c)
    --recombines expression

    e1 = Derive (Var 'x') (TwoOp Add (Var 'a') (Var 'b'))
    e2 = TwoOp Add (Derive (Var 'x') (Var 'a')) (Derive (Var 'x') (Var 'b'))
    e3 = Derive (Var 'x') (TwoOp Add (Var 'x') (Const 3))

    rewrites :: Expr -> Expr -> Expr -> [Expr]
    rewrites e1 e2 expression
        = helperRW e1 e2 expression ++
            case expression of
                (Derive v expr) -> [Derive v new_exp | new_exp <- rewrites e1 e2 expr]
                (TwoOp bop l r) -> [TwoOp bop new_expL r | new_expL <- rewrites e1 e2 l] 
                                        ++ [TwoOp bop l new_expR | new_expR <- rewrites e1 e2 r]
                (OneOp uop expr) -> [OneOp uop new_exp | new_exp <- rewrites e1 e2 expr]
                _ -> []


    helperRW :: Expr -> Expr -> Expr -> [Expr]
    helperRW e1 e2 exp = [apply substitution e2 | substitution <- matchFunc e1 exp]

    -- extra feature : simplify math
    doMath :: Expr -> Expr
    doMath (Derive v expr) = Derive v (doMath expr)
    doMath (TwoOp bop (Const c1) (Const c2)) = Const((helper bop) c1 c2)
    doMath (TwoOp bop l r) = TwoOp bop (doMath l) (doMath r)
    doMath expr = expr

    -- gives you the mathematic operator 
    helper :: BOp -> (Int -> Int -> Int)
    helper bop | bop == Power = (^) | bop == Add = (+) | bop == Subt = (-) | bop == Mult = (*) | bop == Div = div
