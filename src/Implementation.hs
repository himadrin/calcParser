{-# LANGUAGE FlexibleContexts #-}
module Implementation where

    import Calc
    import System.IO()
    import Prelude hiding (exp)

    --lots of help from book and slides and prof. joosten! (thanks you're the best)

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
 
    --matches expressions to come up with a list of substitutions
    matchFunc :: Expr -> Expr -> [Subst]
    matchFunc (Derive varL exprL) (Derive varE exprE) = [vars ++ exprs | vars <- matchFunc varL varE, exprs <- matchFunc exprL exprE, compatible vars exprs]
    matchFunc (TwoOp op1 expLeftL expRightL) (TwoOp op2 expLeftE expRightE) | op1 == op2 = if matchFunc expLeftL expLeftE == [] || matchFunc expRightL expRightE == [] then []
        else [left ++ right | left <- matchFunc expLeftL expLeftE, right <- matchFunc expRightL expRightE,  compatible left right] | otherwise = []
    matchFunc (OneOp op1 exprL) (OneOp op2 exprE) | op1 == op2 = matchFunc exprL exprE | otherwise = []
    matchFunc (Var v) expr@(Const _) = [[(Var v, expr)]]
    matchFunc (Var v) expr | v /= 'p' && v /= 'q' = [[(Var v, expr)]] -- TODO: document in readme that p and q are special
    --write for constants p and q are constants in our laws
    matchFunc (Const i) (Const j) | i == j = [[]]
    matchFunc _ _ = []

    -- checks if two substritutions are compqtible and returns true or false
    compatible :: Subst -> Subst -> Bool
    compatible sub1 sub2 = and [e1 == e2 | (v1, e1) <- sub1, (v2, e2) <-sub2, v1==v2]

    --test exprs
    sub :: Subst
    sub = [((Var 'x'),(Const 3)), ((Var 'y'),(Const 4))]

    exp :: Expr
    exp = (TwoOp Add (Var 'x')(Var 'y'))

    -- applies substitutions to expressions
    apply :: Subst -> Expr -> Expr
    apply substitution (Derive v expr) = Derive (apply substitution v) (apply substitution expr)
    apply substitution (TwoOp bop l r) = TwoOp bop (apply substitution l) (apply substitution r)
    apply substitution (OneOp uop expr) = OneOp uop (apply substitution expr)
    apply ((var,expr): substitution) (Var v) | var == (Var v) = expr | otherwise = apply substitution (Var v)
    apply [] (Var v) = (Var v)
    apply _ (Const c) = (Const c)

    --test exprs
    expr1 :: Expr
    expr1 = Derive (Var 'x') (TwoOp Add (Var 'a') (Var 'b'))

    expr2:: Expr
    expr2 = TwoOp Add (Derive (Var 'x') (Var 'a')) (Derive (Var 'x') (Var 'b'))

    expr3:: Expr
    expr3 = Derive (Var 'x') (TwoOp Add (Var 'x') (Const 3))

    --recombines expression
    rewrites :: Expr -> Expr -> Expr -> [Expr]
    rewrites exp1 exp2 expression
        = helperRW exp1 exp2 expression ++
            case expression of
                (Derive v expr) -> [Derive v new_exp | new_exp <- rewrites exp1 exp2 expr]
                (TwoOp bop l r) -> [TwoOp bop new_expL r | new_expL <- rewrites exp1 exp2 l] 
                                        ++ [TwoOp bop l new_expR | new_expR <- rewrites exp1 exp2 r]
                (OneOp uop expr) -> [OneOp uop new_exp | new_exp <- rewrites exp1 exp2 expr]
                _ -> []

    --helper func for rewriting
    helperRW :: Expr -> Expr -> Expr -> [Expr]
    helperRW e1 e2 expr = [apply substitution e2 | substitution <- matchFunc e1 expr]

    -- extra feature : simplify math if constants are being operated on
    doMath :: Expr -> Expr
    doMath (Derive v expr) = Derive v (doMath expr)
    doMath (TwoOp bop (Const c1) (Const c2)) = Const((helper bop) c1 c2)
    doMath (TwoOp bop l r) = TwoOp bop (doMath l) (doMath r)
    doMath expr = expr

    -- gives you the mathematic operator 
    helper :: BOp -> (Int -> Int -> Int)
    helper bop | bop == Power = (^) | bop == Add = (+) | bop == Subt = (-) | bop == Mult = (*) | bop == Div = div | otherwise = (+)
