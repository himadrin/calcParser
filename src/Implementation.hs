{-# LANGUAGE FlexibleContexts #-}
module Implementation where

    import Calc
    import System.IO()
    import Prelude hiding (exp)

    --lots of help from book and slides and prof. joosten! (thanks you're the best)
 
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

    -- applies substitutions to expressions
    apply :: Subst -> Expr -> Expr
    apply substitution (Derive v expr) = Derive (apply substitution v) (apply substitution expr)
    apply substitution (TwoOp bop l r) = TwoOp bop (apply substitution l) (apply substitution r)
    apply substitution (OneOp uop expr) = OneOp uop (apply substitution expr)
    apply ((var,expr): substitution) (Var v) | var == (Var v) = expr | otherwise = apply substitution (Var v)
    apply [] (Var v) = (Var v)
    apply _ (Const c) = (Const c)

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