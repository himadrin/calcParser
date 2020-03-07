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

-- **COULD NOT GET READFILE TO WORK**
    --getlaws :: [String]
    -- getlaws = do {
        -- lawstext <- liftM lines (readFile "src/laws/Laws.txt");
        --let list = lines lawstext; 
        -- return lawstext}

        -- TODO: PARSE LAWS! (rip)
    -- laws essentially from parser results
    law_addition = Law "law addition" (Derive (Var 'x') (TwoOp Add (Var 'a') (Var 'b')),TwoOp Add (Derive (Var 'x') (Var 'a')) (Derive (Var 'x') (Var 'b')))
    law_product = Law "law_product" (Derive (Var 'x') (TwoOp Mult (Var 'a') (Var 'b')), TwoOp Add (TwoOp Mult (Derive (Var 'x')(Var 'a'))(Var 'b'))(TwoOp Mult (Var 'a')(Derive (Var 'x')(Var 'b'))))
    law_sin = Law "law_sin" (Derive (Var 'x') (OneOp Sin (Var 'a')),TwoOp Mult (OneOp Cos (Var 'a')) (Derive (Var 'x') (Var 'a')))
    law_cos = Law "law_cos" (Derive (Var 'x') (OneOp Cos (Var 'a')),TwoOp Mult (OneOp Neg (OneOp Sin (Var 'a'))) (Derive (Var 'x') (Var 'a')))
    law_ln = Law "law_ln" (Derive (Var 'x') (OneOp Ln (Var 'a')),TwoOp Mult (TwoOp Div (Const '1') (Var 'a')) (Derive (Var 'x') (Var 'a')))
    law_power = Law "law_power" (Derive (Var 'x') (TwoOp Power (Var 'a') (Var 'b')),TwoOp Mult (TwoOp Power (Var 'a') (Var 'b')) (Derive (Var 'x') (TwoOp Mult (Var 'b') (OneOp Ln (Var 'a')))))
    law_self = Law "law_self" (Derive (Var 'x') (Var 'x'),Const '1')
    --law_power_const = Law "law_power_const" (Derive (Var 'x') (TwoOp Power (Var 'a') (Var 'p')), TwoOp Mult (Var 'p') (TwoOp Power (Var 'a') (TwoOp Subt (Var 'p')(Const '1'))))
    law_deriv_const = Law "law: derivative of a const" (Derive (Var 'x') (Var 'p'), Const '0')
    law_deriv_const_neg = Law "law: derivative of a const" (Derive (Var 'x') (OneOp Neg (Var 'p')), Const '0')
    law_exponent1 = Law "convert fraction to exponent" (TwoOp Div (Var 'a') (Var 'b'), TwoOp Mult (Var 'a')(TwoOp Power (Var 'b') (OneOp Neg (Const '1'))))
    law_exponent_add = Law "adding exponents" (TwoOp Add (TwoOp Power (Var 'a') (Var 'b')) (TwoOp Power (Var 'a') (Var 'c')), TwoOp Power (Var 'a') (TwoOp Add (Var 'b') (Var 'c')))
    law_mult_zero = Law "multiply by zero" (TwoOp Mult (Var 'a') (Const '0'), Const '0')
    law_mult_zero2 = Law "multiply by zero" (TwoOp Mult (Const '0') (Var 'a'), Const '0')
    law_mult_one = Law "multiply by one" (TwoOp Mult (Var 'a') (Const '1'), Var 'a')
    law_mult_one2 = Law "multiply by one" (TwoOp Mult (Const '1') (Var 'a'), Var 'a')
    law_add_zero = Law "add zero" (TwoOp Add (Const '0') (Var 'a'), Var 'a')
    law_add_zero2 = Law "add zero" (TwoOp Add (Var 'a') (Const '0'), Var 'a')
    law_exponent_add_with_const = Law "extract constant during exponent add" (TwoOp Mult (TwoOp Power (Var 'a') (Var 'b')) (TwoOp Mult (Var 'p') (TwoOp Power (Var 'a') (Var 'c'))), TwoOp Mult (Var 'p') (TwoOp Power (Var 'a') (TwoOp Add (Var 'b') (Var 'c'))))
    law_div_self2 = Law "divide a var by itself" (TwoOp Div (Var 'x') (Var 'x'), Const '1')
    law_div_self = Law "divide a var by itself" (TwoOp Mult (Var 'x') (TwoOp Power (Var 'x') (OneOp Neg (Const '1'))), Const '1')
    law_list = [law_div_self2, law_div_self, law_addition, law_cos, law_power, law_product, law_sin, law_ln, law_self, law_deriv_const, law_deriv_const_neg, law_exponent1, law_exponent_add, law_mult_zero, law_mult_zero2, law_mult_one, law_mult_one2, law_add_zero, law_add_zero2, law_exponent_add_with_const]

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
