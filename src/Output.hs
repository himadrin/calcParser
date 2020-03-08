module Output where

    -- import Parser as P
    import Calc
    import Implementation
    import System.IO()
    import Prelude hiding (exp)

    --uses rewrites to create list of steps
    stepList :: [Law] -> Expr -> [Step]
    stepList laws_list expression = case steps of
                        [] -> []
                        (Step name expr):_ -> (Step name expr) : (stepList laws_list expr)

                    -- recursively calls rewrites and doMath on expr for each step
                    where steps = [Step name new_exp | Law name (e1, e2) <- laws_list, 
                                                        new_exp <- (map simplify(rewrites e1 e2 expression))]


    -- extra feature : simplify math if constants are being operated on
    simplify :: Expr -> Expr
    simplify (TwoOp Power (Const c1) (Const c2)) = Const((^) c1 c2)
    simplify (TwoOp Add (Const c1) (Const c2)) = Const((+) c1 c2)
    simplify (TwoOp Subt (Const c1) (Const c2)) = Const((-) c1 c2)
    simplify (TwoOp Mult (Const c1) (Const c2)) = Const((*) c1 c2)
    simplify (TwoOp Div (Const c1) (Const c2)) = Const((div) c1 c2)
    simplify expr = expr

    --called in main to handle expression and error from parse - takes in laws and expr
    final :: [Law] -> Err Expr -> Err [Step]
    final laws_list (Expr e) = Expr (stepList laws_list e)
    final _ (Error string) = Error string