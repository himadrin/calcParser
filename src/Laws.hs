module Laws where

    import Calc
    import System.IO()
    import Prelude hiding (exp)

       -- laws essentially from parser results
    law_addition :: Law
    law_addition = Law "law addition" (Derive (Var 'x') (TwoOp Add (Var 'a') (Var 'b')),TwoOp Add (Derive (Var 'x') (Var 'a')) (Derive (Var 'x') (Var 'b')))
    
    law_product :: Law
    law_product = Law "law_product" (Derive (Var 'x') (TwoOp Mult (Var 'a') (Var 'b')), TwoOp Add (TwoOp Mult (Derive (Var 'x')(Var 'a'))(Var 'b'))(TwoOp Mult (Var 'a')(Derive (Var 'x')(Var 'b'))))
    
    law_sin:: Law
    law_sin = Law "law_sin" (Derive (Var 'x') (OneOp Sin (Var 'a')),TwoOp Mult (OneOp Cos (Var 'a')) (Derive (Var 'x') (Var 'a')))
    
    law_cos :: Law
    law_cos = Law "law_cos" (Derive (Var 'x') (OneOp Cos (Var 'a')),TwoOp Mult (OneOp Neg (OneOp Sin (Var 'a'))) (Derive (Var 'x') (Var 'a')))
    
    law_ln :: Law
    law_ln = Law "law_ln" (Derive (Var 'x') (OneOp Ln (Var 'a')),TwoOp Mult (TwoOp Div (Const 1) (Var 'a')) (Derive (Var 'x') (Var 'a')))
    
    law_power :: Law
    law_power = Law "law_power" (Derive (Var 'x') (TwoOp Power (Var 'a') (Var 'b')),TwoOp Mult (TwoOp Power (Var 'a') (Var 'b')) (Derive (Var 'x') (TwoOp Mult (Var 'b') (OneOp Ln (Var 'a')))))
    
    law_tan :: Law
    law_tan = Law "law_tan" (OneOp Tan (Var 'a'), (TwoOp Div (OneOp Sin (Var 'a')) (OneOp Cos (Var 'a'))))

    law_self :: Law
    law_self = Law "law_self" (Derive (Var 'x') (Var 'x'),Const 1)

    law_deriv_const :: Law
    law_deriv_const = Law "law: derivative of a const" (Derive (Var 'x') (Var 'p'), Const 0)

    law_deriv_const_neg :: Law
    law_deriv_const_neg = Law "law: derivative of a const" (Derive (Var 'x') (OneOp Neg (Var 'p')), Const 0)

    law_exponent1 :: Law
    law_exponent1 = Law "convert fraction to exponent" (TwoOp Div (Var 'a') (Var 'b'), TwoOp Mult (Var 'a')(TwoOp Power (Var 'b') (OneOp Neg (Const 1))))
    
    law_exponent_add :: Law
    law_exponent_add = Law "adding exponents" (TwoOp Add (TwoOp Power (Var 'a') (Var 'b')) (TwoOp Power (Var 'a') (Var 'c')), TwoOp Power (Var 'a') (TwoOp Add (Var 'b') (Var 'c')))
    
    law_mult_zero :: Law
    law_mult_zero = Law "multiply by zero" (TwoOp Mult (Var 'a') (Const 0), Const 0)

    law_mult_zero2 :: Law
    law_mult_zero2 = Law "multiply by zero" (TwoOp Mult (Const 0) (Var 'a'), Const 0)

    law_mult_one :: Law
    law_mult_one = Law "multiply by one" (TwoOp Mult (Var 'a') (Const 1), Var 'a')

    law_mult_one2 :: Law
    law_mult_one2 = Law "multiply by one" (TwoOp Mult (Const 1) (Var 'a'), Var 'a')

    law_add_neg :: Law
    law_add_neg = Law "adding a negative" (TwoOp Add (Var 'p') (OneOp Neg (Var 'q')), TwoOp Subt (Var 'p') (Var 'q'))
    
    law_add_zero :: Law
    law_add_zero = Law "add zero" (TwoOp Add (Const 0) (Var 'a'), Var 'a')

    law_add_zero2 :: Law
    law_add_zero2 = Law "add zero" (TwoOp Add (Var 'a') (Const 0), Var 'a')

    law_exponent_add_with_const :: Law
    law_exponent_add_with_const = Law "extract constant during exponent add" (TwoOp Mult (TwoOp Power (Var 'a') (Var 'b')) (TwoOp Mult (Var 'p') (TwoOp Power (Var 'a') (Var 'c'))), TwoOp Mult (Var 'p') (TwoOp Power (Var 'a') (TwoOp Add (Var 'b') (Var 'c'))))
    
    law_div_self2 :: Law
    law_div_self2 = Law "divide a var by itself" (TwoOp Div (Var 'x') (Var 'x'), Const 1)
    
    law_div_self :: Law
    law_div_self = Law "divide a var by itself" (TwoOp Mult (Var 'x') (TwoOp Power (Var 'x') (OneOp Neg (Const 1))), Const 1)
    
    law_list :: [Law]
    law_list = [law_div_self2, law_div_self, law_addition, law_cos, law_power, law_product, law_add_neg, law_sin, law_ln, law_self, law_deriv_const, law_deriv_const_neg, law_exponent1, law_exponent_add, law_mult_zero, law_mult_zero2, law_mult_one, law_mult_one2, law_add_zero, law_add_zero2, law_exponent_add_with_const]
