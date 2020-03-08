import Test.Tasty
import Test.Tasty.HUnit
import Output
import Laws
import Calc
import Implementation
import Parser

--test expressions

-- x^2
expr1 :: Expr
expr1 = (Derive (Var 'x') (TwoOp Power (Var 'x') (Const 3)))
step1 :: Step
step1 = Step "adding a negative" (TwoOp Mult (Const 3) (TwoOp Power (Var 'x') (Const 2)))
-- sin(x)
expr2 :: Expr
expr2 = Derive (Var 'x') (OneOp Sin (Var 'x'))
step2 :: Step
step2 = Step "multiply by one" (OneOp Cos (Var 'x'))
-- cos(x^2)
expr3 :: Expr
expr3 = (Derive (Var 'x') (OneOp Cos (TwoOp Power (Var 'x') (Const 2))))
step3 :: Step
step3 = Step "exponent of 1 is just var" (TwoOp  Mult  (OneOp Neg (OneOp Sin (TwoOp Power (Var 'x') (Const 2)))) (TwoOp  Mult  (Const 2) (Var 'x')))

-- x^(4 + 2x +3)
expr4 :: Expr
expr4 = Derive (Var 'x') (TwoOp Power (Var 'x') (TwoOp Add (TwoOp Add (Const 4) (TwoOp Mult (Const 2) (Var 'x'))) (Const 3)))
step4 :: Step
step4 = Step "multiply by one" (TwoOp Mult (TwoOp Power (Var 'x') (TwoOp Add (TwoOp Add (Const 4) (TwoOp Mult (Const 2) (Var 'x'))) (Const 3))) (TwoOp Add (TwoOp Mult (Const 2) (OneOp Ln (Var 'x'))) (TwoOp Mult (TwoOp Add (TwoOp Add (Const 4) (TwoOp Mult (Const 2) (Var 'x'))) (Const 3)) (TwoOp Power (Var 'x') (OneOp Neg (Const 1))))))

-- (x/2) + (2*x) + 3
expr5 :: Expr
expr5 = Derive (Var 'x') (TwoOp Add (TwoOp Add (TwoOp Div (Var 'x') (Const 2)) (TwoOp Mult (Const 2) (Var 'x'))) (Const 3))
step5 :: Step
step5 = Step "add zero" (TwoOp Add (TwoOp Power (Const 2) (OneOp Neg (Const 1))) (Const 2))

-- 3x + 2x
expr6 :: Expr
expr6 = (Derive (Var 'x') (TwoOp Add (TwoOp Mult (Const 3) (Var 'x')) (TwoOp Mult (Const 2) (Var 'x'))))
step6 :: Step
step6 = Step "multiply by zero" (Const 5)

-- x^(2+3)
expr7 :: Expr
expr7 = (Derive (Var 'x') (TwoOp Power (Var 'x') (TwoOp Add (Const 2) (Const 3))))
step7 :: Step -- first step
step7 = Step "law_power" (TwoOp Mult (TwoOp Power (Var 'x') (Const 5)) (Derive (Var 'x') (TwoOp Mult (Const 5) (OneOp Ln (Var 'x')))))


main :: IO ()
main = defaultMain (testGroup "derivatives" [test1, test2, test3, test4, test5, test6, test7])
test1, test2, test3, test4, test5, test6, test7 :: TestTree

test1 = testCase "derivative of x^3" $
    assertBool "is 3 * x" $ ((last(stepList law_list expr1))==step1)

test2 = testCase "derivative of sin(x)" $
    assertBool "is cos(x)" $ ((last(stepList law_list expr2))==step2)

test3 = testCase "derivative of cos(x^2)" $
    assertBool "is -2xsin(x^2)" $ ((last(stepList law_list expr3))==step3)

test4 = testCase "derivative of x^(4 + 2x +3)" $
    assertBool "is ((x^((4 + (2 * x)) + 3)) * ((2 * ln (x)) + (((4 + (2 * x)) + 3) * (x^-(1)))))" $ ((last(stepList law_list expr4))==step4)

test5 = testCase "derivative of (x/2) + (2*x) + 3" $
    assertBool "is 2 + 1/2" $ ((last(stepList law_list expr5))==step5)

test6 = testCase "derivative of 3x + 2x" $
    assertBool "is 5" $ ((last(stepList law_list expr6))==step6)

test7 = testCase "derivative of x^(2+3)" $
    assertBool "the first step adds 2 and 3" $ ((head(stepList law_list expr7))==step7)
