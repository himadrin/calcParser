import Test.Tasty
import Test.Tasty.HUnit
import Implementation
import Prelude hiding (exp)
import Calc

main :: IO ()
main = defaultMain (testGroup "Library Tests" [testMatch, testRewrites])
testMatch, testRewrites :: TestTree

--test exprs
sub :: Subst
sub = [((Var 'x'),(Const 3)), ((Var 'y'),(Const 4))]

exp :: Expr
exp = (TwoOp Add (Var 'x')(Var 'y'))

--testing all functions in implementation
testMatch = testCase "3 in [1..5]" (assertBool "guess not" (3 `elem` [1..5]))
testApply = testCase "test apply func" (assertBool "apply is wrong" (apply sub exp == TwoOp Add (Const 3)(Const 4)))