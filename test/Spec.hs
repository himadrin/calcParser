import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.LeanCheck as LC

main :: IO ()
main = defaultMain (testGroup "Library Tests" [testMatch, testApply, testRewrites])
testMatch, testApply, testRewrites :: TestTree

--testing all functions in implementation
testMatch = testCase "3 in [1..5]" (assertBool "guess not" (3 `elem` [1..5]))
test2 = testCase "5 in [1..3]" (assertBool "guess not" (5 `elem` [1..3]))
test3 = localOption (Timeout 500000 "0.5 seconds") $
        testCase "1 in [3..]" (assertBool "guess not" (1 `elem` [3..]))