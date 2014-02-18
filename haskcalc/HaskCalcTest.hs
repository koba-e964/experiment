import Test.HUnit
import HaskCalc
main :: IO ()
main=runTestTT alltests >> return ()

-- tests for eval

alltests=TestList [tests_muladd, tests_rem, tests_exp, tests_paren]
-- tests for +-*/
isLeft :: Either a b->Bool
isLeft (Left _)=True
isLeft _=False

tests_muladd=TestList [
	Right 1 ~=? eval "1",
	Right 5 ~=? eval "1+4",
	Right 9 ~=? eval "1+2*4",
	TestCase $ assertBool "error" (isLeft (eval "-+*")),
	eval "1+2*3+4" ~?= Right 11]
-- tests for % (remainder)
tests_rem=TestList [
	eval "3%4" ~?= Right 2,
	eval "10.43%2.5" ~?= Right 0.42]

-- tests for ^ (exponential)
tests_exp=TestList [
	eval "4^4" ~?= Right 256,
	eval "0.0^0.0" ~?= Right 1,
	eval "0.0^-1.0" ~?= Right (1/0)]

-- tests for ()
tests_paren=TestList [
	eval "3+2*(5-9)" ~?= Right (-5),
	eval "(3.5*4)-11" ~?= Right 3]



