import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans.State.Strict (runStateT)
import qualified Data.Map as Map
import Test.HUnit
import HaskCalc
main :: IO ()
main=runTestTT alltests >> return ()

-- eval with m = Identity
eval0 :: String -> Either String Double
eval0 = runIdentity . eval 

-- utility for Double-value check
assertDoubleEqual :: String->Double->Double->Double->Assertion
assertDoubleEqual msg expected actual eps=
	assertBool msg (if expected==0.0 then abs actual <= eps
	else abs (actual/expected-1) <=eps)

(~===?) :: Double->Double->Test
expected ~===? actual = TestCase (assertDoubleEqual ("expected:"++(show expected)++", actual:"++(show actual))
	expected actual 1e-11)

(~?===) :: Double->Double->Test
actual ~?=== expected = TestCase (assertDoubleEqual ("expected:"++(show expected)++", actual:"++(show actual))
	expected actual 1e-11)

-- tests for eval0

alltests=TestList [tests_muladd, tests_rem, tests_exp, tests_paren, tests_evalStrings]
-- tests for +-*/
isLeft :: Either a b->Bool
isLeft (Left _)=True
isLeft _=False

tests_muladd=TestList [
	Right 1 ~=? eval0 "1",
	Right 5 ~=? eval0 "1+4",
	Right 9 ~=? eval0 "1+2*4",
	TestCase $ assertBool "error" (isLeft (eval0 "-+*")),
	eval0 "1+2*3+4" ~?= Right 11]
-- tests for % (remainder)
tests_rem=TestList [
	eval0 "6%4" ~?= Right 2,
	(case eval0 "10.43%2.5" of Right x->x) ~?=== 0.43]

-- tests for ^ (exponential)
tests_exp=TestList [
	eval0 "4^4" ~?= Right 256,
	eval0 "0.0^0.0" ~?= Right 1,
	eval0 "0.0^-1.0" ~?= Right (1/0),
	eval0 "2*3^4" ~?= Right 162]

-- tests for ()
tests_paren=TestList [
	eval0 "3+2*(5-9)" ~?= Right (-5),
	eval0 "(3.5*4)-11" ~?= Right 3]

-- tests for evalStrings
tests_evalStrings = TestList [
  (fst . runIdentity) (runStateT (evalStrings ["test=4", "var=3", "test*var"]) Map.empty) ~?= [Right 4.0, Right 3.0, Right 12.0]]

