module HaskCalc where
import Control.Applicative ((<$>), (*>), (<*), (<*>), many, some)
import Control.Monad (liftM)
import Control.Monad.Trans.State.Strict ()
import Data.List (foldl')
import Data.Map (Map, (!), empty, keys, fromList)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Text.Parsec.Prim (ParsecT, getState, parserZero, putState, runParsecT, try, runPT)
import Text.ParserCombinators.Parsec ((<|>), Parser, char, digit, getInput, letter, parse, pzero, setInput, string)

type MParser u m a= ParsecT String u m a
type Table = Map String Double

-- Double value represented as String
number :: Monad m => MParser u m Double
number = do
	x <- some (digit <|> char '.' <|> char 'e' <|> char '-')
	let{pair = reads x;}
	case pair of
		[] -> parserZero;
		[(x, rest)]->do
			gi <- getInput
			setInput $ rest ++ gi
			return x;

-- variable
variable :: Monad m => MParser Table m Double
variable = do
  name <- try $ (:) <$> letter <*> many (digit <|> letter)
  tbl <- getState
  let res = Map.lookup name tbl
  case res of
    Nothing -> parserZero
    Just v  -> return $! v

type DoubleBiop = Double -> Double -> Double
-- deriveOne returns a derivation tree (of type (Double,[(a,Double)])).
deriveOne :: Monad m => Map String a -> MParser u m Double -> MParser u  m (Double, [(a, Double)]) -- a:Operator type
deriveOne mp parent = do
	let {ops = keys mp;}
	x <- parent
	(foldl' (<|>) parserZero [try $ do{
		string op;
		(t, y) <- deriveOne mp parent;
		return $ (x, (mp ! op, t) : y)
	} | op <- ops]) <|> return (x, [])

simplify :: (Double,[(DoubleBiop,Double)])->Double
simplify derivTree = let (x,ls) = derivTree in
	foldl (\x (op, y) -> op x y) x ls

deriveOneAndSimplify :: Monad m => Map String DoubleBiop -> MParser u m Double -> MParser u m Double
deriveOneAndSimplify mp parent = liftM simplify $ deriveOne mp parent

exponential :: Monad m => MParser Table m Double
exponential = deriveOneAndSimplify (fromList [("^",(**))]) (number <|> variable <|> paren_expr)


doubleRem x 0=x
doubleRem x y = let i = fromIntegral $ floor $ x / y in x - y * i

multi :: Monad m => MParser Table m Double
multi = deriveOneAndSimplify (fromList [("*",(*)), ("/",(/)), ("%",doubleRem)]) exponential

additive :: Monad m => MParser Table m Double
additive = deriveOneAndSimplify (fromList [("+",(+)), ("-",(-))]) multi

paren_expr :: Monad m => MParser Table m Double
paren_expr = char '(' *> expr <* char ')'

expr :: Monad m => MParser Table m Double
expr = additive <|> paren_expr

eval :: Monad m => String -> m (Either String Double)
eval str = do
  res <- runPT expr (fromList [("test", 22.0)]) "Parse error" str
  return $ case res of
    Left x  -> Left (show x)
    Right v -> Right v
