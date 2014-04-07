module HaskCalc where
import Control.Applicative ((<$>), (*>), (<*), some)
import Control.Monad (liftM)
import Control.Monad.Trans.State.Strict ()
import Data.List (foldl')
import Data.Map (Map, (!), keys, fromList)
import Text.Parsec.Prim (ParsecT, parserZero, runParsecT, try, runPT)
import Text.ParserCombinators.Parsec ((<|>), Parser, char, digit, getInput, parse, pzero, setInput, string)

type MParser m a= ParsecT String () m a

number :: Monad m => MParser m Double
number=do
	x <- some (digit <|> char '.' <|> char 'e' <|> char '-')
	let{pair = reads x;}
	case pair of
		[] -> parserZero;
		[(x, rest)]->do
			gi <- getInput
			setInput $ rest ++ gi
			return x;

type DoubleBiop = Double -> Double -> Double
-- deriveOne returns a derivation tree (of type (Double,[(a,Double)])).
deriveOne :: Monad m => Map String a -> MParser m Double -> MParser m (Double, [(a, Double)]) -- a:Operator type
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

deriveOneAndSimplify :: Monad m => Map String DoubleBiop -> MParser m Double -> MParser m Double
deriveOneAndSimplify mp parent = liftM simplify $ deriveOne mp parent

exponential :: Monad m => MParser m Double
exponential = deriveOneAndSimplify (fromList [("^",(**))]) (number <|> paren_expr)


doubleRem x 0=x
doubleRem x y = let i = fromIntegral $ floor $ x / y in x - y * i

multi :: Monad m => MParser m Double
multi = deriveOneAndSimplify (fromList [("*",(*)), ("/",(/)), ("%",doubleRem)]) exponential

additive :: Monad m => MParser m Double
additive = deriveOneAndSimplify (fromList [("+",(+)), ("-",(-))]) multi

paren_expr :: Monad m => MParser m Double
paren_expr = char '(' *> expr <* char ')'

expr :: Monad m => MParser m Double
expr = additive <|> paren_expr

eval :: Monad m => String -> m (Either String Double)
eval str = do
  res <- runPT expr () "Parse error" str
  return $ case res of
    Left x  -> Left (show x)
    Right v -> Right v
