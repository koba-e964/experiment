module HaskCalc where
import Control.Applicative ((<$>), (*>), (<*), (<*>), many, some)
import Control.Monad (forM, liftM)
import Control.Monad.State.Class (get, put)
import Control.Monad.Trans.State.Strict (runStateT)
import Data.List (foldl')
import Data.Map (Map, (!), empty, keys, fromList)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Text.Parsec.Char (spaces)
import Text.Parsec.Prim (ParsecT, (<?>), getState, parserFail, parserZero, putState, runParsecT, try, runPT)
import Text.ParserCombinators.Parsec ((<|>), Parser, char, digit, getInput, letter, parse, pzero, setInput, string)
import Var

type MParser u m a= ParsecT String u m a
type SParser m a = ParsecT String () m a
type Table = Map String Double

-- Double value represented as String
number :: Monad m => SParser m Double
number = do
	x <- try (some (digit <|> char '.' <|> char 'e' <|> char '-')) <?> "not a number"
	let{pair = reads x;}
	case pair of
		[] -> parserFail "parse failure on number";
		[(x, rest)]->do
			gi <- getInput
			setInput $ rest ++ gi
			return x;

-- identifier

identifier :: Monad m => SParser (Var m) String
identifier = do
  name <- try ((:) <$> letter <*> many (digit <|> letter)) <?> "an identifier"
  return name

-- variable
variable :: Monad m => SParser (Var m) Double
variable = do
  name <- identifier
  tbl <- get
  let res = Map.lookup name tbl
  case res of
    Nothing -> return $ error $ "unbound variable:"++ name -- parsing succeeds, but fetching fails.
    Just v  -> return $! v

type DoubleBiop = Double -> Double -> Double
-- deriveOne returns a derivation tree (of type (Double,[(a,Double)])).
deriveOne :: Monad m => Map String a -> SParser m Double -> SParser m (Double, [(a, Double)]) -- a:Operator type
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

deriveOneAndSimplify :: Monad m => Map String DoubleBiop -> SParser m Double -> SParser m Double
deriveOneAndSimplify mp parent = liftM simplify $ deriveOne mp parent

exponential :: Monad m => SParser (Var m) Double
exponential = deriveOneAndSimplify (fromList [("^",(**))]) (number <|> variable <|> paren_expr)


doubleRem x 0=x
doubleRem x y = let i = fromIntegral $ floor $ x / y in x - y * i

multi :: Monad m => SParser (Var m) Double
multi = deriveOneAndSimplify (fromList [("*",(*)), ("/",(/)), ("%",doubleRem)]) exponential

additive :: Monad m => SParser (Var m) Double
additive = deriveOneAndSimplify (fromList [("+",(+)), ("-",(-))]) multi

paren_expr :: Monad m => SParser (Var m) Double
paren_expr = char '(' *> expr <* char ')'

expr :: Monad m => SParser (Var m) Double
expr = try assignment <|> additive <|> paren_expr

assignment :: Monad m => SParser (Var m) Double
assignment = do
  name <- identifier
  spaces
  char '='
  spaces
  val <- expr
  tbl <- get
  put $ Map.insert name val tbl
  return val

eval :: Monad m => String -> m (Either String Double)
eval str =
  let { res = evalString str;
        initialState = fromList [("test", 22.5)]; } in
  liftM fst $ runStateT res initialState

evalString :: Monad m => String -> Var m (Either String Double)
evalString str = do
  res <- runPT expr () "Parse error" str
  return $ case res of
    Left x  -> Left (show x)
    Right v -> Right v

-- evalStrings sequentially evaluates expressions. Note that modifications of variables affect later expressions.
evalStrings :: Monad m => [String] -> Var m [Either String Double]
evalStrings strls = forM strls evalString