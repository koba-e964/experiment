module HaskCalc where
import Control.Applicative ((<$>), (*>), (<*), some)
import Control.Monad.Trans.State.Strict ()
import Data.List (foldl')
import Data.Map (Map, (!), keys, fromList)
import Text.ParserCombinators.Parsec ((<|>), Parser, char, digit, getInput, parse, pzero, setInput, string, try)

number:: Parser Double
number=do
	x<-some (digit <|> char '.' <|> char 'e' <|> char '-')
	let{pair=reads x;}
	case pair of
		[]->pzero;
		[(x,rest)]->do
			gi<-getInput
			setInput (rest++gi)
			return x;

type DoubleBiop=Double->Double->Double
-- deriveOne returns a derivation tree (of type (Double,[(a,Double)])).
deriveOne :: Map String a->Parser Double->Parser (Double,[(a,Double)]) -- a:Operator type
deriveOne mp parent=do
	let{ops=keys mp;}
	x<-parent
	(foldl' (<|>) pzero [try(do{
		string op;
		(t,y)<-deriveOne mp parent;
		return $ (x,(mp!op,t):y)
	})|op<-ops])<|>return (x,[])

simplify :: (Double,[(DoubleBiop,Double)])->Double
simplify derivTree=let (x,ls)=derivTree in
	foldl (\x (op,y)->op x y) x ls

deriveOneAndSimplify::Map String DoubleBiop->Parser Double->Parser Double
deriveOneAndSimplify mp parent=simplify <$> (deriveOne mp parent)

exponential ::Parser Double
exponential = deriveOneAndSimplify (fromList [("^",(**))]) (number<|> paren_expr)

data MultiDiv=Multi|Div|Rem deriving (Eq,Show,Ord)

doubleRem x 0=x
doubleRem x y=let i=fromIntegral (floor (x/y)) in x-y*i

multi_list :: Parser (Double,[(MultiDiv,Double)])
multi_list = deriveOne (fromList [("*",Multi),("/",Div),("%",Rem)]::Map String MultiDiv) exponential
multi :: Parser Double
multi=deriveOneAndSimplify (fromList [("*",(*)),("/",(/)),("%",doubleRem)]) exponential

data AddSubt=Add|Subt deriving (Eq,Show,Ord)
additive_list :: Parser (Double,[(AddSubt,Double)])
additive_list = deriveOne (fromList [("+",Add),("-",Subt)]::Map String AddSubt) multi
additive :: Parser Double
additive=deriveOneAndSimplify (fromList [("+",(+)),("-",(-))]) multi

paren_expr :: Parser Double
paren_expr=char '(' *> expr <* char ')'

expr :: Parser Double
expr=additive <|> paren_expr

eval :: String->Either String Double
eval str=case parse expr "Parse error" str of
	Left x->Left (show x)
	Right v->Right v
