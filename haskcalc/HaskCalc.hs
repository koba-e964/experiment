module HaskCalc where
import Control.Applicative hiding ((<|>))
import Data.Map hiding (foldl,foldl')
import Data.List (foldl')
import Text.ParserCombinators.Parsec

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

-- deriveOne returns a derivation tree (of type (Double,[(a,Double)])).
type DoubleBiop=Double->Double->Double
deriveOne :: [String]->Map String a->Parser Double->Parser (Double,[(a,Double)]) -- a:Operator type
deriveOne ops mp parent=do
	x<-parent
	(foldl' (<|>) pzero [try(do{
		string op;
		(t,y)<-deriveOne ops mp parent;
		return $ (x,(mp!op,t):y)
	})|op<-ops])<|>return (x,[])

simplify :: (Double,[(DoubleBiop,Double)])->Double
simplify derivTree=let (x,ls)=derivTree in
	foldl (\x (op,y)->op x y) x ls

deriveOneAndSimplify::[String]->Map String DoubleBiop->Parser Double->Parser Double
deriveOneAndSimplify ops mp parent=simplify <$> (deriveOne ops mp parent)

data MultiDiv=Multi|Div deriving (Eq,Show,Ord)

multi_list :: Parser (Double,[(MultiDiv,Double)])
multi_list = deriveOne ["*","/"] (fromList [("*",Multi),("/",Div)]::Map String MultiDiv) number
multi :: Parser Double
multi=deriveOneAndSimplify ["*","/"] (fromList [("*",(*)),("/",(/))]) number

data AddSubt=Add|Subt deriving (Eq,Show,Ord)
additive_list :: Parser (Double,[(AddSubt,Double)])
additive_list = deriveOne ["+","-"] (fromList [("+",Add),("-",Subt)]::Map String AddSubt) multi
additive :: Parser Double
additive=deriveOneAndSimplify ["+","-"] (fromList [("+",(+)),("-",(-))]) multi

expr :: Parser Double
expr=additive

eval :: String->Either String Double
eval str=case parse expr "Parse error" str of
	Left x->Left (show x)
	Right v->Right v
