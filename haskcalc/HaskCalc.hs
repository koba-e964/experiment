module HaskCalc where
import Control.Applicative hiding ((<|>))
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

data MultiDiv=Multi|Div deriving (Eq,Show)

multi_list :: Parser (Double,[(MultiDiv,Double)])
multi_list = do
	x<-number
	try(do{
		char '*';
		(t,y)<-multi_list;
		return $ (x,(Multi,t):y);
	})<|>try(do{
		char '/';
		(t,y)<-multi_list;
		return $ (x,(Div,t):y);
	})<|>return (x,[])

multi :: Parser Double
multi =do
	(x,ls)<-multi_list
	return $ foldl k x ls
	where{
		k x (Multi,y)=x*y;
		k x (Div,y)=x/y;
	}

data AddSubt=Add|Subt deriving (Eq,Show)
additive_list :: Parser (Double,[(AddSubt,Double)])
additive_list = do
	x<-multi
	try(do{
		char '+';
		(t,y)<-additive_list;
		return $ (x,(Add,t):y);
	})<|>try(do{
		char '-';
		(t,y)<-additive_list;
		return $ (x,(Subt,t):y);
	})<|>return (x,[])

additive :: Parser Double
additive = do
	(x,ls)<-additive_list
	return $ foldl k x ls
	where{
		k x (Add,y)=x+y;
		k x (Subt,y)=x-y;
	}

expr :: Parser Double
expr=additive

eval :: String->Either String Double
eval str=case parse expr "Parse error" str of
	Left x->Left (show x)
	Right v->Right v
