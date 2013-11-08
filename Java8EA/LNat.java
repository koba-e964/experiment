import java.util.function.*;

public class LNat{
	static  UnaryOperator<UnaryOperator<Integer>> intToFunctor(int n){
		if(n<=0)return f->t->t;
		return f->x->f.apply(intToFunctor(n-1).apply(f).apply(x));
	}
	static int functorToInt(UnaryOperator<UnaryOperator<Integer>> functor){
		return functor.apply(t->t+1).apply(0);
	}
	public static void main(String[] args){
		UnaryOperator<UnaryOperator<Integer>> five=intToFunctor(50);
		System.out.println(five);
		System.out.println(functorToInt(five));
	}
}
