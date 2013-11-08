import java.util.function.*;

public class Comb{
	static class YCombinator<T> implements Function< UnaryOperator<UnaryOperator<T>> ,UnaryOperator<T> >{
		public UnaryOperator<T> apply(UnaryOperator<UnaryOperator<T>> f){
			return x->f.apply(this.apply(f)).apply(x);
		}
	}
	public static void main(String[] args){
		UnaryOperator<UnaryOperator<Integer>> hf=
		f->x->{
			if(x==0){return 1;}
			return x*f.apply(x-1);
		};
		int val=5;
		System.out.println("fact("+val+")="+new YCombinator<Integer>().apply(hf).apply(val));
	}
}
