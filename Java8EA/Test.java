import java.util.function.*;
import java.util.stream.*;
import kobae964_lib.Sequence;


public class Test{
	public static void main(String[] args){
		int n=4;
		Consumer<Object> func=(Object t)->{System.out.println(t);};
		IntConsumer ic=(int t)->{System.out.println(t*2);};
		IntUnaryOperator op=t->t+n;
		func.accept("test");
		ic.accept(3);
		System.out.println(op.applyAsInt(4));
		System.out.println(adder(3).applyAsInt(10));
		Sequence seq=new Sequence(x->2*x[0],new int[]{1});
		IntStream is=seq.ints(3);
		is.forEach(System.out::println);
		ToIntBiFunction<Integer,String> fisi=(Integer x,String y)->{
		int z=x+y.length();
		return z;
		};
		IntUnaryOperator fii=(int x)->x;
		System.out.println(fisi.applyAsInt(3,"test"));
		((IntUnaryOperator)x->x*x).applyAsInt(20131124);
		//System.out.println((t->t*t).toString());
	}
	static IntUnaryOperator adder(int n){
		return t->t+n+0xdead;
	}
}
