import java.util.function.*;

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
		return;
	}
	static IntUnaryOperator adder(int n){
		return t->t+n+0xdead;
	}
}
