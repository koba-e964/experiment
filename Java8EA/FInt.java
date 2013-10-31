import java.util.function.*;


@FunctionalInterface
interface FI0{
	void apply(int t);
}

//no @FunctionalInterface annotation.
interface FI1{
	void apply(int t);
	static void apply2(double f){return (int)f;}
}
@FunctionalInterface
interface BadFI{//not functional interface, 2 abstract methods.
	int apply(int i);
	int badd(String v);
}

@FunctionalInterface
interface NotCompatible{
	int bad(int value);
}

public class FInt{
	public static void main(String[] args){
		FI0 fi0=System.out::print;
		FI1 fi1=System.out::println;
		NotCompatible nc=FI1::apply2;
	}
}
