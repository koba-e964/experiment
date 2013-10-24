package func;

import java.util.*;
import java.util.function.*;
import java.util.stream.*;

public class FuncTest{
	public static class Stopper implements IntPredicate{
		static final int pat=114514;
		int v=0;
		boolean fin=true;
		static final int mod=1000000;
		public boolean test(int d){
			if(!fin)return false;
			v*=10;
			v+=d;
			v%=mod;
			if(v==pat){
				fin=false;
			}
			return true;
		}
	}
	public static class Counter implements IntUnaryOperator{
		public int applyAsInt(int v){
			count++;
			return v;
		}
	}
	static int count=0;
	public static void main(String[]args){
		Random rnd=new Random();
		rnd.ints(0x800000L,0,3).map(x->new int[]{1,4,5}[x])
			.filter(new Stopper())
			.map(new Counter())
			.forEach(System.out::print);
		System.out.println("\n114514");
		System.out.println("count="+count);
	}
}