import java.util.*;

public class ArbOrder{
	public static void main(String[] args){
		int size=100;
		Integer[] ary=new Integer[size];
		Arrays.setAll(ary,t->t);
		Arrays.sort(ary,(x,y)->
				x%2+y%2==1? //if x,y are even and odd
				(x%2==0?1:-1) //even number is larger than odd.
				:x-y);
		System.out.println(Arrays.toString(ary));
	}
}