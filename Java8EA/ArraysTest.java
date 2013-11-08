import java.util.*;
import java.util.concurrent.atomic.*;

public class ArraysTest{
	static AtomicLong cnt=new AtomicLong(0L);
	static int calc(int x,int y){
	/* neither associative nor side-effect-free!!! */
		long c=cnt.getAndIncrement();
		if(y>=0)
			System.out.println(c+":("+x+","+y+")");
		return (int)c;
	}
	public static void main(String[] args){
		int size=1<<10;
		int[] ary=new int[size];
		for(int i=0;i<size;i++){
			ary[i]=-i-1;
		}
		Arrays.parallelPrefix(ary,ArraysTest::calc);
		//System.out.println(Arrays.toString(ary));
	}
}