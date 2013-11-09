import kobae964_lib.Sequence;

import java.util.*;
import java.util.stream.*;

public class MersenneTwister{
	Sequence seq;
	final static int N=624,M=397;
	public MersenneTwister(int[] seed){
		int[] initial=Arrays.copyOf(seed,N);
		seq=new Sequence(x->{
			int y = (x[0]&0x80000000)|(x[1]&0x7fffffff);
			return  x[M]^(y>>>1)^((y&1)==1?(int)0x9908b0dfL:0);
		},initial);
		IntStream.range(0,N).forEach(x->{next();});
	}
	public int next(){
		long y=seq.next()&((1L<<32)-1);
		y ^= (y >>> 11);
		y ^= (y <<   7) & 0x9d2c5680L;
		y ^= (y <<  15) & 0xefc60000L;
		y ^= (y >>> 18);
		return (int)y;
	}
	IntStream ints(){
		return IntStream.generate(this::next);
	}
	public static void main(String[] args){
		int[] init=new int[N];
		int s=(int)0xdeadc0deL;
		for(int i=0;i<N;i++){
			s*=135;s+=5;init[i]=s;
		}
		MersenneTwister rng=new MersenneTwister(init);
		rng.ints().limit(0x20).forEach(System.out::println);
	}
}
