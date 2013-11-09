package kobae964_lib;

import java.util.function.*;
import java.util.*;
import java.util.stream.*;

public class Sequence{
	private ToIntFunction<int[]> recur; //recurrence relation
	private int[] buf;
	private long count;//the number of integers which were already output
	public Sequence(ToIntFunction<int[]> recur,int[] initial){
		this.recur=Objects.requireNonNull(recur);
		Objects.requireNonNull(initial);
		this.buf=Arrays.copyOf(initial,initial.length);
		this.count=0L;
	}
	public int numberOfArguments(){
		return buf.length;
	}
	public int next(){
		if(count<buf.length){
			count++;
			return buf[(int)count-1];
		}
		//count>=buf.length, generate new number
		int[] arg=new int[buf.length];
		for(int i=0,n=arg.length,k=(int)(count%n);i<n;i++,k++,k%=n){
			arg[i]=buf[k];
		}
		int next=recur.applyAsInt(arg);
		buf[(int)(count%arg.length)]=next;
		count++;
		return next;
	}
	public IntStream ints(){
		return ints(Long.MAX_VALUE);
	}
	public IntStream ints(long size){
		Spliterator.OfInt spl=new Spl(this,size);
		return StreamSupport.intStream(spl,false);
	}
	static class Spl implements Spliterator.OfInt{
		Sequence g;
		long size;
		long mc;
		Spl(Sequence g,long size){
			this.g=g;
			this.size=size;
			this.mc=g.count;
		}
		private void checkUnmodified(){
			if(mc!=g.count){
				throw new ConcurrentModificationException();
			}
		}
		@Override
		public Spliterator.OfInt trySplit(){
			return null;
		}
		@Override
		public long estimateSize(){
			return size;
		}
		@Override
		public long getExactSizeIfKnown(){
			return size;
		}
		@Override
		public int characteristics(){
			return NONNULL|ORDERED;
		}
		@Override
		public boolean tryAdvance(IntConsumer action){
			checkUnmodified();
			if(size<=0)return false;
			action.accept(g.next());
			mc=g.count;
			size--;
			return true;
		}
	}
	public static void main(String[] args){
		new Sequence(x->x[0]+x[1],new int[]{0,1}) //a_{n+2}=a_{n}+a_{n+1},a_{0,1}=0,1
		.ints().limit(20).forEach(x->
		{
			System.out.println(x);
		});//Fibonacci
		new Sequence(x->2*x[0],new int[]{1})//power of 2,a_{n+1}=2*a_{n},a_0=1
		.ints(20).forEach(System.out::println);
	}
	
}