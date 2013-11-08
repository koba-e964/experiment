import java.util.stream.*;
import java.util.*;
import java.util.function.*;

public class InfiniteStream{
	static class LFSR implements Spliterator.OfInt{
		private int val;
		private long limit;
		public LFSR(int v,long limit){
			val=v;
			this.limit=limit;
		}
		public LFSR(int v){
			val=v;
			this.limit=Long.MAX_VALUE; //effectively infinite
		}
		private int next(){
			int bit=(val&1)^((val&4)>>2)^
				((val&8)>>3)^((val&0x20)>>5);
			val=(val>>1)|(bit<<15);
			return val;
		}
		public boolean tryAdvance(IntConsumer action){
			if(limit>=1){
				action.accept(next());
				limit--;
				return true;
			}
			return false;
		}
		public Spliterator.OfInt trySplit(){
			return null;
		}
		public int characteristics(){
			return ORDERED;
		}
		public long estimateSize(){
			return limit;
		}
		public long getExactSizeIfKnown(){
			return limit;
		}
		@Override
		public String toString(){
			return "state="+String.format("%04x",val)+", limit="+limit;
		}
	}
	public static void main(String[] args){
		int size=100;
		LFSR l=new LFSR(3);
		IntStream is=StreamSupport.intStream(l,//infinite stream of LFSR
				false);//parallel=false
		is.limit(size).forEach(x->{System.out.printf("%04x\n",x);});
		System.out.println(l);
	}
}