import java.io.UnsupportedEncodingException;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Structure;

public class Test
{
	public interface CLibrary extends Library{
		CLibrary inst=(CLibrary)Native.loadLibrary("msvcrt", CLibrary.class);
		void printf(String format,Object... args);
	}
	public interface WinLib extends Library{
		WinLib inst=(WinLib)Native.loadLibrary("user32",WinLib.class);
		int MessageBoxA(int hWnd, byte[] lpText, byte[] lpCaption, int uType);
	}
	public interface MyLib extends Library{
		public static MyLib inst=(MyLib)Native.loadLibrary("CLib32", MyLib.class);
		int calcHashCodeHimmel(HimmelAdler himmel);
	}
	public interface MyLib64 extends Library{
		public static MyLib64 inst=(MyLib64)Native.loadLibrary("CLib64", MyLib64.class);
		int calcHashCodeHimmel(HimmelAdler himmel);
	}
	public static class HimmelAdler extends Structure{
		public String name;//All fields have to be public in order to get the size of this struct.
		public int ability;
		public long value;
		public int hashCode()
		{
			int h=0xdeadc0de;
			for(byte ch:name.getBytes()){
				h*=31;
				h+=ch&0xff;
			}
			h+=ability;
			h+=(int)value;
			h+=(int)(value>>32L);
			return h;
		}
	}
	public static void main(String[] args)
	{
		boolean bit64=args[0].equals("64");
		CLibrary.inst.printf("Test");
		HimmelAdler himmel=new HimmelAdler();
		himmel.name="jbargajl";
		himmel.ability=234;
		himmel.value=103000331414L;
		int hash;
		if(bit64){
			hash=MyLib64.inst.calcHashCodeHimmel(himmel);
		}else{
			hash=MyLib.inst.calcHashCodeHimmel(himmel);
		}
		if(hash!=himmel.hashCode())
		{
			throw new RuntimeException("Wrong hash code hash="+hash+", correct="+himmel.hashCode());
		}
	}
}