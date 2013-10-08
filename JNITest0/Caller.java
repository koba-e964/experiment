public class Caller
{
	native int func(int i);
	static{
		System.loadLibrary("CallerNative");
	}
	public static void main(String[] args)
	{
		int result=new Caller().func(3);
		System.out.println("func(3)="+result);
	}
}
