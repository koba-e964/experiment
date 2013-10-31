Classfile /C:/Users/2adc/Documents/Programming/experiment/Java8EA/Test.class
  Last modified 2013/10/31; size 1656 bytes
  MD5 checksum 1aff09540e9beb22c60178d19c20bc49
  Compiled from "Test.java"
public class Test
  SourceFile: "Test.java"
  InnerClasses:
       public static final #89= #88 of #92; //Lookup=class java/lang/invoke/MethodHandles$Lookup of class java/lang/invoke/MethodHandles
  BootstrapMethods:
    0: #36 invokestatic java/lang/invoke/LambdaMetafactory.metafactory:(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;
      Method arguments:
        #37 (Ljava/lang/Object;)V
        #38 invokestatic Test.lambda$0:(Ljava/lang/Object;)V
        #37 (Ljava/lang/Object;)V
    1: #36 invokestatic java/lang/invoke/LambdaMetafactory.metafactory:(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;
      Method arguments:
        #40 (I)V
        #41 invokestatic Test.lambda$1:(I)V
        #40 (I)V
    2: #36 invokestatic java/lang/invoke/LambdaMetafactory.metafactory:(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;
      Method arguments:
        #43 (I)I
        #44 invokestatic Test.lambda$2:(II)I
        #43 (I)I
    3: #36 invokestatic java/lang/invoke/LambdaMetafactory.metafactory:(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;
      Method arguments:
        #43 (I)I
        #58 invokestatic Test.lambda$3:(II)I
        #43 (I)I
  minor version: 0
  major version: 52
  flags: ACC_PUBLIC, ACC_SUPER
Constant pool:
   #1 = Methodref          #16.#34        //  java/lang/Object."<init>":()V
   #2 = InvokeDynamic      #0:#39         //  #0:accept:()Ljava/util/function/Consumer;
   #3 = InvokeDynamic      #1:#42         //  #1:accept:()Ljava/util/function/IntConsumer;
   #4 = InvokeDynamic      #2:#45         //  #2:applyAsInt:(I)Ljava/util/function/IntUnaryOperator;
   #5 = String             #46            //  test
   #6 = InterfaceMethodref #47.#48        //  java/util/function/Consumer.accept:(Ljava/lang/Object;)V
   #7 = InterfaceMethodref #49.#50        //  java/util/function/IntConsumer.accept:(I)V
   #8 = Fieldref           #51.#52        //  java/lang/System.out:Ljava/io/PrintStream;
   #9 = InterfaceMethodref #53.#54        //  java/util/function/IntUnaryOperator.applyAsInt:(I)I
  #10 = Methodref          #55.#56        //  java/io/PrintStream.println:(I)V
  #11 = Methodref          #15.#57        //  Test.adder:(I)Ljava/util/function/IntUnaryOperator;
  #12 = InvokeDynamic      #3:#45         //  #3:applyAsInt:(I)Ljava/util/function/IntUnaryOperator;
  #13 = Integer            57005
  #14 = Methodref          #55.#59        //  java/io/PrintStream.println:(Ljava/lang/Object;)V
  #15 = Class              #60            //  Test
  #16 = Class              #61            //  java/lang/Object
  #17 = Utf8               <init>
  #18 = Utf8               ()V
  #19 = Utf8               Code
  #20 = Utf8               LineNumberTable
  #21 = Utf8               main
  #22 = Utf8               ([Ljava/lang/String;)V
  #23 = Utf8               adder
  #24 = Utf8               (I)Ljava/util/function/IntUnaryOperator;
  #25 = Utf8               lambda$3
  #26 = Utf8               (II)I
  #27 = Utf8               lambda$2
  #28 = Utf8               lambda$1
  #29 = Utf8               (I)V
  #30 = Utf8               lambda$0
  #31 = Utf8               (Ljava/lang/Object;)V
  #32 = Utf8               SourceFile
  #33 = Utf8               Test.java
  #34 = NameAndType        #17:#18        //  "<init>":()V
  #35 = Utf8               BootstrapMethods
  #36 = MethodHandle       #6:#62         //  invokestatic java/lang/invoke/LambdaMetafactory.metafactory:(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;
  #37 = MethodType         #31            //  (Ljava/lang/Object;)V
  #38 = MethodHandle       #6:#63         //  invokestatic Test.lambda$0:(Ljava/lang/Object;)V
  #39 = NameAndType        #64:#65        //  accept:()Ljava/util/function/Consumer;
  #40 = MethodType         #29            //  (I)V
  #41 = MethodHandle       #6:#66         //  invokestatic Test.lambda$1:(I)V
  #42 = NameAndType        #64:#67        //  accept:()Ljava/util/function/IntConsumer;
  #43 = MethodType         #68            //  (I)I
  #44 = MethodHandle       #6:#69         //  invokestatic Test.lambda$2:(II)I
  #45 = NameAndType        #70:#24        //  applyAsInt:(I)Ljava/util/function/IntUnaryOperator;
  #46 = Utf8               test
  #47 = Class              #71            //  java/util/function/Consumer
  #48 = NameAndType        #64:#31        //  accept:(Ljava/lang/Object;)V
  #49 = Class              #72            //  java/util/function/IntConsumer
  #50 = NameAndType        #64:#29        //  accept:(I)V
  #51 = Class              #73            //  java/lang/System
  #52 = NameAndType        #74:#75        //  out:Ljava/io/PrintStream;
  #53 = Class              #76            //  java/util/function/IntUnaryOperator
  #54 = NameAndType        #70:#68        //  applyAsInt:(I)I
  #55 = Class              #77            //  java/io/PrintStream
  #56 = NameAndType        #78:#29        //  println:(I)V
  #57 = NameAndType        #23:#24        //  adder:(I)Ljava/util/function/IntUnaryOperator;
  #58 = MethodHandle       #6:#79         //  invokestatic Test.lambda$3:(II)I
  #59 = NameAndType        #78:#31        //  println:(Ljava/lang/Object;)V
  #60 = Utf8               Test
  #61 = Utf8               java/lang/Object
  #62 = Methodref          #80.#81        //  java/lang/invoke/LambdaMetafactory.metafactory:(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;
  #63 = Methodref          #15.#82        //  Test.lambda$0:(Ljava/lang/Object;)V
  #64 = Utf8               accept
  #65 = Utf8               ()Ljava/util/function/Consumer;
  #66 = Methodref          #15.#83        //  Test.lambda$1:(I)V
  #67 = Utf8               ()Ljava/util/function/IntConsumer;
  #68 = Utf8               (I)I
  #69 = Methodref          #15.#84        //  Test.lambda$2:(II)I
  #70 = Utf8               applyAsInt
  #71 = Utf8               java/util/function/Consumer
  #72 = Utf8               java/util/function/IntConsumer
  #73 = Utf8               java/lang/System
  #74 = Utf8               out
  #75 = Utf8               Ljava/io/PrintStream;
  #76 = Utf8               java/util/function/IntUnaryOperator
  #77 = Utf8               java/io/PrintStream
  #78 = Utf8               println
  #79 = Methodref          #15.#85        //  Test.lambda$3:(II)I
  #80 = Class              #86            //  java/lang/invoke/LambdaMetafactory
  #81 = NameAndType        #87:#91        //  metafactory:(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;
  #82 = NameAndType        #30:#31        //  lambda$0:(Ljava/lang/Object;)V
  #83 = NameAndType        #28:#29        //  lambda$1:(I)V
  #84 = NameAndType        #27:#26        //  lambda$2:(II)I
  #85 = NameAndType        #25:#26        //  lambda$3:(II)I
  #86 = Utf8               java/lang/invoke/LambdaMetafactory
  #87 = Utf8               metafactory
  #88 = Class              #93            //  java/lang/invoke/MethodHandles$Lookup
  #89 = Utf8               Lookup
  #90 = Utf8               InnerClasses
  #91 = Utf8               (Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;
  #92 = Class              #94            //  java/lang/invoke/MethodHandles
  #93 = Utf8               java/lang/invoke/MethodHandles$Lookup
  #94 = Utf8               java/lang/invoke/MethodHandles
{
  public Test();
    descriptor: ()V
    flags: ACC_PUBLIC
    Code:
      stack=1, locals=1, args_size=1
         0: aload_0       
         1: invokespecial #1                  // Method java/lang/Object."<init>":()V
         4: return        
      LineNumberTable:
        line 3: 0

  public static void main(java.lang.String[]);
    descriptor: ([Ljava/lang/String;)V
    flags: ACC_PUBLIC, ACC_STATIC
    Code:
      stack=3, locals=5, args_size=1
         0: iconst_4      
         1: istore_1      
         2: invokedynamic #2,  0              // InvokeDynamic #0:accept:()Ljava/util/function/Consumer;
         7: astore_2      
         8: invokedynamic #3,  0              // InvokeDynamic #1:accept:()Ljava/util/function/IntConsumer;
        13: astore_3      
        14: iload_1       
        15: invokedynamic #4,  0              // InvokeDynamic #2:applyAsInt:(I)Ljava/util/function/IntUnaryOperator;
        20: astore        4
        22: aload_2       
        23: ldc           #5                  // String test
        25: invokeinterface #6,  2            // InterfaceMethod java/util/function/Consumer.accept:(Ljava/lang/Object;)V
        30: aload_3       
        31: iconst_3      
        32: invokeinterface #7,  2            // InterfaceMethod java/util/function/IntConsumer.accept:(I)V
        37: getstatic     #8                  // Field java/lang/System.out:Ljava/io/PrintStream;
        40: aload         4
        42: iconst_4      
        43: invokeinterface #9,  2            // InterfaceMethod java/util/function/IntUnaryOperator.applyAsInt:(I)I
        48: invokevirtual #10                 // Method java/io/PrintStream.println:(I)V
        51: getstatic     #8                  // Field java/lang/System.out:Ljava/io/PrintStream;
        54: iconst_3      
        55: invokestatic  #11                 // Method adder:(I)Ljava/util/function/IntUnaryOperator;
        58: bipush        10
        60: invokeinterface #9,  2            // InterfaceMethod java/util/function/IntUnaryOperator.applyAsInt:(I)I
        65: invokevirtual #10                 // Method java/io/PrintStream.println:(I)V
        68: return        
      LineNumberTable:
        line 5: 0
        line 6: 7
        line 7: 13
        line 8: 14
        line 9: 22
        line 10: 30
        line 11: 37
        line 12: 51
        line 13: 68

  static java.util.function.IntUnaryOperator adder(int);
    descriptor: (I)Ljava/util/function/IntUnaryOperator;
    flags: ACC_STATIC
    Code:
      stack=1, locals=1, args_size=1
         0: iload_0       
         1: invokedynamic #12,  0             // InvokeDynamic #3:applyAsInt:(I)Ljava/util/function/IntUnaryOperator;
         6: areturn       
      LineNumberTable:
        line 16: 0
}
