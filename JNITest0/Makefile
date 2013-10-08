JDK="C:/Program Files/Java/jdk1.7.0_17"
JNI_INCLUDE=$(JDK)/include
HEADER=caller_jni.h
SOURCE=caller_jni.cpp
OBJ=caller_jni.o
DEF=caller_jni.def
DLL=CallerNative.dll
TOOLPATH=C:/mingw64/bin/

.PHONY:all run clean

#all: When this target is specified, all files necessary to run these codes will be made.
all:
	make $(DLL)
#run: When this target is specified, Caller will be run.
run:all
	$(JDK)/bin/java Caller -d64

Caller.class:Caller.java
	javac $<
$(HEADER):Caller.class
	javah -o $@ Caller

$(OBJ):$(SOURCE) $(HEADER)
	$(TOOLPATH)g++ -Xpreprocessor -I$(JNI_INCLUDE) -Xpreprocessor -I$(JNI_INCLUDE)/win32 $(SOURCE) -o $@ -c -m64
$(DLL):$(OBJ) $(DEF)
	$(TOOLPATH)dllwrap -k -def $(DEF) -o $@ $(OBJ) -m64
clean:
	-rm Caller.class $(HEADER) $(OBJ) $(DLL)
