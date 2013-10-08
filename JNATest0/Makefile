JDK="C:/Program Files/Java/jdk1.7.0_17"
MG32=C:/MinGW/bin/
MG64=C:/mingw64/bin/

CSOURCE=CLib.cpp
CHEADER=CLib.h
COBJ=CLib.o
COBJ64=CLib.o64
CDEF=CLib.def
CDLL32=CLib32.dll
CDLL64=CLib64.dll


JSOURCE=Test.java
JOBJ=Test.class

.PHONY:run clean
run32:$(JOBJ) $(CDLL32)
	java -classpath .\;jna.jar Test 32
run64:$(JOBJ) $(CDLL64)
	$(JDK)/bin/java -classpath .\;jna.jar -d64 Test 64
clean:
	-rm *.class $(COBJ) $(COBJ64) $(CDLL32) $(CDLL64) *.log



%.class:%.java
	javac -classpath .\;./jna.jar $<
%.o:%.cpp
	$(MG32)g++ -c $< -m32
%.o64:%.cpp
	$(MG64)g++ -c $< -o $@ -m64
$(CDLL32):$(COBJ) $(CDEF)
	$(MG32)dllwrap -k -def $(CDEF) -o $@ $(COBJ) -m32
$(CDLL64):$(COBJ64) $(CDEF)
	$(MG64)dllwrap -k -def $(CDEF) -o $@ $(COBJ64) -m64

tx:
	$(JDK)/bin/java -classpath .\;jna.jar Test 32
	
