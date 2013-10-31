JDKBIN="C:/Program Files/Java/jdk1.8.0/bin/"
OPTION_EN=-J-Duser.language=en
OPTION_JA=-J-Duser.language=ja
JAVAC=$(JDKBIN)javac.exe
%.class:%.java
	$(JAVAC) $(OPTION_EN) $<
%.class.ja:%.java
	$(JAVAC) $(OPTION_JA) $<
%.javap:
	$(JDKBIN)javap.exe $*
%.dasm:
	$(JDKBIN)javap.exe -v $* >$*.asm
