
JAVAC=javac
BCEL_JAR=./bcel-5.2.jar

all: GotoTrans.class

GotoTrans.class: GotoTrans.java
	$(JAVAC) -classpath $(BCEL_JAR) GotoTrans.java

clean:
	/bin/rm -f GotoTrans*.class
