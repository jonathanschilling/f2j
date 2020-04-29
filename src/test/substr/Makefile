
default:
	f2java test.f

src: clean
	f2java test.f
	/bin/rm -f *.class
	javac *.java
	javab *.class

clean:
	/bin/rm -f *.f2j *.java *.class *.old a.out
