
F77=g77

default:
	f2java strdec.f

src: clean
	f2java strdec.f
	/bin/rm -f *.class
	javac *.java
	javab *.class

$(CRAP): clean $(CRAP).f
	f2java $(CRAP).f
	/bin/rm -f *.class
	javac *.java
	javab *.class

clean:
	/bin/rm -f *.f2j *.java *.class *.old a.out
