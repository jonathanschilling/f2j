
F2J=f2java
F2JFLAGS=
JAVA=java
F77=g77
FFLAGS=

native: f77_test
	./f77_test

f77_test: intrinsic.f
	$(F77) $(FFLAGS) intrinsic.f -o f77_test

intrinsic_test: Intrinsic.class
	$(JAVA) Intrinsic

Intrinsic.class: intrinsic.f
	$(F2J) $(F2JFLAGS) intrinsic.f

clean:
	/bin/rm -f Intrinsic.class Achk.class Dchk.class \
           Intrinsic.java Achk.java Dchk.java \
           Intrinsic.f2j *.old
