#!/bin/sh

java -classpath slintest.jar:smatgen.jar:../f2jutil.jar:../sblas.jar:../slapack.jar org.netlib.lapack.testing.lin.Schkaa < stest.in
