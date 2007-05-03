#!/bin/sh

java -classpath slintest.jar:smatgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.testing.lin.Schkaa < stest.in
