#!/bin/sh

java -classpath lintest.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.testing.lin.Dchkaa < dtest.in
