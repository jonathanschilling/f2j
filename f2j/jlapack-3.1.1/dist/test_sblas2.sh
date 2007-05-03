#!/bin/sh

java -classpath sblat2.jar:../f2jutil.jar:../blas.jar org.netlib.blas.testing.Sblat2 < sblat2.in
