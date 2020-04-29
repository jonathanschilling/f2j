#!/bin/sh

java -classpath sblat3.jar:../f2jutil.jar:../blas.jar org.netlib.blas.testing.Sblat3 < sblat3.in
