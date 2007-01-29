#!/bin/sh

java -server -Xmx500M -classpath eigtime.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.timing.lin.Dtimee < dgeptim.in
java -server -Xmx500M -classpath eigtime.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.timing.lin.Dtimee < dneptim.in
java -server -Xmx500M -classpath eigtime.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.timing.lin.Dtimee < dseptim.in
java -server -Xmx500M -classpath eigtime.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.timing.lin.Dtimee < dsvdtim.in
