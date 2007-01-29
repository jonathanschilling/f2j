#!/bin/sh

java -server -Xmx500M -classpath lintime.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.timing.lin.Dtimaa < DBAND.in
java -server -Xmx500M -classpath lintime.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.timing.lin.Dtimaa < DBLASA.in
java -server -Xmx500M -classpath lintime.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.timing.lin.Dtimaa < DBLASB.in
java -server -Xmx500M -classpath lintime.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.timing.lin.Dtimaa < DBLASC.in
java -server -Xmx500M -classpath lintime.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.timing.lin.Dtimaa < DTIME.in
java -server -Xmx500M -classpath lintime.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.timing.lin.Dtimaa < DTIME2.in
