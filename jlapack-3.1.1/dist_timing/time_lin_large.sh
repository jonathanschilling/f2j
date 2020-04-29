#!/bin/sh

java -Xmx500M -classpath lintime.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.timing.lin.Dtimaa < input_files_large/DBAND.in
java -Xmx500M -classpath lintime.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.timing.lin.Dtimaa < input_files_large/DBLASA.in
java -Xmx500M -classpath lintime.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.timing.lin.Dtimaa < input_files_large/DBLASB.in
java -Xmx500M -classpath lintime.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.timing.lin.Dtimaa < input_files_large/DBLASC.in
java -Xmx500M -classpath lintime.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.timing.lin.Dtimaa < input_files_large/DTIME.in
java -Xmx500M -classpath lintime.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.timing.lin.Dtimaa < input_files_large/DTIME2.in
