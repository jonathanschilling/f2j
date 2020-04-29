#!/bin/sh

java -Xmx500M -classpath eigtime.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.timing.eig.Dtimee < input_files_large/DGEPTIM.in
java -Xmx500M -classpath eigtime.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.timing.eig.Dtimee < input_files_large/DNEPTIM.in
java -Xmx500M -classpath eigtime.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.timing.eig.Dtimee < input_files_large/DSEPTIM.in
java -Xmx500M -classpath eigtime.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.timing.eig.Dtimee < input_files_large/DSVDTIM.in
