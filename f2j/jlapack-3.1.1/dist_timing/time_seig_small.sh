#!/bin/sh

java -Xmx500M -classpath seigtime.jar:smatgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.timing.eig.Stimee < sgeptim.in
java -Xmx500M -classpath seigtime.jar:smatgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.timing.eig.Stimee < sneptim.in
java -Xmx500M -classpath seigtime.jar:smatgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.timing.eig.Stimee < sseptim.in
java -Xmx500M -classpath seigtime.jar:smatgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.timing.eig.Stimee < ssvdtim.in
