#!/bin/sh

java -classpath eigtest.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.testing.eig.Dchkee < dbak.in
java -classpath eigtest.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.testing.eig.Dchkee < dec.in
java -classpath eigtest.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.testing.eig.Dchkee < dgbak.in
java -classpath eigtest.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.testing.eig.Dchkee < dgg.in
java -classpath eigtest.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.testing.eig.Dchkee < dsg.in
java -classpath eigtest.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.testing.eig.Dchkee < gqr.in
java -classpath eigtest.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.testing.eig.Dchkee < lse.in
java -classpath eigtest.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.testing.eig.Dchkee < sep.in
java -classpath eigtest.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.testing.eig.Dchkee < dbal.in
java -classpath eigtest.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.testing.eig.Dchkee < ded.in
java -classpath eigtest.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.testing.eig.Dchkee < dgbal.in
java -classpath eigtest.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.testing.eig.Dchkee < dsb.in
java -classpath eigtest.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.testing.eig.Dchkee < glm.in
java -classpath eigtest.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.testing.eig.Dchkee < gsv.in
java -classpath eigtest.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.testing.eig.Dchkee < nep.in
java -classpath eigtest.jar:matgen.jar:../f2jutil.jar:../blas.jar:../lapack.jar org.netlib.lapack.testing.eig.Dchkee < svd.in

