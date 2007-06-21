#!/bin/csh

setenv CPTMP $CLASSPATH":../../blas/blas.jar:../../error_reporting/xerbla.jar"
cd obj
foreach file(org/netlib/lapack/*.class)
  java -classpath $CPTMP -Xmx500M org.apache.bcel.verifier.Verifier $file
end
