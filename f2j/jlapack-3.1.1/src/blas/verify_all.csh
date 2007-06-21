#!/bin/csh

setenv CPTMP $CLASSPATH":../../error_reporting/xerbla.jar"
cd obj
foreach file(org/netlib/blas/*.class)
  java -classpath $CPTMP -Xmx500M org.apache.bcel.verifier.Verifier $file
end
