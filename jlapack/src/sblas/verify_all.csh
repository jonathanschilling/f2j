#!/bin/csh

setenv CPTMP $CLASSPATH":../../error_reporting/xerbla.jar"
cd obj
foreach file(org/netlib/blas/*.class)
  java -classpath $CPTMP de.fub.bytecode.verifier.Verifier $file
end
