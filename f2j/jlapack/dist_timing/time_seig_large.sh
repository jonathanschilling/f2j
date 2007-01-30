#!/bin/sh

java -Xmx500M -classpath seigtime.jar:smatgen.jar:../f2jutil.jar:../sblas.jar:../slapack.jar org.netlib.lapack.timing.eig.Stimee < SGEPTIM.in
java -Xmx500M -classpath seigtime.jar:smatgen.jar:../f2jutil.jar:../sblas.jar:../slapack.jar org.netlib.lapack.timing.eig.Stimee < SNEPTIM.in
java -Xmx500M -classpath seigtime.jar:smatgen.jar:../f2jutil.jar:../sblas.jar:../slapack.jar org.netlib.lapack.timing.eig.Stimee < SSEPTIM.in
java -Xmx500M -classpath seigtime.jar:smatgen.jar:../f2jutil.jar:../sblas.jar:../slapack.jar org.netlib.lapack.timing.eig.Stimee < SSVDTIM.in
