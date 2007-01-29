#!/bin/sh

java -server -Xmx500M -classpath seigtime.jar:smatgen.jar:../f2jutil.jar:../sblas.jar:../slapack.jar org.netlib.lapack.timing.lin.Stimee < SGEPTIM.in
java -server -Xmx500M -classpath seigtime.jar:smatgen.jar:../f2jutil.jar:../sblas.jar:../slapack.jar org.netlib.lapack.timing.lin.Stimee < SNEPTIM.in
java -server -Xmx500M -classpath seigtime.jar:smatgen.jar:../f2jutil.jar:../sblas.jar:../slapack.jar org.netlib.lapack.timing.lin.Stimee < SSEPTIM.in
java -server -Xmx500M -classpath seigtime.jar:smatgen.jar:../f2jutil.jar:../sblas.jar:../slapack.jar org.netlib.lapack.timing.lin.Stimee < SSVDTIM.in
