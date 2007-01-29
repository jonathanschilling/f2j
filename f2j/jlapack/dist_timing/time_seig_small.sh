#!/bin/sh

java -server -Xmx500M -classpath seigtime.jar:smatgen.jar:../f2jutil.jar:../sblas.jar:../slapack.jar org.netlib.lapack.timing.lin.Stimee < sgeptim.in
java -server -Xmx500M -classpath seigtime.jar:smatgen.jar:../f2jutil.jar:../sblas.jar:../slapack.jar org.netlib.lapack.timing.lin.Stimee < sneptim.in
java -server -Xmx500M -classpath seigtime.jar:smatgen.jar:../f2jutil.jar:../sblas.jar:../slapack.jar org.netlib.lapack.timing.lin.Stimee < sseptim.in
java -server -Xmx500M -classpath seigtime.jar:smatgen.jar:../f2jutil.jar:../sblas.jar:../slapack.jar org.netlib.lapack.timing.lin.Stimee < ssvdtim.in
