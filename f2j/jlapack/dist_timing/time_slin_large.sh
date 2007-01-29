#!/bin/sh

java -server -Xmx500M -classpath slintime.jar:smatgen.jar:../f2jutil.jar:../sblas.jar:../slapack.jar org.netlib.lapack.timing.lin.Stimaa < SBAND.in
java -server -Xmx500M -classpath slintime.jar:smatgen.jar:../f2jutil.jar:../sblas.jar:../slapack.jar org.netlib.lapack.timing.lin.Stimaa < SBLASA.in
java -server -Xmx500M -classpath slintime.jar:smatgen.jar:../f2jutil.jar:../sblas.jar:../slapack.jar org.netlib.lapack.timing.lin.Stimaa < SBLASB.in
java -server -Xmx500M -classpath slintime.jar:smatgen.jar:../f2jutil.jar:../sblas.jar:../slapack.jar org.netlib.lapack.timing.lin.Stimaa < SBLASC.in
java -server -Xmx500M -classpath slintime.jar:smatgen.jar:../f2jutil.jar:../sblas.jar:../slapack.jar org.netlib.lapack.timing.lin.Stimaa < STIME.in
java -server -Xmx500M -classpath slintime.jar:smatgen.jar:../f2jutil.jar:../sblas.jar:../slapack.jar org.netlib.lapack.timing.lin.Stimaa < STIME2.in
