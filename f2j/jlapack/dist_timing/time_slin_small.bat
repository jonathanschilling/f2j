#!\bin\sh

java -server -Xmx500M -classpath slintime.jar;smatgen.jar;..\f2jutil.jar;..\sblas.jar;..\slapack.jar org.netlib.lapack.timing.lin.Stimaa < sband.in
java -server -Xmx500M -classpath slintime.jar;smatgen.jar;..\f2jutil.jar;..\sblas.jar;..\slapack.jar org.netlib.lapack.timing.lin.Stimaa < sblasa.in
java -server -Xmx500M -classpath slintime.jar;smatgen.jar;..\f2jutil.jar;..\sblas.jar;..\slapack.jar org.netlib.lapack.timing.lin.Stimaa < sblasb.in
java -server -Xmx500M -classpath slintime.jar;smatgen.jar;..\f2jutil.jar;..\sblas.jar;..\slapack.jar org.netlib.lapack.timing.lin.Stimaa < sblasc.in
java -server -Xmx500M -classpath slintime.jar;smatgen.jar;..\f2jutil.jar;..\sblas.jar;..\slapack.jar org.netlib.lapack.timing.lin.Stimaa < stime.in
java -server -Xmx500M -classpath slintime.jar;smatgen.jar;..\f2jutil.jar;..\sblas.jar;..\slapack.jar org.netlib.lapack.timing.lin.Stimaa < stime2.in
