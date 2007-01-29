#!\bin\sh

java -server -Xmx500M -classpath lintime.jar;matgen.jar;..\f2jutil.jar;..\blas.jar;..\lapack.jar org.netlib.lapack.timing.lin.Dtimaa < dband.in
java -server -Xmx500M -classpath lintime.jar;matgen.jar;..\f2jutil.jar;..\blas.jar;..\lapack.jar org.netlib.lapack.timing.lin.Dtimaa < dblasa.in
java -server -Xmx500M -classpath lintime.jar;matgen.jar;..\f2jutil.jar;..\blas.jar;..\lapack.jar org.netlib.lapack.timing.lin.Dtimaa < dblasb.in
java -server -Xmx500M -classpath lintime.jar;matgen.jar;..\f2jutil.jar;..\blas.jar;..\lapack.jar org.netlib.lapack.timing.lin.Dtimaa < dblasc.in
java -server -Xmx500M -classpath lintime.jar;matgen.jar;..\f2jutil.jar;..\blas.jar;..\lapack.jar org.netlib.lapack.timing.lin.Dtimaa < dtime.in
java -server -Xmx500M -classpath lintime.jar;matgen.jar;..\f2jutil.jar;..\blas.jar;..\lapack.jar org.netlib.lapack.timing.lin.Dtimaa < dtime2.in
