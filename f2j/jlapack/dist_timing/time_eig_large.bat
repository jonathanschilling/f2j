#!\bin\sh

java -server -Xmx500M -classpath eigtime.jar;matgen.jar;..\f2jutil.jar;..\blas.jar;..\lapack.jar org.netlib.lapack.timing.lin.Dtimee < DGEPTIM.in
java -server -Xmx500M -classpath eigtime.jar;matgen.jar;..\f2jutil.jar;..\blas.jar;..\lapack.jar org.netlib.lapack.timing.lin.Dtimee < DNEPTIM.in
java -server -Xmx500M -classpath eigtime.jar;matgen.jar;..\f2jutil.jar;..\blas.jar;..\lapack.jar org.netlib.lapack.timing.lin.Dtimee < DSEPTIM.in
java -server -Xmx500M -classpath eigtime.jar;matgen.jar;..\f2jutil.jar;..\blas.jar;..\lapack.jar org.netlib.lapack.timing.lin.Dtimee < DSVDTIM.in
