/**
 * SsptrfTest - example of calling the Java version of Ssptrf
 *
 * This was adapted from the NAG F07PDF Example (NAG Copyright 1991).
 *
 * To compile and run:
 *
 * localhost> javac -classpath .:f2jutil.jar:blas.jar:lapack.jar SsptrfTest.java
 * localhost> java -classpath .:f2jutil.jar:blas.jar:lapack.jar SsptrfTest
 * Results:
 *    2.07
 *    4.2   1.15
 *    0.22304142   0.8115011   -2.5906773
 *    0.6536584   -0.59596974   0.30308464   0.40738514
 * IPIV
 *    -3   -3   3   4
 * 
 */

import java.lang.*;
import org.netlib.util.*;
import org.netlib.lapack.Ssptrf;

public class SsptrfTest {

  public static float [] ap = { 2.07f , 3.87f , 4.20f , -1.15f, 
     -0.21f , 1.87f , 0.63f , 1.15f , 2.06f, -1.81f };

  public static void main (String [] args)  {
    int i = 0;
    int ifail = 0;
    intW info = new intW(0);
    int j = 0;
    int N = 4;
    String uplo;
    int [] ipiv= new int[N];

    System.out.println("Results:");
    uplo = "L";

    Ssptrf.ssptrf(uplo,N,ap,0,ipiv,0,info);
    ifail = 0;

    for (i = 1; i <= N; i++) {
      for(j = 1; j <= i; j++)
        System.out.print( "   " + ap[((i+(((((2*N)-j))*((j-1)))/2))-(1))]);
      System.out.println();
    }

    System.out.println("IPIV");

    for(i = 1; i <= N; i++)
      System.out.print("   " + ipiv[(i-(1))]);
    System.out.println();

    if ((info.val != 0))
      System.out.println("The factor D is singular");
  }
}
