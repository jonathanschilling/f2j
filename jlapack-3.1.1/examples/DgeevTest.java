/**
 * DgeevTest - example of calling the Java version of Dgeev.
 *
 * This was adapted from the NAG DGEEV Example (NAG Copyright 2005).
 *    http://www.nag.co.uk/lapack-ex/node87.html
 *
 * To compile and run:
 *
 * bambam> javac -classpath .:f2jutil.jar:blas.jar:lapack.jar DgeevTest.java
 * bambam> java -classpath .:f2jutil.jar:blas.jar:lapack.jar DgeevTest
 *
 * DGEEV Example Program Results
 * Eigenvalue(1) = 0.7994821225862091
 * 
 * Eigenvector(1)
 *    -0.6550887675124073
 *    -0.5236294609021241
 *    0.5362184613722346
 *    -0.09560677820122966
 * 
 * Eigenvalue(2) = (-0.0994124532950747, 0.4007924719897547)
 * 
 * Eigenvector(2)
 *    (-0.19330154826422166, 0.25463157192758457)
 *    (0.25185653172674005, -0.522404734711629)
 *    (0.0971824584432815, -0.30838375589722833)
 *    (0.675954054254748, 0.0)
 * 
 * Eigenvalue(3) = (-0.0994124532950747, -0.4007924719897547)
 * 
 * Eigenvector(3)
 *    (-0.19330154826422166, -0.25463157192758457)
 *    (0.25185653172674005, 0.522404734711629)
 *    (0.0971824584432815, 0.30838375589722833)
 *    (0.675954054254748, -0.0)
 * 
 * Eigenvalue(4) = -0.10065721599605859
 * 
 * Eigenvector(4)
 *    0.12533269723090262
 *    0.33202221557175127
 *    0.5938377595573311
 *    0.722087029862455
 *
 **/

import java.lang.*;
import org.netlib.util.*;

public class DgeevTest
{

  public static double[] a = {
    0.35e0, 0.09e0, -0.44e0, 0.25e0,
    0.45e0, 0.07e0, -0.33e0, -0.32e0,
    -0.14e0, -0.54e0, -0.03e0, -0.13e0,
    -0.17e0, 0.35e0, 0.17e0, 0.11e0
  };

  public static void main(String[]args)
  {
    int i, j, lwkopt;
    intW info = new intW(0);
    double[] dummy = new double[1];
    double[] vr = new double[4 * 4];
    double[] wi = new double[4];
    double[] work = new double[264];
    double[] wr = new double[4];

    System.out.println("DGEEV Example Program Results");

    // Compute the eigenvalues and right eigenvectors of A

    org.netlib.lapack.Dgeev.dgeev("No left vectors", "Vectors (right)", 
       4, a, 0, 4, wr, 0, wi, 0, dummy, 0, 1, vr, 0, 4, work, 0, 264, info);

    lwkopt = (int) work[0];

    if(info.val == 0) {
      for(j = 0; j < 4; j++) {
        if((wi[j] == 0.0e0))
          System.out.println("Eigenvalue(" + (j+1) + ") = " + wr[j]);
        else
          System.out.println("Eigenvalue(" + (j+1) + ") = (" + wr[j] 
             + ", " + wi[j] + ")");

        System.out.println();

        System.out.println("Eigenvector(" + (j+1) + ")");

        if(wi[j] == 0.0e0)
          for(i = 0; i < 4; i++)
            System.out.println("   " + vr[i + (j * 4)]);
        else if(wi[j] > 0.0e0)
          for(i = 0; i < 4; i++)
            System.out.println("   (" + vr[i + (j * 4)] 
               + ", " + vr[i + ((j + 1) * 4)] + ")");
        else
          for(i = 0; i < 4; i++)
            System.out.println("   (" + vr[i + ((j - 1) * 4)]
               + ", " + -(vr[i + (j * 4)]) + ")");

        System.out.println();
      }
    }
    else
      System.out.println("Failure in DGEEV.  INFO = " + info.val);

    return;
  }
}
