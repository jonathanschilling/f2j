/**
 * DsyevTest - example of calling the Java version of Dsyev.
 *
 * This was adapted from the NAG DGEEV Example (NAG Copyright 2005).
 *    http://www.nag.co.uk/lapack-ex/node71.html
 *
 * To compile and run:
 *
 * > javac -classpath .:f2jutil.jar:blas.jar:lapack.jar DsyevTest.java
 * > java -classpath .:f2jutil.jar:blas.jar:lapack.jar DsyevTest
 *
 * 
 * DSYEV Example Program Results
 * 
 * Eigenvalues
 *   -2.053115763536997  -0.5146427793906134  -0.2943264517738015  12.862084994701412
 * Eigenvectors
 *   0.7003490260882729  -0.5143735757321091  0.2766775881140148  0.41034202621859706
 *   0.35923381060634124  0.4851033358864254  -0.6633588457388608  0.44224525391361225
 *   -0.1568514757241387  0.5419782033048344  0.6504247278898553  0.5085321180171721
 *   -0.5965399613920387  -0.454262264823846  -0.24566690327519744  0.6143562825060633
 * 
 * Error estimate for the eigenvalues
 * 
 * 1.4279782905803242E-15
 * 
 * Error estimates for the eigenvectors
 *   9.281789835085293E-16  6.481490981748544E-15  6.481490981748544E-15  1.0853858564623254E-16
 *
 **/

import java.lang.*;
import org.netlib.util.*;

public class DsyevTest {

  public static void main (String [] args)  {
    double eerrbd, eps;
    int i, j;
    intW info = new intW(0);
    double [] rcondz = new double[4];
    double [] w = new double[4];
    double [] work = new double[264];
    double [] zerrbd = new double[4];
    double [] a = {
        1 , 2 , 3 , 4 , 2 , 2 , 3 , 4 , 3 , 3 , 3 , 4 , 4 , 4 , 4 , 4 };

    System.out.println("DSYEV Example Program Results");
    System.out.println("");

    org.netlib.lapack.Dsyev.dsyev("Vectors", "Upper", 4, a, 0, 4, w, 0, work,
       0, 264, info);

    if(info.val == 0) {
      System.out.println("Eigenvalues");
      for(j = 0; j < 4; j++)
        System.out.print("  " + w[j]);
      System.out.println("");

      System.out.println("Eigenvectors");
      for (i = 0; i < 4; i++) {
        for(j = 0; j < 4; j++)
          System.out.print("  " + a[i+(j*4)]);
        System.out.println("");
      }
      eps = org.netlib.lapack.Dlamch.dlamch("Eps");
      eerrbd = eps*Math.max(Math.abs(w[0]), Math.abs(w[3]));

      org.netlib.lapack.Ddisna.ddisna("Eigenvectors", 4, 4, w, 0, rcondz,
        0, info);

      for (i = 0; i < 4; i++)
        zerrbd[i] = eerrbd/rcondz[i];

      System.out.println("");
      System.out.println("Error estimate for the eigenvalues");
      System.out.println("");
      System.out.println(eerrbd);
      System.out.println("");
      System.out.println("Error estimates for the eigenvectors");

      for(i = 0; i < 4; i++)
        System.out.print("  " + zerrbd[i]);
      System.out.println("");
    }
    else
      System.out.println("Failure in DSYEV. INFO = " + info.val);
  }
}
