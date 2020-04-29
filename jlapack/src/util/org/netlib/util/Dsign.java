/*
 * This file is part of the Fortran-to-Java (f2java) system,
 * developed at the University of Tennessee.
 *
 * This class implements the Fortran 77 DSIGN intrinsic.
 *
 * Keith Seymour (seymour@cs.utk.edu)
 *
 */

package org.netlib.util;

public class Dsign {

  public static double dsign(double a, double b) {
    if(b > 0)
      return( Math.abs(a) );
    else if (b < 0)
      return( - Math.abs(a) );
    else
      return( 0 );
  }
}
