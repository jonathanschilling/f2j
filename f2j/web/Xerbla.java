/*
*  Produced by f2java.  f2java is part of the Fortran-
*  -to-Java project at the University of Tennessee Netlib
*  numerical software repository.
*
*  Original authorship for the BLAS and LAPACK numerical
*  routines may be found in the Fortran source, available at
*  www.netlib.org.
*
*  Fortran input file: xerbla.f
*
*  The f2j compiler code was written by
*  David M. Doolin (doolin@cs.utk.edu) and
*  Keith  Seymour (seymour@cs.utk.edu)
*/
package org.netlib.util;

// package lapack;
// import blas.*;
public class Xerbla 
{


  // $AUTO: Class methods.

  // Static variables (fortran SAVE stmt)
  public static void xerbla(java.lang.String srname, int info) 
  {
    // Type declarations.
    // Executable code.
    System.out.println(" ** On entry to " + srname + " " + " parameter number " + info + " " + " had " + "an illegal value"); 
    System.exit(1); 
  }

// End class.
}


