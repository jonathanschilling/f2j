package org.netlib.util;

import java.lang. *;


public class LAprint
{


/*  Just a bunch of methods to print out blas and lapack stuff. */

  public static void scalarPrint (String methodname, double val)
  {
    String indent = new String ("         ");
      System.out.println ("Called " + methodname + ", returned:");
      System.out.println (indent + val);
  }				/* Close method scalarPrint().  */


  public static void vectorPrint (String methodname, double[]vector)
  {
    String indent = new String ("         ");
      System.out.println ("Called " + methodname + ", returned:");
      System.out.print (indent + "[ ");
    for (int i = 0; i < 2; i++)
      {
	System.out.print (vector[i] + " ");
      }
    System.out.println (" ]");
  }				/*  Close method vectorPrint().  */


/* I will eventually have to passin the m, n size of the matrices.  */
  public static void matrixPrint (String methodname, double[]matrix, int LD)
  {
    String indent = new String ("         ");
      System.out.println ("Called " + methodname + ", returned:");

    for (int i = 0; i < 2; i++)
      {
	System.out.print ("[ ");
	for (int j = 0; j < 2; j++)
	  {

	    System.out.print (matrix[i + j * LD] + " ");
	  }
	System.out.println ("]");
      }

  }				/*  Close method matrixPrint().  */

  public static void matrixPrint (String methodname, double[][]matrix)
  {
    String indent = new String ("         ");
      System.out.println ("Called " + methodname + ", returned:");
/* Note how idiotic this routine is:  I just assume the matrix is
   2X2.  This will need to be changed.  */
    for (int i = 0; i < 2; i++)
      {
	System.out.print ("[ ");
	for (int j = 0; j < 2; j++)
	  {

	    System.out.print (matrix[i][j] + " ");
	  }
	System.out.println ("]");
      }

  }				/*  Close method matrixPrint().  */




}
