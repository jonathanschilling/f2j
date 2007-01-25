/**
 * This file is part of the Fortran-to-Java (f2java) system,
 * developed at the University of Tennessee.
 *
 * This class contains various helper routines for f2j-generated code.
 * These routines are primarily implemented for handling Fortran intrinsic
 * functions.
 *
 * @author Keith Seymour (seymour@cs.utk.edu)
 *
 */

package org.netlib.util;

import java.io.*;

public class Util {

  /**
   * This method handles situations in which the lhs of an
   * assignment statement is a substring operation.  For example:
   *   a(3:4) = 'hi'
   * We haven't figured out an elegant way to do this with Java Strings,
   * but we do handle it, as follows:
   *
   *  int E1, - E1 is the expression representing the start of the substring
   *      E2; - E2 is the expression representing the end of the substring
   *
   *  a = new StringW(
   *        a.val.substring(0,E1-1) +
   *        "hi".substring(0,E2-E1+1) +
   *        a.val.substring(E2,a.val.length())
   *      );
   *
   * The resulting code looks pretty bad because we have to be
   * prepared to handle rhs strings that are too big to fit in
   * the lhs substring.
   * 
   * @param x -- dest (string to be inserted into)
   * @param y -- source (substring to insert into 'x')
   * @param E1 -- expression representing the start of the substring
   * @param E2 -- expression representing the end of the substring
   *
   * @return the string containing the complete string after inserting the
   *    substring
   */

  public static String stringInsert(String x, String y, int E1, int E2) {
    String tmp;
  
    tmp = new String(
           x.substring(0,E1-1) +
           y.substring(0,E2-E1+1) +
           x.substring(E2,x.length()));
    return tmp;
  }

  /**
   * Three argument max function.
   */
  public static int max(int x, int y, int z) {
    return Math.max( x > y ? x : y, Math.max(y,z));
  }

  /**
   * Three argument max function.
   */
  public static float max(float x, float y, float z) {
    return Math.max( x > y ? x : y, Math.max(y,z));
  }

  /**
   * Three argument max function.
   */
  public static double max(double x, double y, double z) {
    return Math.max( x > y ? x : y, Math.max(y,z));
  }

  /**
   * Three argument min function.
   */
  public static int min(int x, int y, int z) {
    return Math.min( x < y ? x : y, Math.min(y,z));
  }

  /**
   * Three argument min function.
   */
  public static float min(float x, float y, float z) {
    return Math.min( x < y ? x : y, Math.min(y,z));
  }

  /**
   * Three argument min function.
   */
  public static double min(double x, double y, double z) {
    return Math.min( x < y ? x : y, Math.min(y,z));
  }

  /**
   * Base-10 logarithm function.
   */
  public static double log10(double x) {
    return Math.log(x) / 2.30258509;
  }

  /**
   * Base-10 logarithm function.
   */
  public static float log10(float x) {
    return (float) (Math.log(x) / 2.30258509);
  }

  public static int nint(float x) {
    return (int) (( x >= 0 ) ? (x + 0.5) : (x - 0.5));
  }

  public static int idnint(double x) {
    return (int) (( x >= 0 ) ? (x + 0.5) : (x - 0.5));
  }

  public static float sign(float a1, float a2) {
    return (a2 >= 0) ? Math.abs(a1) : -Math.abs(a1);
  }

  public static int isign(int a1, int a2) {
    return (a2 >= 0) ? Math.abs(a1) : -Math.abs(a1);
  }

  /**
   * Returns the equivalent of the Fortran 77 DSIGN intrinsic:
   *   abs(a1) if a2 &gt;= 0
   *  -abs(a1) if a2 &lt; 0
   *   
   * @param a1 -- value
   * @param a2 -- sign transfer indicator
   *
   * @return equivalent of Fortran DSIGN(a1,a2) as described above.
   */
  public static double dsign(double a1, double a2) {
    return (a2 >= 0) ? Math.abs(a1) : -Math.abs(a1);
  }

  public static float dim(float a1, float a2) {
    return (a1 > a2) ? (a1 - a2) : 0;
  }

  public static int idim(int a1, int a2) {
    return (a1 > a2) ? (a1 - a2) : 0;
  }

  public static double ddim(double a1, double a2) {
    return (a1 > a2) ? (a1 - a2) : 0;
  }

  public static double sinh(double a) {
    return ( Math.exp(a) - Math.exp(-a) ) * 0.5;
  }

  public static double cosh(double a) {
    return ( Math.exp(a) + Math.exp(-a) ) * 0.5;
  }

  public static double tanh(double a) {
    return sinh(a) / cosh(a);
  }

  public static void pause() {
    pause(null);
  }

  public static void pause(String msg) {
    if(msg != null)
      System.err.println("PAUSE: " + msg);
    else
      System.err.print("PAUSE: ");

    System.err.println("To resume execution, type:   go");
    System.err.println("Any other input will terminate the program.");

    BufferedReader in = new BufferedReader(new InputStreamReader(System.in));

    String response = null;

    try {
      response = in.readLine();  
    } catch (IOException e) {
      response = null;
    }

    if( (response == null) ||  !response.equals("go")) {
      System.err.println("STOP");
      System.exit(0);
    }
  }
}
