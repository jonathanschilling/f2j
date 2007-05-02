package org.netlib.util;

import java.io.*;

/**
 * StrictMath versions of various math related Fortran intrinsic functions.
 * <p>
 * This file is part of the Fortran-to-Java (f2j) system,
 * developed at the University of Tennessee.
 * <p>
 * This class contains Strict versions of the math related utilities
 * in {@link Util}.
 * <p>
 * @author Keith Seymour (seymour@cs.utk.edu)
 *
 */

public strictfp class StrictUtil extends Util {

  /**
   * Three argument integer max function.
   * <p>
   * This function uses Java's StrictMath package.
   *
   * @param x value 1
   * @param y value 2
   * @param z value 3
   *
   * @return the largest of x, y, or z
   */
  public static int max(int x, int y, int z) {
    return StrictMath.max( x > y ? x : y, StrictMath.max(y,z));
  }

  /**
   * Three argument single precision max function.
   * <p>
   * This function uses Java's StrictMath package.
   *
   * @param x value 1
   * @param y value 2
   * @param z value 3
   *
   * @return the largest of x, y, or z
   */
  public static float max(float x, float y, float z) {
    return StrictMath.max( x > y ? x : y, StrictMath.max(y,z));
  }

  /**
   * Three argument double precision max function.
   * <p>
   * This function uses Java's StrictMath package.
   *
   * @param x value 1
   * @param y value 2
   * @param z value 3
   *
   * @return the largest of x, y, or z
   */
  public static double max(double x, double y, double z) {
    return StrictMath.max( x > y ? x : y, StrictMath.max(y,z));
  }

  /**
   * Three argument integer min function.
   * <p>
   * This function uses Java's StrictMath package.
   *
   * @param x value 1
   * @param y value 2
   * @param z value 3
   *
   * @return the smallest of x, y, or z
   */
  public static int min(int x, int y, int z) {
    return StrictMath.min( x < y ? x : y, StrictMath.min(y,z));
  }

  /**
   * Three argument single precision min function.
   * <p>
   * This function uses Java's StrictMath package.
   *
   * @param x value 1
   * @param y value 2
   * @param z value 3
   *
   * @return the smallest of x, y, or z
   */
  public static float min(float x, float y, float z) {
    return StrictMath.min( x < y ? x : y, StrictMath.min(y,z));
  }

  /**
   * Three argument double precision min function.
   * <p>
   * This function uses Java's StrictMath package.
   *
   * @param x value 1
   * @param y value 2
   * @param z value 3
   *
   * @return the smallest of x, y, or z
   */
  public static double min(double x, double y, double z) {
    return StrictMath.min( x < y ? x : y, StrictMath.min(y,z));
  }

  /**
   * Base-10 logarithm function.
   * <p>
   * This function uses Java's StrictMath package.
   *
   * @param x the value
   *
   * @return base-10 log of x
   */
  public static double log10(double x) {
    return StrictMath.log(x) / 2.30258509;
  }

  /**
   * Base-10 logarithm function.
   * <p>
   * This function uses Java's StrictMath package.
   *
   * @param x the value
   *
   * @return base-10 log of x
   */
  public static float log10(float x) {
    return (float) (StrictMath.log(x) / 2.30258509);
  }

  /**
   * Fortran nearest integer (NINT) intrinsic function.
   * <p>
   * Returns:
   * <ul>
   *   <li> (int)(x+0.5), if x &gt;= 0
   *   <li> (int)(x-0.5), if x &lt; 0
   * </ul>
   * <p>
   * This function uses Java's StrictMath package.
   *
   * @param x the floating point value
   *
   * @return the nearest integer to x
   */
  public static int nint(float x) {
    return (int) (( x >= 0 ) ? (x + 0.5) : (x - 0.5));
  }

  /**
   * Fortran nearest integer (IDNINT) intrinsic function.
   * <p>
   * Returns:<br>
   * <ul>
   *   <li> (int)(x+0.5), if x &gt;= 0
   *   <li> (int)(x-0.5), if x &lt; 0
   * </ul>
   * <p>
   * This function uses Java's StrictMath package.
   *
   * @param x the double precision floating point value
   *
   * @return the nearest integer to x
   */
  public static int idnint(double x) {
    return (int) (( x >= 0 ) ? (x + 0.5) : (x - 0.5));
  }

  /**
   * Fortran floating point transfer of sign (SIGN) intrinsic function.
   * <p>
   * Returns:<br>
   * <ul>
   *   <li> abs(a1), if a2 &gt;= 0
   *   <li>-abs(a1), if a2 &lt; 0
   * </ul>
   * <p>
   * This function uses Java's StrictMath package.
   *
   * @param a1 floating point value
   * @param a2 sign transfer indicator
   *
   * @return equivalent of Fortran SIGN(a1,a2) as described above.
   */
  public static float sign(float a1, float a2) {
    return (a2 >= 0) ? StrictMath.abs(a1) : -StrictMath.abs(a1);
  }

  /**
   * Fortran integer transfer of sign (ISIGN) intrinsic function.
   * <p>
   * Returns:<br>
   * <ul>
   *   <li> abs(a1), if a2 &gt;= 0
   *   <li>-abs(a1), if a2 &lt; 0
   * </ul>
   * <p>
   * This function uses Java's StrictMath package.
   *
   * @param a1 integer value
   * @param a2 sign transfer indicator
   *
   * @return equivalent of Fortran ISIGN(a1,a2) as described above.
   */
  public static int isign(int a1, int a2) {
    return (a2 >= 0) ? StrictMath.abs(a1) : -StrictMath.abs(a1);
  }

  /**
   * Fortran double precision transfer of sign (DSIGN) intrinsic function.
   * <p>
   * Returns:<br>
   * <ul>
   *   <li> abs(a1), if a2 &gt;= 0
   *   <li>-abs(a1), if a2 &lt; 0
   * </ul>
   * <p>
   * This function uses Java's StrictMath package.
   *
   * @param a1 double precision floating point value
   * @param a2 sign transfer indicator
   *
   * @return equivalent of Fortran DSIGN(a1,a2) as described above.
   */
  public static double dsign(double a1, double a2) {
    return (a2 >= 0) ? StrictMath.abs(a1) : -StrictMath.abs(a1);
  }

  /**
   * Fortran floating point positive difference (DIM) intrinsic function.
   * <p>
   * Returns:<br>
   * <ul>
   *   <li> a1 - a2, if a1 &gt; a2
   *   <li> 0, if a1 &lt;= a2
   * </ul>
   * <p>
   * This function uses Java's StrictMath package.
   *
   * @param a1 floating point value
   * @param a2 floating point value
   *
   * @return equivalent of Fortran DIM(a1,a2) as described above.
   */
  public static float dim(float a1, float a2) {
    return (a1 > a2) ? (a1 - a2) : 0;
  }

  /**
   * Fortran integer positive difference (IDIM) intrinsic function.
   * <p>
   * Returns:<br>
   * <ul>
   *   <li> a1 - a2, if a1 &gt; a2
   *   <li> 0, if a1 &lt;= a2
   * </ul>
   * <p>
   * This function uses Java's StrictMath package.
   *
   * @param a1 integer value
   * @param a2 integer value
   *
   * @return equivalent of Fortran IDIM(a1,a2) as described above.
   */
  public static int idim(int a1, int a2) {
    return (a1 > a2) ? (a1 - a2) : 0;
  }

  /**
   * Fortran double precision positive difference (DDIM) intrinsic function.
   * <p>
   * Returns:<br>
   * <ul>
   *   <li> a1 - a2, if a1 &gt; a2
   *   <li> 0, if a1 &lt;= a2
   * </ul>
   * <p>
   * This function uses Java's StrictMath package.
   *
   * @param a1 double precision floating point value
   * @param a2 double precision floating point value
   *
   * @return equivalent of Fortran DDIM(a1,a2) as described above.
   */
  public static double ddim(double a1, double a2) {
    return (a1 > a2) ? (a1 - a2) : 0;
  }

  /**
   * Fortran hyperbolic sine (SINH) intrinsic function.
   * <p>
   * This function uses Java's StrictMath package.
   *
   * @param a the value to get the sine of
   *
   * @return the hyperbolic sine of a
   */
  public static double sinh(double a) {
    return ( StrictMath.exp(a) - StrictMath.exp(-a) ) * 0.5;
  }

  /**
   * Fortran hyperbolic cosine (COSH) intrinsic function.
   * <p>
   * This function uses Java's StrictMath package.
   *
   * @param a the value to get the cosine of
   *
   * @return the hyperbolic cosine of a
   */
  public static double cosh(double a) {
    return ( StrictMath.exp(a) + StrictMath.exp(-a) ) * 0.5;
  }

  /**
   * Fortran hyperbolic tangent (TANH) intrinsic function.
   * <p>
   * This function uses Java's StrictMath package.
   *
   * @param a the value to get the tangent of
   *
   * @return the hyperbolic tangent of a
   */
  public static double tanh(double a) {
    return sinh(a) / cosh(a);
  }
}
