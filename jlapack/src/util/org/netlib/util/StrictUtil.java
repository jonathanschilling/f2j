package org.netlib.util;

import java.io.*;

/**
 * Strict versions of the math related utilities.
 **/

public strictfp class StrictUtil extends Util {
  public static int max(int x, int y, int z) {
    return StrictMath.max( x > y ? x : y, StrictMath.max(y,z));
  }

  public static float max(float x, float y, float z) {
    return StrictMath.max( x > y ? x : y, StrictMath.max(y,z));
  }

  public static double max(double x, double y, double z) {
    return StrictMath.max( x > y ? x : y, StrictMath.max(y,z));
  }

  public static int min(int x, int y, int z) {
    return StrictMath.min( x < y ? x : y, StrictMath.min(y,z));
  }

  public static float min(float x, float y, float z) {
    return StrictMath.min( x < y ? x : y, StrictMath.min(y,z));
  }

  public static double min(double x, double y, double z) {
    return StrictMath.min( x < y ? x : y, StrictMath.min(y,z));
  }

  public static double log10(double x) {
    return StrictMath.log(x) / 2.30258509;
  }

  public static float log10(float x) {
    return (float) (StrictMath.log(x) / 2.30258509);
  }

  public static int nint(float x) {
    return (int) (( x >= 0 ) ? (x + 0.5) : (x - 0.5));
  }

  public static int idnint(double x) {
    return (int) (( x >= 0 ) ? (x + 0.5) : (x - 0.5));
  }

  public static float sign(float a1, float a2) {
    return (a2 >= 0) ? StrictMath.abs(a1) : -StrictMath.abs(a1);
  }

  public static int isign(int a1, int a2) {
    return (a2 >= 0) ? StrictMath.abs(a1) : -StrictMath.abs(a1);
  }

  public static double dsign(double a1, double a2) {
    return (a2 >= 0) ? StrictMath.abs(a1) : -StrictMath.abs(a1);
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
    return ( StrictMath.exp(a) - StrictMath.exp(-a) ) * 0.5;
  }

  public static double cosh(double a) {
    return ( StrictMath.exp(a) + StrictMath.exp(-a) ) * 0.5;
  }

  public static double tanh(double a) {
    return sinh(a) / cosh(a);
  }
}
