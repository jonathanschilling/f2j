/**
 * This file is part of the Fortran-to-Java (f2java) system,
 * developed at the University of Tennessee.
 *
 * This class acts as an object wrapper for passing double
 * precision floating point values by reference in f2java 
 * translated files.
 *
 * @author Keith Seymour (seymour@cs.utk.edu)
 *
 */

package org.netlib.util;

public class doubleW {
  public double val;

  /**
   * Create a new double wrapper.
   *
   * @param x -- the initial value
   */
  public doubleW(double x) {
     val = x;
  }
}
