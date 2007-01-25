/**
 * This file is part of the Fortran-to-Java (f2java) system,
 * developed at the University of Tennessee.
 *
 * This class acts as an object wrapper for passing single
 * precision floating point values by reference in f2java 
 * translated files.
 *
 * @author Keith Seymour (seymour@cs.utk.edu)
 *
 */

package org.netlib.util;

public class floatW {
  public float val;

  /**
   * Create a new float wrapper.
   *
   * @param x -- the initial value
   */
  public floatW(float x) {
     val = x;
  }
}
