package org.netlib.util;

/**
 * This file is part of the Fortran-to-Java (f2j) system,
 * developed at the University of Tennessee.
 * <p>
 * This class acts as an object wrapper for passing single
 * precision floating point values by reference in f2j 
 * translated files.
 * <p>
 * @author Keith Seymour (seymour@cs.utk.edu)
 *
 */

public class floatW {
  public float val;

  /**
   * Create a new float wrapper.
   *
   * @param x the initial value
   */
  public floatW(float x) {
     val = x;
  }
}
