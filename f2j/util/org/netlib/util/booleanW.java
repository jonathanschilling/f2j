package org.netlib.util;

/**
 * This file is part of the Fortran-to-Java (f2j) system,
 * developed at the University of Tennessee.
 * <p>
 * This class acts as an object wrapper for passing boolean
 * values by reference in f2j translated files.
 * <p>
 * @author Keith Seymour (seymour@cs.utk.edu)
 *
 */

public class booleanW {
  public boolean val;

  /**
   * Create a new boolean wrapper.
   *
   * @param x the initial value
   */
  public booleanW(boolean x) {
     val = x;
  }
}
