/**
 * This file is part of the Fortran-to-Java (f2java) system,
 * developed at the University of Tennessee.
 *
 * This class acts as an object wrapper for passing integer
 * values by reference in f2java translated files.
 *
 * @author Keith Seymour (seymour@cs.utk.edu)
 *
 */

package org.netlib.util;

public class intW {
  public int val;

  /**
   * Create a new int wrapper.
   *
   * @param x -- the initial value
   */
  public intW(int x) {
     val = x;
  }
}
