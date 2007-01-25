/**
 * This file is part of the Fortran-to-Java (f2java) system,
 * developed at the University of Tennessee.
 *
 * This class acts as an object wrapper for passing string
 * values by reference in f2java translated files.
 *
 * @author Keith Seymour (seymour@cs.utk.edu)
 *
 */

package org.netlib.util;

public class StringW {
 public String val;

  /**
   * Create a new string wrapper.
   *
   * @param x -- the initial value
   */
 public StringW(String x) {
   val = x;
 }
}
