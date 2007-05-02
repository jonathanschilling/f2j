package org.netlib.util;

/**
 * f2j object wrapper for strings.
 * <p>
 * This file is part of the Fortran-to-Java (f2j) system,
 * developed at the University of Tennessee.
 * <p>
 * This class acts as an object wrapper for passing string
 * values by reference in f2j translated files.
 * <p>
 * @author Keith Seymour (seymour@cs.utk.edu)
 *
 */

public class StringW {
 public String val;

  /**
   * Create a new string wrapper.
   *
   * @param x the initial value
   */
 public StringW(String x) {
   val = x;
 }
}
