/*
 * This file is part of the Fortran-to-Java (f2java) system,
 * developed at the University of Tennessee.
 *
 * This class acts as an object wrapper for passing single
 * precision floating point values by reference in f2java 
 * translated files.
 *
 * Keith Seymour (seymour@cs.utk.edu)
 *
 */

package org.netlib.util;

public class floatW {
  public float val;

  public floatW(float x) {
     val = x;
  }
}
