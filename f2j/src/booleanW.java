/*
 * This file is part of the Fortran-to-Java (f2java) system,
 * developed at the University of Tennessee.
 *
 * This class acts as an object wrapper for passing boolean
 * values by reference in f2java translated files.
 *
 * Keith Seymour (seymour@cs.utk.edu)
 *
 */

public class booleanW {
  public boolean val;

  public booleanW(boolean x) {
     val = x;
  }
}
