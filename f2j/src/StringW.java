/*
 * This file is part of the Fortran-to-Java (f2java) system,
 * developed at the University of Tennessee.
 *
 * This class acts as an object wrapper for passing string
 * values by reference in f2java translated files.
 *
 * Keith Seymour (seymour@cs.utk.edu)
 *
 */

public class StringW {
 public String val;

 public StringW(String x) {
   val = x;
 }
}
