/*
 * This file is part of the Fortran-to-Java (f2java) system,
 * developed at the University of Tennessee.
 *
 * This class aids in the translation of goto statements.
 * The code generator translates gotos and labels into calls
 * to Dummy.go_to() or Dummy.label().  These calls act as
 * 'placeholders' so that the gotos and labels can be found
 * in the disassembled class file and converted to jasmin
 * gotos and jasmin labels.  Thus the resulting jasmin source
 * code should contain no calls to Dummy.go_to() or label().
 * If so, the print statements should warn the user that the
 * goto translation was not successful.
 *
 * Keith Seymour (seymour@cs.utk.edu)
 *
 */

package org.netlib.util;

public class Dummy {
  public static void go_to(String clname, int lbl) {
    System.err.println("Warning: Untransformed goto remaining in program! ("
      +clname+", " + lbl + ")");
  }

  public static void label(String clname, int lbl) {
    System.err.println("Warning: Untransformed label remaining in program! ("
      +clname+", " + lbl + ")");
  }
}
