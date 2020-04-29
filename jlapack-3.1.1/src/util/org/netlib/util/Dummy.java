package org.netlib.util;

/**
 * Placeholders for Fortran GOTO statements and labels.
 * <p>
 * This file is part of the Fortran-to-Java (f2j) system,
 * developed at the University of Tennessee.
 * <p>
 * This class aids in the translation of goto statements.
 * The code generator translates gotos and labels into calls
 * to Dummy.go_to() or Dummy.label().  These calls act as
 * 'placeholders' so that the gotos and labels can be found
 * in the class file and converted to real branch
 * instructions in the bytecode.  Thus the resulting class
 * file should contain no calls to Dummy.go_to() or Dummy.label().
 * If so, the print statements should warn the user that the
 * goto translation was not successful.
 * <p>
 * @author Keith Seymour (seymour@cs.utk.edu)
 *
 */

public class Dummy {

  /**
   * Placeholder for a Fortran GOTO statement.
   *
   * @param clname name of the program unit where this GOTO exists
   * @param lbl the label number (target) of the GOTO
   */
  public static void go_to(String clname, int lbl) {
    System.err.println("Warning: Untransformed goto remaining in program! ("
      +clname+", " + lbl + ")");
  }

  /**
   * Placeholder for a Fortran label.
   *
   * @param clname name of the program unit where this label exists
   * @param lbl the label number
   */
  public static void label(String clname, int lbl) {
    System.err.println("Warning: Untransformed label remaining in program! ("
      +clname+", " + lbl + ")");
  }
}
