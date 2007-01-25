package org.netlib.util;

/**
 * This file is part of the Fortran-to-Java (f2j) system,
 * developed at the University of Tennessee.
 * <p>
 * This class implements the Fortran 77 SECOND intrinsic.
 * SECOND is supposed to provide the CPU time for the
 * process since the start of execution.  Currently,
 * Java doesn't have a similar method, so we use this
 * cheesy simulation:  <br>
 * <ul>
 *   <li> f2j inserts a call at the beginning of the program
 *         to record the start time.
 *   <li> on the first call, record the current time.
 *   <li> on subsequent calls, return the difference 
 *         between the current call time and the starting
 *         time.
 * </ul>
 * Essentially, this version of etime returns the
 * wall-clock time elapsed since the beginning of 
 * execution.
 * <p>
 * @author Keith Seymour (seymour@cs.utk.edu)
 *
 */

public class Second {

  /**
   * Supposed to return the elapsed CPU time since the beginning of 
   * program execution.  Currently implemented as wall clock time.
   *
   * @return the elapsed time.
   */
  public static float second()
  {
    float [] tarray= new float[2];

    Etime.etime();
    Etime.etime(tarray,0);

    return tarray[0];
  }
}
