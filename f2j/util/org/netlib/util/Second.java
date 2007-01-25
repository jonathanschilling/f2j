/**
 * This file is part of the Fortran-to-Java (f2java) system,
 * developed at the University of Tennessee.
 *
 * This class implements the Fortran 77 SECOND intrinsic.
 * SECOND is supposed to provide the CPU time for the
 * process since the start of execution.  Currently,
 * Java doesn't have a similar method, so we use this
 * cheesy simulation:  
 *   - insert a call to Etime.etime() at the beginning 
 *       of the program.  
 *   - on the first call, record the current time
 *   - on subsequent calls, return the difference 
 *       between the current call and the starting
 *       time.
 * Essentially, this version of etime returns the
 * wall-clock time elapsed since the beginning of 
 * execution.
 *
 * @author Keith Seymour (seymour@cs.utk.edu)
 *
 */

package org.netlib.util;

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
