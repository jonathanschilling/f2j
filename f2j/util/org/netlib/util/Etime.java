/**
 * This file is part of the Fortran-to-Java (f2java) system,
 * developed at the University of Tennessee.
 *
 * This class implements the Fortran 77 ETIME intrinsic.
 * ETIME is supposed to provide the CPU time for the
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

public class Etime {
  private static int call_num = 0;
  private static long start_time = 0;

  /**
   * Initializes the timer.
   */
  public static void etime()
  {
    float [] dummy = new float[2];
    etime(dummy,0);
  }

  /**
   * Get the elapsed time.  Sets the first element of the
   * array 't' to the elapsed time.  This is also the
   * return value.
   * 
   * @param t -- Two-element array of times.  The first
   *    element should be user time.  The second element
   *    should be system time.  Currently these are set
   *    the same, though.
   * @param t_offset -- Offset from t.  Normally zero.
   *
   * @returns first element of t.
   */
  public static float etime(float [] t, int t_offset)
  {
    if(call_num++ == 0)
    {
      start_time = System.currentTimeMillis();
      t[0 + t_offset] = 0.0f;
      t[1 + t_offset] = 0.0f;
      return 0.0f;
    }

    t[0 + t_offset]=(float)(System.currentTimeMillis() - start_time) / 1000.0f;
    t[1 + t_offset] = t[0 + t_offset];
    return t[0 + t_offset];
  }
}
