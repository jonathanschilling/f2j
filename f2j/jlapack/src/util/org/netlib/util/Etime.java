/*
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
 * Keith Seymour (seymour@cs.utk.edu)
 *
 */

package org.netlib.util;

public class Etime {
  private static int call_num = 0;
  private static long start_time = 0;

  public static void etime()
  {
    double [] dummy = new double[2];
    etime(dummy,0);
  }

  public static double etime(double [] t, int t_offset)
  {
    if(call_num++ == 0)
    {
      start_time = System.currentTimeMillis();
      t[0 + t_offset] = 0.0;
      t[1 + t_offset] = 0.0;
      return 0.0;
    }

    t[0 + t_offset]=(double)(System.currentTimeMillis() - start_time) / 1000.0;
    t[1 + t_offset] = t[0 + t_offset];
    return t[0 + t_offset];
  }
}
