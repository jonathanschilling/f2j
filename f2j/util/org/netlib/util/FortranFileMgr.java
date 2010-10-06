package org.netlib.util;

import java.util.Hashtable;

/**
 * This class contains code for runtime handling of Fortran files,
 * keeps track of which files are bound to which units, etc.
 * <p>
 * This file is part of the Fortran-to-Java (f2j) system,
 * developed at the University of Tennessee.
 * <p>
 * @author Keith Seymour (seymour@cs.utk.edu)
 */

public class FortranFileMgr {
  private static final FortranFileMgr instance = new FortranFileMgr();
  private Hashtable files;
  private int INIT_CAP = 37;

  public static int FTN_STDIN = 5;
  public static int FTN_STDOUT = 6;
  public static int FTN_STDERR = 0;

  /**
   * Creates a new instance of FortranFileMgr.
   */
  private FortranFileMgr()
  {
    files = new Hashtable(INIT_CAP);
  }

  public FortranFile get(Integer u)
  {
    return (FortranFile)files.get(u);
  }

  /**
   * Returns instance of FortranFileMgr.
   *
   * @returns the instance
   */
  public static FortranFileMgr getInstance()
  {
    return instance;
  }

  public int open(int unit, String filename, String status, String access,
    String form, int recl, String blank, boolean terminate_on_error)
  {
    FortranFile ff;
    int rv;

    try {
      ff = new FortranFile(unit, filename, status, access, form, recl, blank);
    } catch(Exception e) {
      System.err.println(e);
      return -1;
    }

    rv = ff.open(terminate_on_error);

    if((rv != 0) && !terminate_on_error)
        return rv;

    files.put(new Integer(unit), ff);

    return 0;
  }

  public int close(int unit, String status, boolean terminate_on_error)
  {
    FortranFile ff;
    int rv;

    ff = get(new Integer(unit));

    /* f77 spec says closing a non-existent file is ok, so no error here,
     * just return success.
     */
    if(ff == null)
      return 0;

    rv = ff.close(status, terminate_on_error);

    if((rv != 0) && !terminate_on_error)
        return rv;

    files.remove(new Integer(unit));

    return 0;
  }
}
