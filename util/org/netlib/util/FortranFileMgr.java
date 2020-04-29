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

  /**
   * Opens the specified file and associates it with the specified unit number.
   *
   * @param unit - the unit number to use for referencing this file
   * @param filename - the name of the file to open
   * @param status - "old", "new", "scratch", or "unknown"
   *   "old" if the file exists, "new" if it should be created,
   *   "scratch" if the file should be temporary and deleted upon close(),
   *   "unknown" is processor dependent.
   * @param access - "sequential" for sequential files, and "direct" for
   *   direct access files.
   * @param form - "formatted" or "unformatted"
   * @param recl - the length of each record in a file being connected for
   *   direct access
   * @param blank - "null" or "zero.  if "null", all blank characters in
   *   numeric formatted input fields are ignored.  if "zero", all blanks
   *   other than leading blanks are treated as zeros.
   * @param terminate_on_error - if true, call System.exit() on error,
   *   otherwise return -1.
   *
   * @returns 0 on success, -1 on error.
   */
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

  /**
   * Closes the specified unit.
   *
   * @param unit - the unit to be closed
   * @param status - "keep" or "delete".  if "delete", the file is
   *   removed.  if "keep", it is not removed.
   * @param terminate_on_error - if true, call System.exit() on error,
   *   otherwise return -1.
   *
   * @returns 0 on success, -1 on error.
   */
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

  /**
   * Rewinds the specified unit.
   *
   * @param unit - the unit to rewind
   * @param terminate_on_error - if true, call System.exit() on error,
   *   otherwise return -1.
   *
   * @returns 0 on success, -1 on error.
   */
  public int rewind(int unit, boolean terminate_on_error)
  {
    FortranFile ff;
    int rv;

    ff = get(new Integer(unit));

    if(ff == null)
      return 0;

    rv = ff.rewind(terminate_on_error);

    if((rv != 0) && !terminate_on_error)
      return rv;

    return 0;
  }

  /**
   * Flush the specified unit.
   *
   * @param unit - the unit to flush
   * @param terminate_on_error - if true, call System.exit() on error,
   *   otherwise return -1.
   *
   * @returns 0 on success, -1 on error.
   */
  public int flush(int unit, boolean terminate_on_error)
  {
    FortranFile ff;
    int rv;

    ff = get(new Integer(unit));

    if(ff == null)
      return 0;

    rv = ff.flush(terminate_on_error);

    if((rv != 0) && !terminate_on_error)
      return rv;

    return 0;
  }

  /**
   * Backspace the specified unit.
   *
   * @param unit - the unit to backspace
   * @param terminate_on_error - if true, call System.exit() on error,
   *   otherwise return -1.
   *
   * @returns 0 on success, -1 on error.
   */
  public int backspace(int unit, boolean terminate_on_error)
  {
    FortranFile ff;
    int rv;

    ff = get(new Integer(unit));

    if(ff == null)
      return 0;

    rv = ff.backspace(terminate_on_error);

    if((rv != 0) && !terminate_on_error)
      return rv;

    return 0;
  }

  /**
   * Writes an 'endfile' record to the specified unit.
   *
   * @param unit - the unit to write the endfile record to
   * @param terminate_on_error - if true, call System.exit() on error,
   *   otherwise return -1.
   *
   * @returns 0 on success, -1 on error.
   */
  public int endfile(int unit, boolean terminate_on_error)
  {
    FortranFile ff;
    int rv;

    ff = get(new Integer(unit));

    if(ff == null)
      return 0;

    rv = ff.endfile(terminate_on_error);

    if((rv != 0) && !terminate_on_error)
      return rv;

    return 0;
  }
}
