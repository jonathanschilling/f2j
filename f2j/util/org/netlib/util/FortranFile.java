package org.netlib.util;

import java.io.*;
import java.util.Vector;

/**
 * This class is used to represent a Fortran file.
 * <p>
 * This file is part of the Fortran-to-Java (f2j) system,
 * developed at the University of Tennessee.
 * <p>
 * This represents all the information about a Fortran file:
 *
 *   [UNIT =] u 
 *   IOSTAT = ios
 *   ERR = s
 *   FILE = fin
 *   STATUS = sta
 *   ACCESS = acc
 *   FORM = fm
 *   RECL = rl
 *   _BLANK_=_blnk
 * <p>
 * @author Keith Seymour (seymour@cs.utk.edu)
 *
 */

public class FortranFile extends File {
  public static final String DEFAULT_STATUS = "UNKNOWN";
  public static final String DEFAULT_ACCESS = "SEQUENTIAL";
  public static final String DEFAULT_FORM_DIRECT = "UNFORMATTED";
  public static final String DEFAULT_FORM_SEQ = "FORMATTED";
  public static final String DEFAULT_BLANK = "NULL";

  public static final int DEFAULT_RECL = 0;

  public static final int ERR_OPEN = 14;
  public static final int ERR_CLOSE = 142;
  public static final int ERR_KEEP_SCRATCH = 33;
  public static final int ERR_OLD_FILE_DOESNT_EXIST = 6;
  public static final int ERR_NEW_FILE_EXISTS = 107;
  public static final int ERR_NO_RECL = 25;
  public static final int ERR_NEG_RECL = 26;
  public static final int ERR_RECL_SEQUENTIAL = 200;
  public static final int ERR_BAD_ACCESS = 27;
  public static final int ERR_BAD_FORM = 28;
  public static final int ERR_BAD_OPEN_STATUS = 29;
  public static final int ERR_BAD_CLOSE_STATUS = 34;
  public static final int ERR_BAD_BLANK = 30;
  public static final int ERR_INVALID_FILE = 31;
  public static final int ERR_SCRATCH_AND_FILE = 32;
  public static final int ERR_BLANK_UNFORMATTED = 125;
  public static final int ERR_SCRATCH_NO_FILE = 151;

  RandomAccessFile ra_file = null;

  int unit;
  String filename;
  String status;
  boolean status_flag;
  String access;
  boolean access_flag;
  String form;
  boolean form_flag;
  int recl;
  String blank;
  boolean blank_flag;

  private String errmsg = null;

  /**
   * Creates a new FortranFile with default filename of 'fort.x', where x
   * is the unit number.  Every file must have a unit number to identify it.
   *
   * @param unit - the unit number of the file.
   **/

  public FortranFile(int unit, String filename, String status, String access,
    String form, int recl, String blank)
  {
    super(filename);

    /* trim only trailing whitespace as required in f77 spec */

    this.unit = unit;
    this.filename = removeTrailingBlanks(filename);
    this.status = removeTrailingBlanks(status);
    this.access = removeTrailingBlanks(access);
    this.form = removeTrailingBlanks(form);
    this.recl = recl;
    this.blank = removeTrailingBlanks(blank);

    status_flag = status != null;
    access_flag = access != null;
    form_flag = form != null;
    blank_flag = blank != null;
  }

  public int open(boolean terminate_on_error) {
    File f = null;
    int retval = 0;

    if(filename == null)
      return ERR_OPEN;

    retval = check_open_args(filename, status, access,
       form, recl, blank);

    if(retval != 0) {
      if(!terminate_on_error)
        return retval;

      System.err.println("Runtime error: " + errmsg);
      System.exit(1);
    }

    if(status == null)
      status = DEFAULT_STATUS;

    if(access == null)
      access = DEFAULT_ACCESS;

    if(form == null) {
      if(access.equalsIgnoreCase("sequential"))
        form = DEFAULT_FORM_SEQ;
      else
        form = DEFAULT_FORM_DIRECT;
    }

    if(blank == null)
      blank = DEFAULT_BLANK;

    if(filename == null)
      filename = "fort." + Integer.toString(unit);

    try {
      f = new File(filename);

      /* if the user specified STATUS='OLD', then it is considered an
       * error if the file does not already exist.
       */
      if(status.equalsIgnoreCase("old")) {
        if(!f.exists()) {
          retval = ERR_OLD_FILE_DOESNT_EXIST;
          throw new Exception("STATUS='OLD' and file does not exist");
        }
      }

      /* similarly, if STATUS='NEW', then the file must not exist already */
      if(status.equalsIgnoreCase("new")) {
        if(f.exists()) {
          retval = ERR_NEW_FILE_EXISTS;
          throw new Exception("STATUS='NEW' and file exists");
        }
      }

      ra_file = new RandomAccessFile(f, "rw");
      ra_file.seek(ra_file.length());
    }
    catch (Exception e) {
      if(!terminate_on_error)
        return retval;

      System.err.println("Runtime error: " + e.getMessage());
      System.exit(1);
    }

    return 0;
  }

  public int close(String close_status, boolean terminate_on_error)
  {
    int retval = 0;

    retval = check_close_args(close_status);

    if(retval != 0) {
      if(!terminate_on_error)
        return retval;

      System.err.println("Runtime error: " + errmsg);
      System.exit(1);
    }

    try {
      if(ra_file != null) {
        ra_file.close();
        ra_file = null;
      }
    } catch (Exception e) {
      errmsg = "CLOSE error: unable to close file";
      return ERR_CLOSE;
    }

    if((close_status != null) && close_status.equalsIgnoreCase("delete"))
    {
      File f = null;

      try {
        f = new File(filename);

        if(f.exists())
          f.delete();
      } catch (Exception e) {
        errmsg = "CLOSE error: unable to delete file";
        return ERR_CLOSE;
      }
    }

    if(retval != 0) {
      if(!terminate_on_error)
        return retval;

      System.err.println("Runtime error: " + errmsg);
      System.exit(1);
    }

    return 0;
  }

  public PrintStream getPrintStream() {
    PrintStream ps = null;
    try {
      ps = new PrintStream(new FileOutputStream(ra_file.getFD()));
    } catch (Exception e) {
      return null;
    }

    return ps;
  }

  public DataInputStream getDataInputStream() {
    DataInputStream ds = null;
    try {
      ds = new DataInputStream(new FileInputStream(ra_file.getFD()));
    } catch (Exception e) {
      return null;
    }

    return ds;
  }

  private String removeTrailingBlanks(String s)
  {
    if(s == null) return null;

    return s.replaceAll("\\s+$", "");
  }

  private int check_open_args(String filename, String status,
    String access, String form, int recl, String blank)
  {
    if((status != null) &&
       !status.equalsIgnoreCase("old") &&
       !status.equalsIgnoreCase("new") &&
       !status.equalsIgnoreCase("scratch") &&
       !status.equalsIgnoreCase("unknown"))
    {
      errmsg =
        "OPEN error: status must be one of " +
        "OLD, NEW, SCRATCH, or UNKNOWN";
      return ERR_BAD_OPEN_STATUS;
    }

    if((status != null) && status.equalsIgnoreCase("scratch") &&
       filename != null) {
      errmsg =
        "OPEN error: SCRATCH must not be specified with a named file";
      return ERR_SCRATCH_AND_FILE;
    }

    if((access != null) &&
       !access.equalsIgnoreCase("sequential") &&
       !access.equalsIgnoreCase("direct"))
    {
      errmsg =
        "OPEN error: access must be one of " +
        "SEQUENTIAL or DIRECT";
      return ERR_BAD_ACCESS;
    }

    if((form != null) &&
       !form.equalsIgnoreCase("formatted") &&
       !form.equalsIgnoreCase("unformatted"))
    {
      errmsg =
        "OPEN error: form must be one of " +
        "FORMATTED or UNFORMATTED";
      return ERR_BAD_FORM;
    }

    if(recl < 0) {
      errmsg =
        "OPEN error: recl must be positive";
      return ERR_NEG_RECL;
    }

    if((blank != null) &&
       !blank.equalsIgnoreCase("null") &&
       !blank.equalsIgnoreCase("zero"))
    {
      errmsg =
        "OPEN error: blank must be one of " +
        "NULL or ZERO";
      return ERR_BAD_BLANK;
    }

    if((recl > 0) && (access != null) && access.equalsIgnoreCase("sequential"))
    {
      errmsg =
        "OPEN error: RECL must not be specified for " +
        " SEQUENTIAL access";
      return ERR_RECL_SEQUENTIAL;
    }

    if((recl == 0) && (access != null) && access.equalsIgnoreCase("direct"))
    {
      errmsg =
        "OPEN error: RECL must be specified for " +
        " DIRECT access";
      return ERR_NO_RECL;
    }

    if((blank != null) && blank.equalsIgnoreCase("zero") &&
       (form != null) && form.equalsIgnoreCase("unformatted"))
    {
      errmsg =
         "OPEN error: blank=ZERO can only be used " +
         " with formatted i/o";
      return ERR_BLANK_UNFORMATTED;
    }

    return 0;
  }

  private int check_close_args(String close_status)
  {
    if(close_status != null) {
      if(!close_status.equalsIgnoreCase("keep") &&
         !close_status.equalsIgnoreCase("delete"))
      {
        errmsg =
          "CLOSE error: status must be one of " +
          "KEEP or DELETE";
        return ERR_BAD_CLOSE_STATUS;
      }

      if(close_status.equalsIgnoreCase("keep") &&
         status.equalsIgnoreCase("scratch"))
      {
        errmsg =
          "CLOSE error: status cannot be " +
          "KEEP for a SCRATCH file";
        return ERR_KEEP_SCRATCH;
      }
    }

    return 0;
  }
}
