package org.netlib.util;

import java.io.*;
import java.util.Vector;
import org.j_paine.formatter.*;

/**
 * Implementations of various Fortran intrinsic functions.
 * <p>
 * This file is part of the Fortran-to-Java (f2j) system,
 * developed at the University of Tennessee.
 * <p>
 * This class contains various helper routines for f2j-generated code.
 * These routines are primarily implemented for handling Fortran intrinsic
 * functions.
 * <p>
 * @author Keith Seymour (seymour@cs.utk.edu)
 *
 */

public class Util {

  /**
   * Inserts a string into a substring of another string.
   * <p>
   * This method handles situations in which the lhs of an
   * assignment statement is a substring operation.  For example:
   * <p>
   * <code>
   *   a(3:4) = 'hi'
   * </code>
   * <p>
   * We haven't figured out an elegant way to do this with Java Strings,
   * but we do handle it, as follows:
   * <p>
   * <p>
   * <code>
   *  a = new StringW(
   *        a.val.substring(0,E1-1) +
   *        "hi".substring(0,E2-E1+1) +
   *        a.val.substring(E2,a.val.length())
   *      );
   * <code>
   * <p>
   * Where E1 is the expression representing the starting index of the substring
   * and E2 is the expression representing the ending index of the substring
   * <p>
   * The resulting code looks pretty bad because we have to be
   * prepared to handle rhs strings that are too big to fit in
   * the lhs substring.
   * <p>
   * @param x dest (string to be inserted into)
   * @param y source (substring to insert into 'x')
   * @param E1 expression representing the start of the substring
   * @param E2 expression representing the end of the substring
   *
   * @return the string containing the complete string after inserting the
   *    substring
   */
  public static String stringInsert(String x, String y, int E1, int E2) {
    String tmp;
  
    tmp = new String(
           x.substring(0,E1-1) +
           y.substring(0,E2-E1+1) +
           x.substring(E2,x.length()));
    return tmp;
  }

  /**
   * Inserts a string into a single character substring of another string.
   *
   * @param x dest (string to be inserted into)
   * @param y source (substring to insert into 'x')
   * @param E1 expression representing the index of the character
   *
   * @return the string containing the complete string after inserting the
   *    substring
   */
  public static String stringInsert(String x, String y, int E1) {
    return stringInsert(x, y, E1, E1);
  }

  /**
   * Returns a string representation of the character at the given index.
   * Note: this is based on the Fortran index (1..N).
   *
   * @param s the string
   * @param idx the index
   *
   * @return new string containing a single character (from s[idx])
   */
  public static String strCharAt(String s, int idx) {
    return String.valueOf(s.charAt(idx-1));
  }

  /**
   * Three argument integer max function.
   *
   * @param x value 1
   * @param y value 2
   * @param z value 3
   * 
   * @return the largest of x, y, or z
   */
  public static int max(int x, int y, int z) {
    return Math.max( x > y ? x : y, Math.max(y,z));
  }

  /**
   * Three argument single precision max function.
   *
   * @param x value 1
   * @param y value 2
   * @param z value 3
   * 
   * @return the largest of x, y, or z
   */
  public static float max(float x, float y, float z) {
    return Math.max( x > y ? x : y, Math.max(y,z));
  }

  /**
   * Three argument double precision max function.
   *
   * @param x value 1
   * @param y value 2
   * @param z value 3
   * 
   * @return the largest of x, y, or z
   */
  public static double max(double x, double y, double z) {
    return Math.max( x > y ? x : y, Math.max(y,z));
  }

  /**
   * Three argument integer min function.
   *
   * @param x value 1
   * @param y value 2
   * @param z value 3
   * 
   * @return the smallest of x, y, or z
   */
  public static int min(int x, int y, int z) {
    return Math.min( x < y ? x : y, Math.min(y,z));
  }

  /**
   * Three argument single precision min function.
   *
   * @param x value 1
   * @param y value 2
   * @param z value 3
   * 
   * @return the smallest of x, y, or z
   */
  public static float min(float x, float y, float z) {
    return Math.min( x < y ? x : y, Math.min(y,z));
  }

  /**
   * Three argument double precision min function.
   *
   * @param x value 1
   * @param y value 2
   * @param z value 3
   * 
   * @return the smallest of x, y, or z
   */
  public static double min(double x, double y, double z) {
    return Math.min( x < y ? x : y, Math.min(y,z));
  }

  /**
   * Base-10 logarithm function.
   *
   * @param x the value
   *
   * @return base-10 log of x
   */
  public static double log10(double x) {
    return Math.log(x) / 2.30258509;
  }

  /**
   * Base-10 logarithm function.
   *
   * @param x the value
   *
   * @return base-10 log of x
   */
  public static float log10(float x) {
    return (float) (Math.log(x) / 2.30258509);
  }

  /**
   * Fortran nearest integer (NINT) intrinsic function.
   * <p>
   * Returns:
   * <ul>
   *   <li> (int)(x+0.5), if x &gt;= 0
   *   <li> (int)(x-0.5), if x &lt; 0
   * </ul>
   * 
   * @param x the floating point value
   *
   * @return the nearest integer to x
   */
  public static int nint(float x) {
    return (int) (( x >= 0 ) ? (x + 0.5) : (x - 0.5));
  }

  /**
   * Fortran nearest integer (IDNINT) intrinsic function.
   * <p>
   * Returns:<br>
   * <ul>
   *   <li> (int)(x+0.5), if x &gt;= 0
   *   <li> (int)(x-0.5), if x &lt; 0
   * </ul>
   * 
   * @param x the double precision floating point value
   *
   * @return the nearest integer to x
   */
  public static int idnint(double x) {
    return (int) (( x >= 0 ) ? (x + 0.5) : (x - 0.5));
  }

  /**
   * Fortran floating point transfer of sign (SIGN) intrinsic function.
   * <p>
   * Returns:<br>
   * <ul>
   *   <li> abs(a1), if a2 &gt;= 0
   *   <li>-abs(a1), if a2 &lt; 0
   * </ul>
   *
   * @param a1 floating point value
   * @param a2 sign transfer indicator
   *
   * @return equivalent of Fortran SIGN(a1,a2) as described above.
   */
  public static float sign(float a1, float a2) {
    return (a2 >= 0) ? Math.abs(a1) : -Math.abs(a1);
  }

  /**
   * Fortran integer transfer of sign (ISIGN) intrinsic function.
   * <p>
   * Returns:<br>
   * <ul>
   *   <li> abs(a1), if a2 &gt;= 0
   *   <li>-abs(a1), if a2 &lt; 0
   * </ul>
   *
   * @param a1 integer value
   * @param a2 sign transfer indicator
   *
   * @return equivalent of Fortran ISIGN(a1,a2) as described above.
   */
  public static int isign(int a1, int a2) {
    return (a2 >= 0) ? Math.abs(a1) : -Math.abs(a1);
  }

  /**
   * Fortran double precision transfer of sign (DSIGN) intrinsic function.
   * <p>
   * Returns:<br>
   * <ul>
   *   <li> abs(a1), if a2 &gt;= 0
   *   <li>-abs(a1), if a2 &lt; 0
   * </ul>
   *
   * @param a1 double precision floating point value
   * @param a2 sign transfer indicator
   *
   * @return equivalent of Fortran DSIGN(a1,a2) as described above.
   */
  public static double dsign(double a1, double a2) {
    return (a2 >= 0) ? Math.abs(a1) : -Math.abs(a1);
  }

  /**
   * Fortran floating point positive difference (DIM) intrinsic function.
   * <p>
   * Returns:<br>
   * <ul>
   *   <li> a1 - a2, if a1 &gt; a2
   *   <li> 0, if a1 &lt;= a2
   * </ul>
   *
   * @param a1 floating point value
   * @param a2 floating point value
   *
   * @return equivalent of Fortran DIM(a1,a2) as described above.
   */
  public static float dim(float a1, float a2) {
    return (a1 > a2) ? (a1 - a2) : 0;
  }

  /**
   * Fortran integer positive difference (IDIM) intrinsic function.
   * <p>
   * Returns:<br>
   * <ul>
   *   <li> a1 - a2, if a1 &gt; a2
   *   <li> 0, if a1 &lt;= a2
   * </ul>
   *
   * @param a1 integer value
   * @param a2 integer value
   *
   * @return equivalent of Fortran IDIM(a1,a2) as described above.
   */
  public static int idim(int a1, int a2) {
    return (a1 > a2) ? (a1 - a2) : 0;
  }

  /**
   * Fortran double precision positive difference (DDIM) intrinsic function.
   * <p>
   * Returns:<br>
   * <ul>
   *   <li> a1 - a2, if a1 &gt; a2
   *   <li> 0, if a1 &lt;= a2
   * </ul>
   *
   * @param a1 double precision floating point value
   * @param a2 double precision floating point value
   *
   * @return equivalent of Fortran DDIM(a1,a2) as described above.
   */
  public static double ddim(double a1, double a2) {
    return (a1 > a2) ? (a1 - a2) : 0;
  }

  /**
   * Fortran hyperbolic sine (SINH) intrinsic function.
   *
   * @param a the value to get the sine of
   *
   * @return the hyperbolic sine of a
   */
  public static double sinh(double a) {
    return ( Math.exp(a) - Math.exp(-a) ) * 0.5;
  }

  /**
   * Fortran hyperbolic cosine (COSH) intrinsic function.
   *
   * @param a the value to get the cosine of
   *
   * @return the hyperbolic cosine of a
   */
  public static double cosh(double a) {
    return ( Math.exp(a) + Math.exp(-a) ) * 0.5;
  }

  /**
   * Fortran hyperbolic tangent (TANH) intrinsic function.
   *
   * @param a the value to get the tangent of
   *
   * @return the hyperbolic tangent of a
   */
  public static double tanh(double a) {
    return sinh(a) / cosh(a);
  }

  /**
   * Pauses execution temporarily.
   * <p>
   * I think this was an implementation dependent feature of Fortran 77.
   */
  public static void pause() {
    pause(null);
  }

  /**
   * Pauses execution temporarily.
   * <p>
   * I think this was an implementation dependent feature of Fortran 77.
   *
   * @param msg the message to be printed before pausing.  if null, no
   *   message will be printed.
   */
  public static void pause(String msg) {
    if(msg != null)
      System.err.println("PAUSE: " + msg);
    else
      System.err.print("PAUSE: ");

    System.err.println("To resume execution, type:   go");
    System.err.println("Any other input will terminate the program.");

    String response = null;

    try {
      response = EasyIn.myCrappyReadLine();
    } catch (IOException e) {
      response = null;
    }

    if( (response == null) ||  !response.equals("go")) {
      System.err.println("STOP");
      System.exit(0);
    }
  }

  /**
   * Formatted write.
   *
   * @param fmt String containing the Fortran format specification.
   * @param v Vector containing the arguments to the WRITE() call.
   *
   */
  public static void f77write(String fmt, Vector v) {
    f77write(FortranFileMgr.FTN_STDOUT, fmt, v);
  }

  /**
   * Unformatted write.
   *
   * @param v Vector containing the arguments to the WRITE() call.
   *
   */
  public static void f77write(Vector v) {
    f77write(FortranFileMgr.FTN_STDOUT, v);
  }

  private static DataInputStream getDataInputStream(int unit)
  {
    DataInputStream instream = null;
    FortranFile ff;
    FortranFileMgr fmgr;

    fmgr = FortranFileMgr.getInstance();
    ff = fmgr.get(new Integer(unit));

    if(ff != null)
      instream = ff.getDataInputStream();
    else if(unit == FortranFileMgr.FTN_STDIN)
      instream = new DataInputStream(System.in);

    return instream;
  }

  private static PrintStream getPrintStream(int unit)
  {
    PrintStream outstream = null;
    FortranFile ff;
    FortranFileMgr fmgr;

    fmgr = FortranFileMgr.getInstance();
    ff = fmgr.get(new Integer(unit));

    if(ff != null)
      outstream = ff.getPrintStream();
    else {
      if(unit == FortranFileMgr.FTN_STDOUT)
        outstream = System.out;
      else if(unit == FortranFileMgr.FTN_STDERR)
        outstream = System.err;
    }

    return outstream;
  }

  /**
   * Formatted write.
   *
   * @param unit Unit number to which output should go
   * @param fmt String containing the Fortran format specification.
   * @param v Vector containing the arguments to the WRITE() call.
   *
   */
  public static void f77write(int unit, String fmt, Vector v)
  {
    PrintStream outstream = null;

    if(fmt == null) {
      f77write(unit, v);
      return;
    }

    outstream = getPrintStream(unit);

    try {
      Formatter f = new Formatter(fmt);
      Vector newvec = processVector(v);
      f.write( newvec, outstream );
      outstream.println();
    }
    catch ( Exception e ) {
      String m = e.getMessage();

      if(m != null)
        outstream.println(m);
      else
        outstream.println();
    }

    outstream.flush();
  }

  /**
   * Unformatted write.
   *
   * @param unit Unit number to which output should go
   * @param v Vector containing the arguments to the WRITE() call.
   *
   */
  public static void f77write(int unit, Vector v)
  {
    PrintStream outstream = null;
    java.util.Enumeration e;
    Object o;

    Vector newvec = processVector(v);

    e = newvec.elements();

    outstream = getPrintStream(unit);

    /* fortran seems to prepend a space before the first
     * unformatted element.  since non-string types get
     * a string prepended in the loop below, we only
     * do it for strings here.
     */

    if(e.hasMoreElements()) {
      o = e.nextElement();
      if(o instanceof String)
        outstream.print(" ");
      output_unformatted_element(o, outstream);
    }

    while(e.hasMoreElements())
      output_unformatted_element(e.nextElement(), outstream);

    outstream.println();
    outstream.flush();
  }

  private static void output_unformatted_element(Object o, PrintStream os) {
    if(o instanceof Boolean) {
      /* print true/false as T/F like fortran does */
      if(((Boolean) o).booleanValue())
        os.print(" T");
      else
        os.print(" F");
    }
    else if((o instanceof Float) || (o instanceof Double))
      os.print("  " + o);  // two spaces
    else if(o instanceof String)
      os.print(o);
    else
      os.print(" " + o);   // one space
  }

  /**
   * Formatted read.
   *
   * @param fmt String containing the Fortran format specification.
   * @param v Vector containing the arguments to the READ() call.
   *
   */
  public static int f77read(String fmt, Vector v)
  {
    return f77read(FortranFileMgr.FTN_STDIN, fmt, v);
  }

  public static int f77read(int unit, String fmt, Vector v)
  {
    try {
      Formatter f = new Formatter(fmt);
      f.read( v, new DataInputStream(System.in) );
    }
    catch ( EndOfFileWhenStartingReadException eof_exc) {
      return 0;
    }
    catch ( Exception e ) {
      String m = e.getMessage();

      if(m != null)
        System.out.println(m);
      else
        System.out.println("Warning: READ exception.");

      return -1;
    }

    return v.size();
  }

  /**
   * Expands array elements into separate entries in the Vector. 
   *
   */

  static Vector processVector(Vector v)
  {
    java.util.Enumeration e;
    Vector newvec = new Vector();

    for(e = v.elements(); e.hasMoreElements() ;) {
      Object el = e.nextElement();

      if(el instanceof ArraySpec)
        newvec.addAll(((ArraySpec)el).get_vec());
      else
        newvec.addElement(el);
    }

    return newvec;
  }
}
