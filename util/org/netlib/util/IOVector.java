package org.netlib.util;

import java.util.Vector;

/**
 * Implementation of I/O Vector for f2j-generated I/O statements.
 * <p>
 * This file is part of the Fortran-to-Java (f2j) system,
 * developed at the University of Tennessee.
 * <p>
 * This class is used to hold values prior to calling a write
 * operation or following a read operation.  This is a subclass
 * of Java's Vector class and currently we have only added one
 * new method.
 * <p>
 * @author Keith Seymour (seymour@cs.utk.edu)
 */

public class IOVector extends Vector
{
  /**
   * Creates a new IOVector.
   */
  public IOVector() {
    super();
  }

  /**
   * just to work around verifier complaint.
   */
  public void clear() {
    super.clear();
  }

  /**
   * another workaround.
   */
  public Object remove(int idx) {
    return super.remove(idx);
  }

  /**
   * Simple wrapper for addElement() that avoids annoying "unchecked call" warning
   * messages.
   *
   * @param o - the object to add to the IOVector
   */

  public void addElement(Object o) {
    super.addElement(o);
  }

  /**
   * Remove the element at idx and if it is a String type, then
   * right-pad to length total_len.
   *
   * @param idx - index of the element to get
   * @param total_len - length to pad string types to
   *
   * @returns the removed object
   */
  public Object remove(int idx, int total_len) {
    Object o;

    o = super.remove(idx);

    if(o == null)
      return null;

    if(o instanceof String) {
      String s = (String) o;
      int slen;

      slen = s.length();

      if(slen >= total_len)
        return o;

      return Util.pad(s, total_len, ' ');
    }

    return o;
  }
}
