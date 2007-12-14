package org.netlib.util;

import java.util.Vector;

/**
 * This class represents array arguments to I/O calls.  For example,
 * if you pass an array to WRITE() in Fortran and the format specifies
 * to print multiple values, they'll be pulled from the array as
 * appropriate.  Here, we just pull all the array elements into
 * the I/O vector.
 * <p>
 * This file is part of the Fortran-to-Java (f2j) system,
 * developed at the University of Tennessee.
 * <p>
 * @author Keith Seymour (seymour@cs.utk.edu)
 */

public class ArraySpec {
  private Vector vec;

  /**
   * Create a new ArraySpec for an integer array.
   *
   * @param arr The array to be used in the I/O call
   * @param offset The offset into the array (i.e. the start point)
   * @param len The number of elements to copy from the
   *   array to the I/O vector.
   */
  public ArraySpec(int [] arr, int offset, int len) {
    vec = new Vector();

    for(int i=offset; i< offset+len; i++)
      vec.addElement(new Integer(arr[i]));
  }

  /**
   * Create a new ArraySpec for a double precision array.
   *
   * @param arr The array to be used in the I/O call
   * @param offset The offset into the array (i.e. the start point)
   * @param len The number of elements to copy from the
   *   array to the I/O vector.
   */
  public ArraySpec(double [] arr, int offset, int len) {
    vec = new Vector();

    for(int i=offset; i< offset+len; i++)
      vec.addElement(new Double(arr[i]));
  }

  /**
   * Create a new ArraySpec for a float array.
   *
   * @param arr The array to be used in the I/O call
   * @param offset The offset into the array (i.e. the start point)
   * @param len The number of elements to copy from the
   *   array to the I/O vector.
   */
  public ArraySpec(float [] arr, int offset, int len) {
    vec = new Vector();

    for(int i=offset; i< offset+len; i++)
      vec.addElement(new Float(arr[i]));
  }

  /**
   * Create a new ArraySpec for a String array.
   *
   * @param arr The array to be used in the I/O call
   * @param offset The offset into the array (i.e. the start point)
   * @param len The number of elements to copy from the
   *   array to the I/O vector.
   */
  public ArraySpec(String [] arr, int offset, int len) {
    vec = new Vector();

    for(int i=offset; i< offset+len; i++)
      vec.addElement(new String(arr[i]));
  }

  /**
   * Create a new ArraySpec for a String (not array).  Here the
   * String is not an array, but we want to pull out the characters
   * individually.
   *
   * @param str The string to be used in the I/O call
   */
  public ArraySpec(String str) {
    char [] chars = str.toCharArray();
    vec = new Vector();

    for(int i = 0; i < chars.length; i++)
      vec.addElement(new String(String.valueOf(chars[i])));
  }

  /**
   * Gets the I/O vector for this ArraySpec.
   *
   * @return the Vector representation of the ArraySpec.
   */
  public Vector get_vec() {
    return vec;
  }
}
