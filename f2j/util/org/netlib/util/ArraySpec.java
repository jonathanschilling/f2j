package org.netlib.util;

import java.util.Vector;

public class ArraySpec {
  private Vector vec;

  public ArraySpec(int [] arr, int offset, int len) {
    vec = new Vector();

    for(int i=offset; i< offset+len; i++) {
      vec.addElement(new Integer(arr[i]));
    }
  }

  public ArraySpec(double [] arr, int offset, int len) {
    vec = new Vector();

    for(int i=offset; i< offset+len; i++) {
      vec.addElement(new Double(arr[i]));
    }
  }

  public ArraySpec(float [] arr, int offset, int len) {
    vec = new Vector();

    for(int i=offset; i< offset+len; i++) {
      vec.addElement(new Float(arr[i]));
    }
  }

  public Vector get_vec() {
    return vec;
  }
}
