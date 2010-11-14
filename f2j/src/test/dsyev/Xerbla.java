package org.netlib.err;
import java.lang.*;

public class Xerbla {

  public static void xerbla (String srname, int info)  {

    System.out.println(" ** On entry to "  + (srname) + " "  + " parameter number "  + (info) + " "  + " had "  + "an illegal value" );

    return;
  }
}
