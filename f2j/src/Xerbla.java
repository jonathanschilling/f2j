// package lapack;
// import blas.*;
import java.lang.*;

public class Xerbla {

  public static void xerbla (String srname, int info) {
    System.err.println(" ** On entry to " + srname +
       " parameter number " + info + " had an illegal value");
  }
}
