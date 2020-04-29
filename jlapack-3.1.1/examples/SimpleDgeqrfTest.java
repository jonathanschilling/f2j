import org.netlib.util.*;
import org.netlib.lapack.DGEQRF;
import org.netlib.lapack.DORGQR;

public class SimpleDgeqrfTest {

  public static void main(String[] args) {
    double[][] bb = {{-0.3714,   -0.7428,    -0.5571},
                     { 0.5774,     0.5774,    0.5774 }};
    int M = bb.length;
    int N = bb[0].length;
    double[]tau = new double[Math.min(M,N)];
    double[]work = new double[Math.max(1,N)];
    org.netlib.util.intW info = new org.netlib.util.intW(2);

    DGEQRF.DGEQRF(M, N, bb, tau, work, work.length, info);

    System.out.println("dgeqrf info = " + info.val);

    DORGQR.DORGQR(M, Math.min(M,N), Math.min(M,N), bb, tau, work, work.length, info);

    System.out.println("dorgqr info = " + info.val);

    for(int i=0;i<M;i++) {
      for(int j=0;j<Math.min(M,N);j++) {
        System.out.print(bb[i][j] + " ");
      }
      System.out.println();
    }
  }
}
