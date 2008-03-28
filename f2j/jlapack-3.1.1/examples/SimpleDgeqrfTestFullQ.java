import org.netlib.util.*;
import org.netlib.lapack.DGEQRF;
import org.netlib.lapack.DORGQR;

public class SimpleDgeqrfTestFullQ {

  public static void main(String[] args) {
    double[][] bb = {{-0.3714,    0.5774},
                     {-0.7428,    0.5774},
                     {-0.5571,    0.5774}};
    int M = bb.length;
    int N = bb[0].length;
    double[][] q = new double[Math.max(M,N)][Math.max(M,N)];
    double[]tau = new double[Math.min(M,N)];
    double[]work = new double[Math.max(1,Math.max(M,N))];
    org.netlib.util.intW info = new org.netlib.util.intW(2);
    int i,j;

    DGEQRF.DGEQRF(M, N, bb, tau, work, work.length, info);

    System.out.println("dgeqrf info = " + info.val);

    for(i=0;i<Math.min(M,N);i++) {
      for(j=0;j<i;j++)
        System.out.print("               0.0 ");
      for(j=i;j<N;j++) {
        System.out.print(bb[i][j] + " ");
      }
      System.out.println();
    }
    for(i=Math.min(M,N);i<Math.max(M,N);i++)
      for(j=0;j<N;j++)
        System.out.print("               0.0 ");
    System.out.println();

    for(i=1;i<M;i++) {
      for(j=0;j<i;j++) {
        q[i][j] = bb[i][j];
      }
    }

    DORGQR.DORGQR(M, M, Math.min(M,N), q, tau, work, work.length, info);

    System.out.println("dorgqr info = " + info.val);

    for(i=0;i<M;i++) {
      for(j=0;j<M;j++) {
        System.out.print(q[i][j] + " ");
      }
      System.out.println();
    }
  }
}
