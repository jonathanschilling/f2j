import org.netlib.util.*;
import org.netlib.lapack.Dgeqrf;
import org.netlib.lapack.Dorgqr;

public class DgeqrfTestFullQ {

  public static void main(String[] args) {
    double[] bb = {-0.3714, -0.7428, -0.5571, 0.5774, 0.5774, 0.5774};
    int M = 3;
    int N = 2;
    double[] q = new double[Math.max(M,N) * Math.max(M,N)];
    double[]tau = new double[Math.min(M,N)];
    double[]work = new double[Math.max(1,Math.max(M,N))];
    org.netlib.util.intW info = new org.netlib.util.intW(2);
    int i,j;

    Dgeqrf.dgeqrf(M, N, bb, 0, M, tau, 0, work, 0, work.length, info);

    System.out.println("dgeqrf info = " + info.val);

    for(i=0;i<Math.min(M,N);i++) {
      for(j=0;j<i;j++)
        System.out.print("               0.0 ");
      for(j=i;j<N;j++) {
        System.out.print(bb[i+j*M] + " ");
      }
      System.out.println();
    }
    for(i=Math.min(M,N);i<Math.max(M,N);i++)
      for(j=0;j<N;j++)
        System.out.print("               0.0 ");
    System.out.println();

    for(i=1;i<M;i++) {
      for(j=0;j<i;j++) {
        q[i+j*M] = bb[i+j*M];
      }
    }

    Dorgqr.dorgqr(M, M, Math.min(M,N), q, 0, M, tau, 0, work, 0, work.length, info);

    System.out.println("dorgqr info = " + info.val);

    for(i=0;i<M;i++) {
      for(j=0;j<M;j++) {
        System.out.print(q[i+j*M] + " ");
      }
      System.out.println();
    }
  }
}
