import org.netlib.lapack.Dsyev;
import org.netlib.util.*;
import org.netlib.err.*;

public class DsyevTest {
  public static void main(String args[]) {
    java.lang.String jobz;
    java.lang.String uplo;
    int n;
    double[] a;
    int _a_offset;
    int lda;
    double[] w;
    int _w_offset;
    double[] work;
    int _work_offset;
    int lwork;
    intW info;

    System.out.println("hello");

    jobz = "N";
    uplo = "U";
    n = 10;
    lda = 10;
    a = new double[n*lda];
    _a_offset = 0;
    lwork = 40;
    w = new double[n];
    _w_offset = 0;
    work = new double[lwork];
    _work_offset = 0;
    info = new intW(0);
lda=1;

    Dsyev.dsyev(jobz, uplo, n, a, _a_offset, lda, w, _w_offset, work, _work_offset, lwork, info);
           
    System.out.println("after, info = " + info.val);
  }
}
