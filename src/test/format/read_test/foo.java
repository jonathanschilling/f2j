import java.lang.*;
import java.util.*;
import org.j_paine.formatter.*;
import org.netlib.util.*;

public class foo {
  public static void main(String [] args) {
    Vector iovec = new Vector();
    String summry= new String("                                ");
    int nout= 0;

    EasyIn __f2j_stdin = new EasyIn();
    java.util.Vector __io_vec = new java.util.Vector();
    summry = __f2j_stdin.readChars(32);
    __f2j_stdin.skipRemaining();
    nout = __f2j_stdin.readInt();
    __f2j_stdin.skipRemaining();

    System.out.println(summry + " ## " + nout);

    while(true) {
      String s;
      boolean b;

      iovec.clear();
      if(Util.f77read("A6, L2", iovec) <= 0)
        break;
      s = (String)iovec.remove(0);
      b = ((Boolean) iovec.remove(0)).booleanValue();
      System.out.println("s = " + s);
      System.out.println("b = " + b);
    }
  }
}
