import java.lang.*;
import java.util.*;
import org.j_paine.formatter.*;
import org.netlib.util.*;

public class crap {
  public static void main(String [] args) {
    Vector iovec = new Vector();
    int flag = 1;

    while(flag > 0) {
      iovec.clear();
      flag = Util.f77read("A16", iovec);
      System.out.println("flag = " + flag + ", iovec = " + iovec);
    }
  }
}
