import java.io.*;

public class ExceptionTableRecalcDriver {
  public static void main(String args[]) {
    try {
      ExceptionTableRecalc.a(0);
    } catch(IOException e) {
      System.out.println("caught io exception");
    } catch(ArithmeticException e2) {
      System.out.println("caught arithmetic exception");
    }

    try {
      ExceptionTableRecalc.a(1);
    } catch(IOException e) {
      System.out.println("caught io exception");
    } catch(ArithmeticException e2) {
      System.out.println("caught arithmetic exception");
    }

    System.out.println("after the catch block");
  }
}
