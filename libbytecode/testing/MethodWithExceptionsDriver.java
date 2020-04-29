public class MethodWithExceptionsDriver {
  public static void main(String args[]) {
    MethodWithExceptions m = new MethodWithExceptions();

    try {
      m.one_exception();
    } catch (java.lang.IllegalAccessException e) {
      System.out.println("not good");
    }

    try {
      m.two_exceptions();
    } catch (java.lang.IllegalAccessException e) {
      System.out.println("not good");
    } catch (java.lang.reflect.InvocationTargetException e2) {
      System.out.println("not good");
    }

    System.out.println("asdf");
  }
}
