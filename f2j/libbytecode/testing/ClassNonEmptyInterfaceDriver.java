public class ClassNonEmptyInterfaceDriver implements ClassNonEmptyInterface {
  public static void main(String args[]) {
    ClassNonEmptyInterfaceDriver a = new ClassNonEmptyInterfaceDriver();

    a.foo(args);
    System.out.println("asdf");
  }

  public void foo(String x[]) {
    System.out.println("foo");
    return;
  }
}
