public class ClassWithFieldsDriver {
  public static void main(String args[]) {
    ClassWithFields c = new ClassWithFields();
    c.public_int = 1234;
    System.out.println(c.public_int);
    System.out.println(ClassWithFields.int_with_constantval_attr);
  }
}
