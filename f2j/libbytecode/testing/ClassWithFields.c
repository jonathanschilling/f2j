#include <stdio.h>
#include "bytecode.h"

/* 
 * Generates a class with some fields.
 */

int main(){
  JVM_METHOD *main_method;
  JVM_FIELD *field;
  JVM_CLASS *cur_class_file;
  int intval;
  double doubleval;
  int c;

  cur_class_file = bc_new_class("ClassWithFields", "asdf.f", NULL, NULL, 
      JVM_ACC_PUBLIC|JVM_ACC_SUPER);

  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  bc_add_field(cur_class_file, "public_int",     "I", JVM_ACC_PUBLIC);
  bc_add_field(cur_class_file, "private_int",    "I", JVM_ACC_PRIVATE);
  bc_add_field(cur_class_file, "protected_int",  "I", JVM_ACC_PROTECTED);
  bc_add_field(cur_class_file, "static_int",     "I", JVM_ACC_STATIC);
  bc_add_field(cur_class_file, "final_int",      "I", JVM_ACC_FINAL);
  bc_add_field(cur_class_file, "volatile_int",   "I", JVM_ACC_VOLATILE);
  bc_add_field(cur_class_file, "transient_int",  "I", JVM_ACC_TRANSIENT);

  bc_add_field(cur_class_file, "public_static_int",  "I", JVM_ACC_PUBLIC|JVM_ACC_STATIC);
  bc_add_field(cur_class_file, "private_static_int", "I", JVM_ACC_PRIVATE|JVM_ACC_STATIC);
  bc_add_field(cur_class_file, "public_final_int",   "I", JVM_ACC_PUBLIC|JVM_ACC_FINAL);

  bc_add_field(cur_class_file, "public_object", "Ljava/lang/String;", JVM_ACC_PUBLIC);

  bc_add_field(cur_class_file, "public_int_array",   "[I", JVM_ACC_PUBLIC);

  /* test adding ConstantValue attribute to a field.  first create the
   * field and then add the attribute 
   */

  field = bc_add_field(cur_class_file, "int_with_constantval_attr", "I", 
     JVM_ACC_PUBLIC);

  intval = 666;

  bc_set_constant_value_attr(field, CONSTANT_Integer, &intval);

  field = bc_add_field(cur_class_file, "dbl_with_constantval_attr", "D",
     JVM_ACC_PUBLIC);

  doubleval = 123.456;

  bc_set_constant_value_attr(field, CONSTANT_Double, &doubleval);

  /* test adding Synthetic attribute to a field. */

  field = bc_add_field(cur_class_file, "int_with_synthetic_attr", "I", 
     JVM_ACC_PUBLIC);

  bc_set_field_synthetic(field);

  /* test adding Deprecated attribute to a field. */

  field = bc_add_field(cur_class_file, "int_deprecated", "I", 
     JVM_ACC_PUBLIC);

  bc_set_field_deprecated(field);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}
