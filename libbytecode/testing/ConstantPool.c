#include <stdio.h>
#include "bytecode.h"

/* 
 * Inserting different kinds of constants into the constant pool.
 */

int main(){
  JVM_METHOD *main_method, *test_method;
  JVM_CLASS *cur_class_file;

  int dbl_idx, float_idx, int_idx, long_idx, str_idx, class_idx,
    utf8_idx, field_idx, meth_idx, interface_idx, name_idx;

  double x = 2.2;
  float y = 1.1;
  int a = 666;
  long long l = 123456;
  char *utf8 = "utf8 string";
  char *str = "hello world";
  char *class_str = "java.lang.String";

  CP_INFO *str_node;

  cur_class_file = bc_new_class("ConstantPool", "asdf.f", NULL, NULL, 
      JVM_ACC_PUBLIC|JVM_ACC_SUPER);

  dbl_idx = cp_find_or_insert(cur_class_file, CONSTANT_Double, &x);
  float_idx = cp_find_or_insert(cur_class_file, CONSTANT_Float, &y);
  int_idx = cp_manual_insert(cur_class_file, CONSTANT_Integer, &a);
  long_idx = cp_find_or_insert(cur_class_file, CONSTANT_Long, &l);
  utf8_idx = cp_find_or_insert(cur_class_file, CONSTANT_Utf8, utf8);
  class_idx = cp_find_or_insert(cur_class_file, CONSTANT_Class, class_str);
  str_idx = cp_find_or_insert(cur_class_file, CONSTANT_String, str);

  field_idx = bc_new_fieldref(cur_class_file, "java.lang.Boolean", 
     "TRUE", "Ljava.lang.Boolean;");
  meth_idx = bc_new_methodref(cur_class_file, "java.lang.Boolean", 
     "getBoolean", "(Ljava.lang.String;)Z");
  interface_idx = bc_new_interface_methodref(cur_class_file, 
     "java.util.Enumeration", "hasMoreElements", "()Z");
  name_idx = bc_new_name_and_type(cur_class_file, "blah", "I");

  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  bc_append(main_method, jvm_ldc2_w, dbl_idx);
  bc_append(main_method, jvm_pop2);

  bc_append(main_method, jvm_ldc2_w, long_idx);
  bc_append(main_method, jvm_pop2);

  bc_append(main_method, jvm_new, class_idx);
  bc_append(main_method, jvm_pop);

  bc_append(main_method, jvm_ldc, float_idx);
  bc_append(main_method, jvm_pop);

  bc_append(main_method, jvm_ldc, int_idx);
  bc_append(main_method, jvm_pop);

  bc_append(main_method, jvm_ldc, str_idx);
  bc_append(main_method, jvm_pop);

  bc_append(main_method, jvm_getstatic, field_idx);
  bc_append(main_method, jvm_pop);

  bc_append(main_method, jvm_ldc, str_idx);
  bc_append(main_method, jvm_invokestatic, meth_idx);
  bc_append(main_method, jvm_pop);

  bc_gen_return(main_method);

  test_method = bc_new_method(cur_class_file, "test",
     "(Ljava/util/Enumeration;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);
  bc_append(test_method, jvm_aload_0);
  bc_append(test_method, jvm_invokeinterface, interface_idx, 1);
  bc_append(test_method, jvm_pop);
  bc_gen_return(test_method);

  /* To see a dump of the constant pool, uncomment the following line. */
  /* cp_dump(cur_class_file); */

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}
