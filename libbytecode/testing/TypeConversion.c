#include <stdio.h>
#include "bytecode.h"

/*
 * Generates all the various type conversion instructions mentioned in
 * section 3.11.4 of the JVM spec.
 */

int main(){
  JVM_METHOD *main_method;
  JVM_CLASS *cur_class_file;
  int int_var, long_var, float_var, double_var, byte_var, char_var, short_var;
  int i;

  cur_class_file = bc_new_class("TypeConversion", "asdf.f", NULL, 
      NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER);
  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  int_var = bc_get_next_local(main_method, jvm_Int);
  long_var = bc_get_next_local(main_method, jvm_Long);
  float_var = bc_get_next_local(main_method, jvm_Float);
  double_var = bc_get_next_local(main_method, jvm_Double);
  char_var = bc_get_next_local(main_method, jvm_Char);
  byte_var = bc_get_next_local(main_method, jvm_Byte);
  short_var = bc_get_next_local(main_method, jvm_Short);

  /* Widening conversions */

  bc_push_int_const(main_method, 10);
  bc_append(main_method, jvm_i2l);
  bc_gen_store_op(main_method, long_var, jvm_Long);

  bc_push_int_const(main_method, 10);
  bc_append(main_method, jvm_i2f);
  bc_gen_store_op(main_method, float_var, jvm_Float);

  bc_push_int_const(main_method, 10);
  bc_append(main_method, jvm_i2d);
  bc_gen_store_op(main_method, double_var, jvm_Double);

  bc_push_long_const(main_method, 10);
  bc_append(main_method, jvm_l2f);
  bc_gen_store_op(main_method, float_var, jvm_Float);

  bc_push_long_const(main_method, 10);
  bc_append(main_method, jvm_l2d);
  bc_gen_store_op(main_method, double_var, jvm_Double);

  bc_push_float_const(main_method, 10.0);
  bc_append(main_method, jvm_f2d);
  bc_gen_store_op(main_method, double_var, jvm_Double);

  /* Narrowing conversions */

  bc_push_int_const(main_method, 10);
  bc_append(main_method, jvm_i2b);
  bc_gen_store_op(main_method, byte_var, jvm_Byte);

  bc_push_int_const(main_method, 10);
  bc_append(main_method, jvm_i2c);
  bc_gen_store_op(main_method, char_var, jvm_Char);

  bc_push_int_const(main_method, 10);
  bc_append(main_method, jvm_i2s);
  bc_gen_store_op(main_method, short_var, jvm_Short);

  bc_push_long_const(main_method, 10);
  bc_append(main_method, jvm_l2i);
  bc_gen_store_op(main_method, int_var, jvm_Int);

  bc_push_float_const(main_method, 10.0);
  bc_append(main_method, jvm_f2i);
  bc_gen_store_op(main_method, int_var, jvm_Int);

  bc_push_float_const(main_method, 10.0);
  bc_append(main_method, jvm_f2l);
  bc_gen_store_op(main_method, long_var, jvm_Long);

  bc_push_double_const(main_method, 10.0);
  bc_append(main_method, jvm_d2i);
  bc_gen_store_op(main_method, int_var, jvm_Int);

  bc_push_double_const(main_method, 10.0);
  bc_append(main_method, jvm_d2l);
  bc_gen_store_op(main_method, long_var, jvm_Long);

  bc_push_double_const(main_method, 10.0);
  bc_append(main_method, jvm_d2f);
  bc_gen_store_op(main_method, float_var, jvm_Float);

  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}
