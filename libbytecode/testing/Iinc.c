#include <stdio.h>
#include "bytecode.h"

/*
 * Tests whether the library is properly generating wide instructions
 * when the index operand to the iinc instruction exceeds an unsigned 
 * byte value or when the immediate operand exceeds a signed byte value.
 */

int main(){
  JVM_METHOD *main_method;
  JVM_CLASS *cur_class_file;

  cur_class_file = bc_new_class("Iinc", "iinc.f", NULL, 
      NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER);
  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  bc_append(main_method, jvm_iconst_0);
  bc_gen_store_op(main_method, 1, jvm_Int);
  bc_append(main_method, jvm_iconst_0);
  bc_gen_store_op(main_method, 255, jvm_Int);
  bc_append(main_method, jvm_iconst_0);
  bc_gen_store_op(main_method, 300, jvm_Int);

  bc_gen_iinc(main_method, 1, -1);
  bc_gen_iinc(main_method, 255, -1);
  bc_gen_iinc(main_method, 300, -1);
  bc_gen_iinc(main_method, 255, 127);
  bc_gen_iinc(main_method, 255, 300);
  bc_gen_iinc(main_method, 255, -127);
  bc_gen_iinc(main_method, 255, -128);
  bc_gen_iinc(main_method, 255, -129);
  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}
