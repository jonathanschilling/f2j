#include <stdio.h>
#include "bytecode.h"

/*
 * Generate a method with a line number table.
 */

int main(){
  JVM_METHOD *main_method;
  JVM_CLASS *cur_class_file;
  JVM_CODE_GRAPH_NODE *op;

  cur_class_file = bc_new_class("LineNumbers", "asdf.f", NULL, 
      NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER);
  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  /* line 13 */
  op = bc_push_int_const(main_method, 12);
  bc_gen_store_op(main_method, 1, jvm_Int);
  bc_set_line_number(main_method, op, 13);

  /* line 14 */
  op = bc_push_int_const(main_method, 34);
  bc_gen_store_op(main_method, 2, jvm_Int);
  bc_set_line_number(main_method, op, 14);

  /* line 15 */
  op = bc_push_int_const(main_method, 56);
  bc_gen_store_op(main_method, 3, jvm_Int);
  bc_set_line_number(main_method, op, 15);

  /* line 16 */
  op = bc_gen_load_op(main_method, 1, jvm_Int);
  bc_gen_load_op(main_method, 2, jvm_Int);
  bc_append(main_method, jvm_imul);
  bc_gen_load_op(main_method, 3, jvm_Int);
  bc_push_int_const(main_method, 2);
  bc_append(main_method, jvm_isub);
  bc_append(main_method, jvm_iadd);
  bc_gen_store_op(main_method, 3, jvm_Int);
  bc_set_line_number(main_method, op, 16);

  /* line 17 */
  op = bc_gen_load_op(main_method, 3, jvm_Int);
  bc_push_int_const(main_method, 666);
  bc_append(main_method, jvm_iadd);
  bc_gen_store_op(main_method, 3, jvm_Int);
  bc_set_line_number(main_method, op, 17);

  /* line 18 */
  op = bc_gen_return(main_method);
  bc_set_line_number(main_method, op, 18);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}
