#include <stdio.h>
#include "bytecode.h"

/*
 * Generate a method with a local variable table.  the "start" of the
 * local variable should be set to the first op after a value has been
 * assigned to the variable (see section 4.7.9 of the JVM spec).  if the
 * variable is valid through the end of the method, the the "end" field
 * does not need to be set.  it will automatically be set to the last
 * instruction in the method.  otherwise, the "end" should be set manually
 * to the last instruction for which the variable would be valid.
 */

int main(){
  JVM_METHOD *main_method;
  JVM_CLASS *cur_class_file;
  JVM_CODE_GRAPH_NODE *op;
  JVM_LOCAL_VARIABLE_TABLE_ENTRY *arg_ent, *a_ent, *x_ent, *y_ent, *z_ent;

  cur_class_file = bc_new_class("LocalVariables", "asdf.f", NULL, 
      NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER);
  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  arg_ent = bc_set_local_var_name(main_method, 0, "args", "Ljava/lang/String;");

  /* line 13 */
  op = bc_push_int_const(main_method, 12);
  bc_gen_store_op(main_method, 1, jvm_Int);
  bc_set_line_number(main_method, op, 13);
  x_ent = bc_set_local_var_name(main_method, 1, "x", "I");

  bc_set_local_var_start(arg_ent, op);

  /* line 14 */
  op = bc_push_int_const(main_method, 34);
  bc_gen_store_op(main_method, 2, jvm_Int);
  bc_set_line_number(main_method, op, 14);
  y_ent = bc_set_local_var_name(main_method, 2, "y", "I");

  bc_set_local_var_start(x_ent, op);

  /* line 15 */
  op = bc_push_int_const(main_method, 56);
  bc_gen_store_op(main_method, 3, jvm_Int);
  bc_set_line_number(main_method, op, 15);
  z_ent = bc_set_local_var_name(main_method, 3, "z", "I");

  bc_set_local_var_start(y_ent, op);

  /* line 18 */
  op = bc_push_int_const(main_method, 78);
  bc_gen_store_op(main_method, 4, jvm_Int);
  bc_set_line_number(main_method, op, 18);
  a_ent = bc_set_local_var_name(main_method, 4, "a", "I");

  bc_set_local_var_start(z_ent, op);

  /* line 19 */
  op = bc_gen_load_op(main_method, 4, jvm_Int);
  bc_set_local_var_start(a_ent, op);
  op = bc_gen_store_op(main_method, 3, jvm_Int);
  bc_set_local_var_end(a_ent, op);
  bc_set_line_number(main_method, op, 19);

  /* line 21 */
  op = bc_gen_load_op(main_method, 1, jvm_Int);
  bc_gen_load_op(main_method, 2, jvm_Int);
  bc_append(main_method, jvm_imul);
  bc_gen_load_op(main_method, 3, jvm_Int);
  bc_push_int_const(main_method, 2);
  bc_append(main_method, jvm_isub);
  bc_append(main_method, jvm_iadd);
  bc_gen_store_op(main_method, 3, jvm_Int);
  bc_set_line_number(main_method, op, 21);

  /* line 22 */
  op = bc_gen_load_op(main_method, 3, jvm_Int);
  bc_push_int_const(main_method, 666);
  bc_append(main_method, jvm_iadd);
  bc_gen_store_op(main_method, 3, jvm_Int);
  bc_set_line_number(main_method, op, 21);

  /* line 23 */
  op = bc_gen_return(main_method);
  bc_set_line_number(main_method, op, 23);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}
