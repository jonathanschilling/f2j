#include <stdio.h>
#include "bytecode.h"

/* 
 * Generates a goto which jumps over the exception handler, but creates
 * an exception handler so big that the library converts the goto to goto_w
 * and therefore must recalculate the addresses of the other instructions
 * and the entries in the exception table.
 */

int main(){
  JVM_METHOD *main_method, *foo_method;
  JVM_CLASS *cur_class_file;
  JVM_CODE_GRAPH_NODE *if_node, *start, *end, *handler;
  int str_idx, meth_idx, field_idx;
  JVM_EXCEPTION_TABLE_ENTRY *et_entry;
  char *io_str, *arith_str;
  int i, elvnum;

  io_str = "io exception";
  arith_str = "arithmetic exception";

  cur_class_file = bc_new_class("ExceptionTableRecalc", "asdf.f", NULL, NULL, 
      JVM_ACC_PUBLIC|JVM_ACC_SUPER);

  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  foo_method = bc_new_method(cur_class_file, "a",
     "(I)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);
  bc_add_method_exception(foo_method, "java.io.IOException");
  bc_add_method_exception(foo_method, "java.lang.ArithmeticException");

  bc_append(foo_method,jvm_iload_0);
  if_node = bc_append(foo_method,jvm_ifne);

  bc_gen_new_obj_dup(foo_method, "java.io.IOException");

  str_idx = cp_find_or_insert(cur_class_file, CONSTANT_String, io_str);

  bc_append(foo_method, jvm_ldc, str_idx);

  meth_idx = bc_new_methodref(cur_class_file, "java.io.IOException",
     "<init>", "(Ljava.lang.String;)V");

  bc_append(foo_method, jvm_invokespecial, meth_idx);
  bc_append(foo_method,jvm_athrow);

  bc_set_branch_target(if_node, bc_append(foo_method,jvm_iload_0));
  bc_append(foo_method,jvm_iconst_1);
  if_node = bc_append(foo_method,jvm_if_icmpne);

  bc_gen_new_obj(foo_method, "java.lang.ArithmeticException");
  bc_append(foo_method,jvm_dup);

  str_idx = cp_find_or_insert(cur_class_file, CONSTANT_String, arith_str);

  bc_append(foo_method, jvm_ldc, str_idx);

  meth_idx = bc_new_methodref(cur_class_file, "java.lang.ArithmeticException",
     "<init>", "(Ljava.lang.String;)V");

  bc_append(foo_method, jvm_invokespecial, meth_idx);
  bc_append(foo_method,jvm_athrow);

  bc_set_branch_target(if_node, bc_gen_return(foo_method));

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  /* iconst_0: */
  start = bc_push_int_const(main_method, 0);

  /* invokestatic <Method void a(int)>: */
  meth_idx = bc_new_methodref(cur_class_file, "ExceptionTableRecalc",
     "a", "(I)V");
  bc_append(main_method, jvm_invokestatic, meth_idx);

  /* goto: */
  end = bc_append(main_method, jvm_goto);

  /*** the code for the exception handler begins here ***/

  elvnum = bc_get_next_local(main_method, jvm_Object);
  handler = bc_gen_store_op(main_method, elvnum, jvm_Object);

  /* getstatic (System.out):  */
  field_idx = bc_new_fieldref(cur_class_file, "java.lang.System",
     "out", "Ljava.io.PrintStream;");

  bc_append(main_method, jvm_getstatic, field_idx);

  /* load the arg to println():  */
  bc_push_string_const(main_method, "caught io exception");

  /* now invoke println():  */
  meth_idx = bc_new_methodref(cur_class_file, "java.io.PrintStream",
     "println", "(Ljava.lang.String;)V");

  bc_append(main_method, jvm_invokevirtual, meth_idx);

  et_entry = bc_new_exception_table_entry(main_method, start, end, handler, 
     "java.io.IOException");
  bc_add_exception_handler(main_method, et_entry);

  /* the exception handler falls through to the following code */

  /* iconst_0: */
  bc_set_branch_target(end, start = bc_push_int_const(main_method, 1));

  /* invokestatic <Method void a(int)>: */
  meth_idx = bc_new_methodref(cur_class_file, "ExceptionTableRecalc",
     "a", "(I)V");
  bc_append(main_method, jvm_invokestatic, meth_idx);

  /* goto: */
  end = bc_append(main_method, jvm_goto);

  /*** the code for the exception handler begins here ***/
  
  elvnum = bc_get_next_local(main_method, jvm_Object);
  handler = bc_gen_store_op(main_method, elvnum, jvm_Object);
  
  /* Generate enough nop instructions so that the goto which branches over
   * this exception hanlder will have to be changed to goto_w.
   */

  for(i=0;i<32768;i++)
    bc_append(main_method, jvm_nop); 

  /* getstatic (System.out):  */
  field_idx = bc_new_fieldref(cur_class_file, "java.lang.System",
     "out", "Ljava.io.PrintStream;");

  bc_append(main_method, jvm_getstatic, field_idx);

  /* load the arg to println():  */
  bc_push_string_const(main_method, "caught arithmetic exception");

  /* now invoke println():  */
  meth_idx = bc_new_methodref(cur_class_file, "java.io.PrintStream",
     "println", "(Ljava.lang.String;)V");

  bc_append(main_method, jvm_invokevirtual, meth_idx);

  /* return from the exception handler */
  bc_gen_return(main_method);

  et_entry = bc_new_exception_table_entry(main_method, start, end, handler,
     "java.lang.ArithmeticException");
  bc_add_exception_handler(main_method, et_entry);

  /* getstatic (System.out):  */
  field_idx = bc_new_fieldref(cur_class_file, "java.lang.System",
     "out", "Ljava.io.PrintStream;");

  bc_set_branch_target(end, bc_append(main_method, jvm_getstatic, field_idx));

  /* load "after handler", the arg to println():  */
  bc_push_string_const(main_method, "after handler");

  /* now invoke println():  */
  meth_idx = bc_new_methodref(cur_class_file, "java.io.PrintStream",
     "println", "(Ljava.lang.String;)V");

  bc_append(main_method, jvm_invokevirtual, meth_idx);

  /* return from the exception handler */
  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}
