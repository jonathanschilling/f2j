#include <stdio.h>
#include "bytecode.h"

/* 
 * Generates two exception handlers and the exception table entries
 * for them.
 */

int main(){
  JVM_METHOD *main_method, *foo_method;
  JVM_CLASS *cur_class_file;
  JVM_CODE_GRAPH_NODE *if_node, *start, *end, *end2, *handler;
  int str_idx, meth_idx, field_idx;
  JVM_EXCEPTION_TABLE_ENTRY *et_entry;
  char *io_str, *arith_str;
  int elvnum;

  io_str = "io exception";
  arith_str = "arithmetic exception";

  cur_class_file = bc_new_class("Synchronization", "asdf.f", NULL, NULL, 
      JVM_ACC_PUBLIC|JVM_ACC_SUPER);

  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  foo_method = bc_new_method(cur_class_file, "foo",
     "()V", JVM_ACC_PUBLIC);

  bc_gen_load_op(foo_method, 0, jvm_Object);
  bc_append(foo_method,jvm_monitorenter);

  /* getstatic (System.out):  */ 
  field_idx = bc_new_fieldref(cur_class_file, "java.lang.System",
     "out", "Ljava.io.PrintStream;");
  
  start = bc_append(foo_method, jvm_getstatic, field_idx);

  bc_push_string_const(foo_method, "hello...");

  /* now invoke println():  */
  meth_idx = bc_new_methodref(cur_class_file, "java.io.PrintStream",
     "println", "(Ljava.lang.String;)V");

  bc_append(foo_method, jvm_invokevirtual, meth_idx);

  bc_gen_load_op(foo_method, 0, jvm_Object);
  bc_append(foo_method,jvm_monitorexit);

  end = bc_gen_return(foo_method);

  handler = bc_gen_store_op(foo_method, 1, jvm_Object);
  bc_gen_load_op(foo_method, 0, jvm_Object);
  bc_append(foo_method,jvm_monitorexit);
  end2 = bc_gen_load_op(foo_method, 1, jvm_Object);
  bc_append(foo_method,jvm_athrow);

  et_entry = bc_new_exception_table_entry(foo_method, start, end, handler, NULL);
  bc_add_exception_handler(foo_method, et_entry);
  et_entry = bc_new_exception_table_entry(foo_method, handler, end2, handler, NULL);
  bc_add_exception_handler(foo_method, et_entry);

  bc_gen_return(foo_method);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  bc_gen_new_obj_dup(main_method, "Synchronization");
  meth_idx = bc_new_methodref(cur_class_file, "Synchronization",
     "<init>", "()V");
  bc_append(main_method, jvm_invokespecial, meth_idx);
  
  meth_idx = bc_new_methodref(cur_class_file, "Synchronization",
     "foo", "()V");
  bc_append(main_method, jvm_invokevirtual, meth_idx);

  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}
