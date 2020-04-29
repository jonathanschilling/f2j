#include <stdio.h>
#include "bytecode.h"

/*
 * Generates goto instructions with 'labeled' targets.
 */

int main(){
  JVM_METHOD *main_method;
  JVM_CLASS *cur_class_file;
  JVM_CODE_GRAPH_NODE *goto_node, *ret_node, *foo;
  int meth_idx, out_idx;

  cur_class_file = bc_new_class("Goto", "asdf.f", NULL, 
      NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER);
  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  out_idx = bc_new_fieldref(cur_class_file, "java.lang.System",
     "out", "Ljava.io.PrintStream;");

  meth_idx = bc_new_methodref(cur_class_file, "java.io.PrintStream",
     "println", "(Ljava.lang.String;)V");

  goto_node = bc_append(main_method, jvm_goto);
  bc_set_branch_label(goto_node, "foo_label");

  foo = bc_append(main_method, jvm_getstatic, out_idx);
  bc_push_string_const(main_method, "bad");
  bc_append(main_method, jvm_invokevirtual, meth_idx);

  foo = bc_append(main_method, jvm_getstatic, out_idx);
  bc_push_string_const(main_method, "good");
  bc_append(main_method, jvm_invokevirtual, meth_idx);

  bc_associate_branch_label(main_method, foo, "foo_label");

  goto_node = bc_append(main_method, jvm_goto);
  bc_set_integer_branch_label(goto_node, 1234);

  foo = bc_append(main_method, jvm_getstatic, out_idx);
  bc_push_string_const(main_method, "bad");
  bc_append(main_method, jvm_invokevirtual, meth_idx);

  foo = bc_append(main_method, jvm_getstatic, out_idx);
  bc_push_string_const(main_method, "good");
  bc_append(main_method, jvm_invokevirtual, meth_idx);

  bc_gen_return(main_method);

  bc_associate_integer_branch_label(main_method, foo, 1234);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}
