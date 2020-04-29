#include <stdio.h>
#include "bytecode.h"

/*
 * Generates the various operand stack management instructions 
 * mentioned in section 3.11.6 of the JVM spec.
 */

int main(){
  JVM_METHOD *main_method;
  JVM_CLASS *cur_class_file;
  int i;

  cur_class_file = bc_new_class("StackManagement", "asdf.f", NULL, 
      NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER);
  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  /* pop instruction */
  bc_push_int_const(main_method, 300);
  bc_append(main_method, jvm_pop);

  /* Form 1 of pop2 instruction */
  bc_push_int_const(main_method, 400);
  bc_push_int_const(main_method, 500);
  bc_append(main_method, jvm_pop2);

  /* Form 2 of pop2 instruction */
  bc_push_double_const(main_method, 34.61);
  bc_append(main_method, jvm_pop2);

  /* dup instruction */
  bc_push_int_const(main_method, 300);
  bc_append(main_method, jvm_dup);
  bc_append(main_method, jvm_pop2);

  /* dup_x1 instruction */
  bc_push_int_const(main_method, 100);
  bc_push_int_const(main_method, 200);
  bc_append(main_method, jvm_dup_x1);
  bc_append(main_method, jvm_pop);
  bc_append(main_method, jvm_pop2);

  /* Form 1 of dup_x2 instruction: */
  bc_push_int_const(main_method, 100);
  bc_push_int_const(main_method, 200);
  bc_push_int_const(main_method, 300);
  bc_append(main_method, jvm_dup_x2);
  bc_append(main_method, jvm_pop2);
  bc_append(main_method, jvm_pop2);
  
  /* Form 2 of dup_x2 instruction */
  bc_push_double_const(main_method, 34.61);
  bc_push_int_const(main_method, 200);
  bc_append(main_method, jvm_dup_x2);
  bc_append(main_method, jvm_pop);
  bc_append(main_method, jvm_pop2);
  bc_append(main_method, jvm_pop);

  /* Form 1 of dup2 instruction */
  bc_push_int_const(main_method, 200);
  bc_push_int_const(main_method, 300);
  bc_append(main_method, jvm_dup2);
  bc_append(main_method, jvm_pop2);
  bc_append(main_method, jvm_pop2);

  /* Form 2 of dup2 instruction */
  bc_push_double_const(main_method, 34.61);
  bc_append(main_method, jvm_dup2);
  bc_append(main_method, jvm_pop2);

  /* Form 1 of dup2_x1 instruction */
  bc_push_int_const(main_method, 200);
  bc_push_int_const(main_method, 300);
  bc_push_int_const(main_method, 400);
  bc_append(main_method, jvm_dup2_x1);
  bc_append(main_method, jvm_pop2);
  bc_append(main_method, jvm_pop2);
  bc_append(main_method, jvm_pop);

  /* Form 2 of dup2_x1 instruction */
  bc_push_int_const(main_method, 400);
  bc_push_double_const(main_method, 34.61);
  bc_append(main_method, jvm_dup2_x1);
  bc_append(main_method, jvm_pop2);
  bc_append(main_method, jvm_pop);
  bc_append(main_method, jvm_pop2);

  /* Form 1 of dup2_x2 instruction */
  bc_push_int_const(main_method, 100);
  bc_push_int_const(main_method, 200);
  bc_push_int_const(main_method, 300);
  bc_push_int_const(main_method, 400);
  bc_append(main_method, jvm_dup2_x2);
  bc_append(main_method, jvm_pop2);
  bc_append(main_method, jvm_pop2);
  bc_append(main_method, jvm_pop2);

  /* Form 2 of dup2_x2 instruction */
  bc_push_int_const(main_method, 300);
  bc_push_int_const(main_method, 400);
  bc_push_double_const(main_method, 34.61);
  bc_append(main_method, jvm_dup2_x2);
  bc_append(main_method, jvm_pop2);
  bc_append(main_method, jvm_pop2);
  bc_append(main_method, jvm_pop2);

  /* Form 3 of dup2_x2 instruction */
  bc_push_double_const(main_method, 34.61);
  bc_push_int_const(main_method, 300);
  bc_push_int_const(main_method, 400);
  bc_append(main_method, jvm_dup2_x2);
  bc_append(main_method, jvm_pop2);
  bc_append(main_method, jvm_pop2);
  bc_append(main_method, jvm_pop2);

  /* Form 4 of dup2_x2 instruction */
  bc_push_double_const(main_method, 34.61);
  bc_push_double_const(main_method, 34.61);
  bc_append(main_method, jvm_dup2_x2);
  bc_append(main_method, jvm_pop2);
  bc_append(main_method, jvm_pop2);
  bc_append(main_method, jvm_pop2);

  /* swap instruction */
  bc_push_int_const(main_method, 300);
  bc_push_int_const(main_method, 400);
  bc_append(main_method, jvm_swap);
  bc_append(main_method, jvm_pop2);

  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}
