#include <stdio.h>
#include "bytecode.h"

/* 
 * Generates some of the method invocation and return instructions
 * mentioned in section 3.11.8 of the JVM spec.  Most of the method
 * invocation instructions are generated in other test programs, so
 * those are not tested here.  This program primarily tests the 
 * bc_gen_return function to make sure that the correct return instruction
 * is generated for the given method descriptor.
 */

int main() {
  JVM_METHOD *main_method, *int_method, *long_method, *float_method,
     *double_method, *obj_method, *int_method_nodesc, *long_method_nodesc, 
     *float_method_nodesc, *double_method_nodesc, *obj_method_nodesc;
  JVM_CLASS *cur_class_file;
  int meth_idx;

  cur_class_file = bc_new_class("MethodInvocation", "asdf.f", NULL, NULL, 
      JVM_ACC_PUBLIC|JVM_ACC_SUPER);

  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  int_method = bc_new_method(cur_class_file, "int_method",
     "()I", JVM_ACC_PUBLIC|JVM_ACC_STATIC);
  bc_push_int_const(int_method, 1234);
  bc_gen_return(int_method);

  long_method = bc_new_method(cur_class_file, "long_method",
     "()J", JVM_ACC_PUBLIC|JVM_ACC_STATIC);
  bc_push_long_const(long_method, 4321);
  bc_gen_return(long_method);

  float_method = bc_new_method(cur_class_file, "float_method",
     "()F", JVM_ACC_PUBLIC|JVM_ACC_STATIC);
  bc_push_float_const(float_method, 9.9);
  bc_gen_return(float_method);

  double_method = bc_new_method(cur_class_file, "double_method",
     "()D", JVM_ACC_PUBLIC|JVM_ACC_STATIC);
  bc_push_double_const(double_method, 8.8);
  bc_gen_return(double_method);

  obj_method = bc_new_method(cur_class_file, "obj_method",
     "()Ljava/lang/String;", JVM_ACC_PUBLIC|JVM_ACC_STATIC);
  bc_push_string_const(obj_method, "blah");
  bc_gen_return(obj_method);

  int_method_nodesc = bc_new_method(cur_class_file, "int_method_nodesc",
     NULL, JVM_ACC_PUBLIC|JVM_ACC_STATIC);
  bc_push_int_const(int_method_nodesc, 1234);
  bc_gen_return(int_method_nodesc);
  bc_set_method_descriptor(int_method_nodesc, "()I");

  long_method_nodesc = bc_new_method(cur_class_file, "long_method_nodesc",
     NULL, JVM_ACC_PUBLIC|JVM_ACC_STATIC);
  bc_push_long_const(long_method_nodesc, 4321);
  bc_gen_return(long_method_nodesc);
  bc_set_method_descriptor(long_method_nodesc, "()J");

  float_method_nodesc = bc_new_method(cur_class_file, "float_method_nodesc",
     NULL, JVM_ACC_PUBLIC|JVM_ACC_STATIC);
  bc_push_float_const(float_method_nodesc, 9.9);
  bc_gen_return(float_method_nodesc);
  bc_set_method_descriptor(float_method_nodesc, "()F");

  double_method_nodesc = bc_new_method(cur_class_file, "double_method_nodesc",
     NULL, JVM_ACC_PUBLIC|JVM_ACC_STATIC);
  bc_push_double_const(double_method_nodesc, 8.8);
  bc_gen_return(double_method_nodesc);
  bc_set_method_descriptor(double_method_nodesc, "()D");

  obj_method_nodesc = bc_new_method(cur_class_file, "obj_method_nodesc",
     NULL, JVM_ACC_PUBLIC|JVM_ACC_STATIC);
  bc_push_string_const(obj_method_nodesc, "blah");
  bc_gen_return(obj_method_nodesc);
  bc_set_method_descriptor(obj_method_nodesc, "()Ljava/lang/String;");

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  meth_idx = bc_new_methodref(cur_class_file, "MethodInvocation",
     "int_method", "()I");
  bc_append(main_method, jvm_invokestatic, meth_idx);

  meth_idx = bc_new_methodref(cur_class_file, "MethodInvocation",
     "long_method", "()J");
  bc_append(main_method, jvm_invokestatic, meth_idx);

  meth_idx = bc_new_methodref(cur_class_file, "MethodInvocation",
     "float_method", "()F");
  bc_append(main_method, jvm_invokestatic, meth_idx);

  meth_idx = bc_new_methodref(cur_class_file, "MethodInvocation",
     "double_method", "()D");
  bc_append(main_method, jvm_invokestatic, meth_idx);

  meth_idx = bc_new_methodref(cur_class_file, "MethodInvocation",
     "obj_method", "()Ljava/lang/String;");
  bc_append(main_method, jvm_invokestatic, meth_idx);

  meth_idx = bc_new_methodref(cur_class_file, "MethodInvocation",
     "int_method_nodesc", "()I");
  bc_append(main_method, jvm_invokestatic, meth_idx);

  meth_idx = bc_new_methodref(cur_class_file, "MethodInvocation",
     "long_method_nodesc", "()J");
  bc_append(main_method, jvm_invokestatic, meth_idx);

  meth_idx = bc_new_methodref(cur_class_file, "MethodInvocation",
     "float_method_nodesc", "()F");
  bc_append(main_method, jvm_invokestatic, meth_idx);

  meth_idx = bc_new_methodref(cur_class_file, "MethodInvocation",
     "double_method_nodesc", "()D");
  bc_append(main_method, jvm_invokestatic, meth_idx);

  meth_idx = bc_new_methodref(cur_class_file, "MethodInvocation",
     "obj_method_nodesc", "()Ljava/lang/String;");
  bc_append(main_method, jvm_invokestatic, meth_idx);

  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}
