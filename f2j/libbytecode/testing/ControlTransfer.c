#include <stdio.h>
#include "bytecode.h"

/*
 * Generates various control transfer instructions mentioned in
 * section 3.11.7 of the JVM spec except for the following:
 *  -goto and goto_w which are tested in ExceptionTable.c
 *  -tableswitch which is tested in Tableswitch.c
 *  -lookupswitch which is tested in Lookupswitch.c
 */

int main(){
  JVM_METHOD *main_method;
  JVM_CLASS *cur_class_file;
  int meth_idx, out_idx, in_idx;
  JVM_CODE_GRAPH_NODE *target, *ifeq, *iflt, *ifle, *ifne, *ifgt, *ifge, 
    *ifnull, *ifnonnull, *if_icmpeq, *if_icmplt, *if_icmple, *if_icmpne, 
    *if_icmpgt, *if_icmpge, *if_acmpeq, *if_acmpne, *jsr, *jsr2;
  int i, retvar;

  cur_class_file = bc_new_class("ControlTransfer", "asdf.f", NULL, 
      NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER);
  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  jsr = bc_append(main_method, jvm_jsr);

  in_idx = bc_new_fieldref(cur_class_file, "java.lang.System",
     "in", "Ljava.io.InputStream;");
  out_idx = bc_new_fieldref(cur_class_file, "java.lang.System",
     "out", "Ljava.io.PrintStream;");

  bc_push_int_const(main_method, 5);
  ifeq = bc_append(main_method, jvm_ifeq);

  jsr2 = bc_append(main_method, jvm_jsr);

  bc_push_int_const(main_method, 5);
  iflt = bc_append(main_method, jvm_iflt);

  bc_push_int_const(main_method, 5);
  ifle = bc_append(main_method, jvm_ifle);

  bc_push_int_const(main_method, 0);
  ifne = bc_append(main_method, jvm_ifne);

  bc_push_int_const(main_method, -5);
  ifgt = bc_append(main_method, jvm_ifgt);

  bc_push_int_const(main_method, -5);
  ifge = bc_append(main_method, jvm_ifge);

  bc_append(main_method, jvm_getstatic, out_idx);
  ifnull = bc_append(main_method, jvm_ifnull);

  bc_push_null_const(main_method);
  ifnonnull = bc_append(main_method, jvm_ifnonnull);

  bc_push_int_const(main_method, 5);
  bc_push_int_const(main_method, 4);
  if_icmpeq = bc_append(main_method, jvm_if_icmpeq);

  bc_push_int_const(main_method, 5);
  bc_push_int_const(main_method, 4);
  if_icmplt = bc_append(main_method, jvm_if_icmplt);

  bc_push_int_const(main_method, 5);
  bc_push_int_const(main_method, 4);
  if_icmple = bc_append(main_method, jvm_if_icmple);

  bc_push_int_const(main_method, 4);
  bc_push_int_const(main_method, 4);
  if_icmpne = bc_append(main_method, jvm_if_icmpne);

  bc_push_int_const(main_method, 4);
  bc_push_int_const(main_method, 5);
  if_icmpgt = bc_append(main_method, jvm_if_icmpgt);

  bc_push_int_const(main_method, 4);
  bc_push_int_const(main_method, 5);
  if_icmpge = bc_append(main_method, jvm_if_icmpge);

  bc_append(main_method, jvm_getstatic, out_idx);
  bc_append(main_method, jvm_getstatic, in_idx);
  if_acmpeq = bc_append(main_method, jvm_if_acmpeq);

  bc_append(main_method, jvm_getstatic, out_idx);
  bc_append(main_method, jvm_getstatic, out_idx);
  if_acmpne = bc_append(main_method, jvm_if_acmpne);

  meth_idx = bc_new_methodref(cur_class_file, "java.io.PrintStream",
     "println", "(Ljava.lang.String;)V");

  bc_append(main_method, jvm_getstatic, out_idx);
  bc_push_string_const(main_method, "good");
  bc_append(main_method, jvm_invokevirtual, meth_idx);

  bc_gen_return(main_method);

  target = bc_append(main_method, jvm_getstatic, out_idx);
  bc_push_string_const(main_method, "bad");
  bc_append(main_method, jvm_invokevirtual, meth_idx);

  bc_gen_return(main_method);

  bc_set_branch_target(ifeq, target);
  bc_set_branch_target(iflt, target);
  bc_set_branch_target(ifle, target);
  bc_set_branch_target(ifne, target);
  bc_set_branch_target(ifgt, target);
  bc_set_branch_target(ifge, target);
  bc_set_branch_target(ifnull, target);
  bc_set_branch_target(ifnonnull, target);
  bc_set_branch_target(if_icmpeq, target);
  bc_set_branch_target(if_icmplt, target);
  bc_set_branch_target(if_icmple, target);
  bc_set_branch_target(if_icmpne, target);
  bc_set_branch_target(if_icmpgt, target);
  bc_set_branch_target(if_icmpge, target);
  bc_set_branch_target(if_acmpeq, target);
  bc_set_branch_target(if_acmpne, target);

  retvar = bc_get_next_local(main_method, jvm_Int);
  target = bc_gen_store_op(main_method, retvar, jvm_Object);
  bc_append(main_method, jvm_getstatic, out_idx);
  bc_push_string_const(main_method, "in subroutine 1");
  bc_append(main_method, jvm_invokevirtual, meth_idx);
  bc_append(main_method, jvm_ret, retvar);

  bc_set_branch_target(jsr, target);

  /* Generate enough nop instructions so that the jsr which branches over
   * the first embedded subroutine will have to be changed to jsr_w.
   */
  
  for(i=0;i<32768;i++)
    bc_append(main_method, jvm_nop);
  
  target = bc_gen_store_op(main_method, retvar, jvm_Object);
  bc_append(main_method, jvm_getstatic, out_idx);
  bc_push_string_const(main_method, "in subroutine 2");
  bc_append(main_method, jvm_invokevirtual, meth_idx);
  bc_append(main_method, jvm_ret, retvar);

  bc_set_branch_target(jsr2, target);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}
