#include <stdio.h>
#include "bytecode.h"

/*
 * Generate two switch blocks with a large span in the case numbers so
 * that a lookupswitch instruction will be used instead of tableswitch.
 * The second switch is after a goto which will get converted to a goto_w, 
 * so we can test whether the second switch padding and branch targets 
 * get recalculated correctly.
 */

int main() {
  JVM_METHOD *main_method;
  JVM_CLASS *cur_class_file;
  int meth_idx, out_idx;
  JVM_CODE_GRAPH_NODE *tswitch, *case1, *case2, *case1_break, *case2_break,
    *target, *goto_node, *after_goto;
  int i;

  cur_class_file = bc_new_class("LookupSwitch", "asdf.f", NULL, 
      NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER);
  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  out_idx = bc_new_fieldref(cur_class_file, "java.lang.System",
     "out", "Ljava.io.PrintStream;");

  meth_idx = bc_new_methodref(cur_class_file, "java.io.PrintStream",
     "println", "(Ljava.lang.String;)V");

  bc_push_int_const(main_method, 2);
  tswitch = bc_gen_switch(main_method);

  case1 = bc_append(main_method, jvm_getstatic, out_idx);
  bc_add_switch_case(tswitch, case1, 100);
  bc_push_string_const(main_method, "one hundred");
  bc_append(main_method, jvm_invokevirtual, meth_idx);
  case1_break = bc_append(main_method, jvm_goto);

  case2 = bc_append(main_method, jvm_getstatic, out_idx);
  bc_add_switch_case(tswitch, case2, 2);
  bc_push_string_const(main_method, "two");
  bc_append(main_method, jvm_invokevirtual, meth_idx);

  target = bc_append(main_method, jvm_getstatic, out_idx);
  bc_add_switch_default(tswitch, target);
  bc_push_string_const(main_method, "after switch");
  bc_append(main_method, jvm_invokevirtual, meth_idx);

  bc_set_branch_target(case1_break, target);

  goto_node = bc_append(main_method, jvm_goto);

  after_goto = bc_push_int_const(main_method, 2);
  tswitch = bc_gen_switch(main_method);

  case1 = bc_append(main_method, jvm_getstatic, out_idx);
  bc_add_switch_case(tswitch, case1, 100);
  bc_push_string_const(main_method, "one hundred");
  bc_append(main_method, jvm_invokevirtual, meth_idx);
  case1_break = bc_append(main_method, jvm_goto);

  case2 = bc_append(main_method, jvm_getstatic, out_idx);
  bc_add_switch_case(tswitch, case2, 2);
  bc_push_string_const(main_method, "two");
  bc_append(main_method, jvm_invokevirtual, meth_idx);

  target = bc_append(main_method, jvm_getstatic, out_idx);
  bc_add_switch_default(tswitch, target);
  bc_push_string_const(main_method, "after switch");
  bc_append(main_method, jvm_invokevirtual, meth_idx);

  bc_set_branch_target(case1_break, target);

  /* now generate a switch with a missing case to determine whether
   * the missing cases are generated properly.
   */

  bc_push_int_const(main_method, 6);
  tswitch = bc_gen_switch(main_method);

  case1 = bc_append(main_method, jvm_getstatic, out_idx);
  bc_add_switch_case(tswitch, case1, 500);
  bc_push_string_const(main_method, "five hundred");
  bc_append(main_method, jvm_invokevirtual, meth_idx);
  case1_break = bc_append(main_method, jvm_goto);

  case2 = bc_append(main_method, jvm_getstatic, out_idx);
  bc_add_switch_case(tswitch, case2, 6);
  bc_push_string_const(main_method, "six");
  bc_append(main_method, jvm_invokevirtual, meth_idx);
  case2_break = bc_append(main_method, jvm_goto);

  /* case 7 intentionally missing */

  case2 = bc_append(main_method, jvm_getstatic, out_idx);
  bc_add_switch_case(tswitch, case2, 8);
  bc_push_string_const(main_method, "eight");
  bc_append(main_method, jvm_invokevirtual, meth_idx);

  target = bc_append(main_method, jvm_getstatic, out_idx);
  bc_add_switch_default(tswitch, target);
  bc_push_string_const(main_method, "after switch");
  bc_append(main_method, jvm_invokevirtual, meth_idx);

  bc_set_branch_target(case1_break, target);
  bc_set_branch_target(case2_break, target);

  bc_gen_return(main_method);

  /* Generate enough nop instructions so that the goto which branches over
   * the second switch block will have to be changed to goto_w.  this is
   * done to make sure that the padding and instruction width are properly
   * recalculated.
   */

  for(i=0;i<32768;i++)
    bc_append(main_method, jvm_nop);

  target = bc_append(main_method, jvm_goto);
  bc_set_branch_target(target, after_goto);
  bc_set_branch_target(goto_node, target);

  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}
