#include <stdio.h>
#include "bytecode.h"

void gen_add_test(JVM_CLASS *);
void gen_sub_test(JVM_CLASS *);
void gen_mul_test(JVM_CLASS *);
void gen_div_test(JVM_CLASS *);
void gen_rem_test(JVM_CLASS *);
void gen_neg_test(JVM_CLASS *);
void gen_shift_test(JVM_CLASS *);
void gen_bitwise_or_test(JVM_CLASS *);
void gen_bitwise_and_test(JVM_CLASS *);
void gen_bitwise_xor_test(JVM_CLASS *);
void gen_comparison_test(JVM_CLASS *);

/*
 * Generates all the various arithmetic instructions mentioned in
 * section 3.11.3 of the JVM spec (except for iinc, which has its
 * own test case Iinc.c).
 */

int main(){
  JVM_METHOD *main_method;
  JVM_CLASS *cur_class_file;
  int meth_idx;

  cur_class_file = bc_new_class("Arithmetic", "asdf.f", NULL, 
      NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER);
  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  meth_idx = bc_new_methodref(cur_class_file, "Arithmetic",
     "test_add", "()V");
  bc_append(main_method, jvm_invokestatic, meth_idx);

  meth_idx = bc_new_methodref(cur_class_file, "Arithmetic",
     "test_sub", "()V");
  bc_append(main_method, jvm_invokestatic, meth_idx);

  meth_idx = bc_new_methodref(cur_class_file, "Arithmetic",
     "test_mul", "()V");
  bc_append(main_method, jvm_invokestatic, meth_idx);

  meth_idx = bc_new_methodref(cur_class_file, "Arithmetic",
     "test_div", "()V");
  bc_append(main_method, jvm_invokestatic, meth_idx);

  meth_idx = bc_new_methodref(cur_class_file, "Arithmetic",
     "test_rem", "()V");
  bc_append(main_method, jvm_invokestatic, meth_idx);

  meth_idx = bc_new_methodref(cur_class_file, "Arithmetic",
     "test_neg", "()V");
  bc_append(main_method, jvm_invokestatic, meth_idx);

  meth_idx = bc_new_methodref(cur_class_file, "Arithmetic",
     "test_shift", "()V");
  bc_append(main_method, jvm_invokestatic, meth_idx);

  meth_idx = bc_new_methodref(cur_class_file, "Arithmetic",
     "test_bitwise_or", "()V");
  bc_append(main_method, jvm_invokestatic, meth_idx);

  meth_idx = bc_new_methodref(cur_class_file, "Arithmetic",
     "test_bitwise_and", "()V");
  bc_append(main_method, jvm_invokestatic, meth_idx);

  meth_idx = bc_new_methodref(cur_class_file, "Arithmetic",
     "test_bitwise_xor", "()V");
  bc_append(main_method, jvm_invokestatic, meth_idx);

  meth_idx = bc_new_methodref(cur_class_file, "Arithmetic",
     "test_comparison", "()V");
  bc_append(main_method, jvm_invokestatic, meth_idx);

  bc_gen_return(main_method);

  gen_add_test(cur_class_file);
  gen_sub_test(cur_class_file);
  gen_mul_test(cur_class_file);
  gen_div_test(cur_class_file);
  gen_rem_test(cur_class_file);
  gen_neg_test(cur_class_file);
  gen_shift_test(cur_class_file);
  gen_bitwise_or_test(cur_class_file);
  gen_bitwise_and_test(cur_class_file);
  gen_bitwise_xor_test(cur_class_file);
  gen_comparison_test(cur_class_file);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}

void gen_add_test(JVM_CLASS *cur_class_file)
{
  JVM_CODE_GRAPH_NODE *int_if, *long_if, *float_if, *double_if, *target;
  int i, i_one, i_two, l_one, l_two, f_one, f_two, d_one, d_two;
  JVM_METHOD *add_method;
  int meth_idx, field_idx;

  add_method = bc_new_method(cur_class_file, "test_add",
     "()V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  i_one = bc_get_next_local(add_method, jvm_Int);
  bc_push_int_const(add_method, 1);
  bc_gen_store_op(add_method, i_one, jvm_Int);

  i_two = bc_get_next_local(add_method, jvm_Int);
  bc_push_int_const(add_method, 2);
  bc_gen_store_op(add_method, i_two, jvm_Int);

  l_one = bc_get_next_local(add_method, jvm_Long);
  bc_push_long_const(add_method, 1);
  bc_gen_store_op(add_method, l_one, jvm_Long);

  l_two = bc_get_next_local(add_method, jvm_Long);
  bc_push_long_const(add_method, 2);
  bc_gen_store_op(add_method, l_two, jvm_Long);

  f_one = bc_get_next_local(add_method, jvm_Float);
  bc_push_float_const(add_method, 1.0);
  bc_gen_store_op(add_method, f_one, jvm_Float);

  f_two = bc_get_next_local(add_method, jvm_Float);
  bc_push_float_const(add_method, 2.0);
  bc_gen_store_op(add_method, f_two, jvm_Float);

  d_one = bc_get_next_local(add_method, jvm_Double);
  bc_push_double_const(add_method, 1.0);
  bc_gen_store_op(add_method, d_one, jvm_Double);

  d_two = bc_get_next_local(add_method, jvm_Double);
  bc_push_double_const(add_method, 2.0);
  bc_gen_store_op(add_method, d_two, jvm_Double);

  bc_gen_load_op(add_method, i_one, jvm_Int);
  bc_gen_load_op(add_method, i_one, jvm_Int);
  bc_append(add_method, jvm_iadd);
  bc_gen_load_op(add_method, i_two, jvm_Int);

  int_if = bc_append(add_method, jvm_if_icmpne);

  bc_gen_load_op(add_method, l_one, jvm_Long);
  bc_gen_load_op(add_method, l_one, jvm_Long);
  bc_append(add_method, jvm_ladd);
  bc_gen_load_op(add_method, l_two, jvm_Long);
  bc_append(add_method, jvm_lcmp);

  long_if = bc_append(add_method, jvm_ifne);

  bc_gen_load_op(add_method, f_one, jvm_Float);
  bc_gen_load_op(add_method, f_one, jvm_Float);
  bc_append(add_method, jvm_fadd);
  bc_gen_load_op(add_method, f_two, jvm_Float);
  bc_append(add_method, jvm_fcmpl);

  float_if = bc_append(add_method, jvm_ifne);

  bc_gen_load_op(add_method, d_one, jvm_Double);
  bc_gen_load_op(add_method, d_one, jvm_Double);
  bc_append(add_method, jvm_dadd);
  bc_gen_load_op(add_method, d_two, jvm_Double);
  bc_append(add_method, jvm_dcmpl);

  double_if = bc_append(add_method, jvm_ifne);

  /* getstatic (System.out):  */
  field_idx = bc_new_fieldref(cur_class_file, "java.lang.System",
     "out", "Ljava.io.PrintStream;");

  bc_append(add_method, jvm_getstatic, field_idx);

  /* load the arg to println():  */
  bc_push_string_const(add_method, "Addition:          OK");

  /* now invoke println():  */
  meth_idx = bc_new_methodref(cur_class_file, "java.io.PrintStream",
     "println", "(Ljava.lang.String;)V");

  bc_append(add_method, jvm_invokevirtual, meth_idx);

  bc_gen_return(add_method);

  target = bc_append(add_method, jvm_getstatic, field_idx);
  bc_push_string_const(add_method, "Addition:          Failed");
  bc_append(add_method, jvm_invokevirtual, meth_idx);

  bc_set_branch_target(int_if, target);
  bc_set_branch_target(long_if, target);
  bc_set_branch_target(float_if, target);
  bc_set_branch_target(double_if, target);

  bc_gen_return(add_method);
}

void gen_sub_test(JVM_CLASS *cur_class_file)
{
  JVM_CODE_GRAPH_NODE *int_if, *long_if, *float_if, *double_if, *target;
  int i, i_one, i_zero, l_one, l_zero, f_one, f_zero, d_one, d_zero;
  JVM_METHOD *sub_method;
  int meth_idx, field_idx;

  sub_method = bc_new_method(cur_class_file, "test_sub",
     "()V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  i_one = bc_get_next_local(sub_method, jvm_Int);
  bc_push_int_const(sub_method, 1);
  bc_gen_store_op(sub_method, i_one, jvm_Int);

  i_zero = bc_get_next_local(sub_method, jvm_Int);
  bc_push_int_const(sub_method, 0);
  bc_gen_store_op(sub_method, i_zero, jvm_Int);

  l_one = bc_get_next_local(sub_method, jvm_Long);
  bc_push_long_const(sub_method, 1);
  bc_gen_store_op(sub_method, l_one, jvm_Long);

  l_zero = bc_get_next_local(sub_method, jvm_Long);
  bc_push_long_const(sub_method, 0);
  bc_gen_store_op(sub_method, l_zero, jvm_Long);

  f_one = bc_get_next_local(sub_method, jvm_Float);
  bc_push_float_const(sub_method, 1.0);
  bc_gen_store_op(sub_method, f_one, jvm_Float);

  f_zero = bc_get_next_local(sub_method, jvm_Float);
  bc_push_float_const(sub_method, 0.0);
  bc_gen_store_op(sub_method, f_zero, jvm_Float);

  d_one = bc_get_next_local(sub_method, jvm_Double);
  bc_push_double_const(sub_method, 1.0);
  bc_gen_store_op(sub_method, d_one, jvm_Double);

  d_zero = bc_get_next_local(sub_method, jvm_Double);
  bc_push_double_const(sub_method, 0.0);
  bc_gen_store_op(sub_method, d_zero, jvm_Double);

  bc_gen_load_op(sub_method, i_one, jvm_Int);
  bc_gen_load_op(sub_method, i_one, jvm_Int);
  bc_append(sub_method, jvm_isub);
  bc_gen_load_op(sub_method, i_zero, jvm_Int);

  int_if = bc_append(sub_method, jvm_if_icmpne);

  bc_gen_load_op(sub_method, l_one, jvm_Long);
  bc_gen_load_op(sub_method, l_one, jvm_Long);
  bc_append(sub_method, jvm_lsub);
  bc_gen_load_op(sub_method, l_zero, jvm_Long);
  bc_append(sub_method, jvm_lcmp);

  long_if = bc_append(sub_method, jvm_ifne);

  bc_gen_load_op(sub_method, f_one, jvm_Float);
  bc_gen_load_op(sub_method, f_one, jvm_Float);
  bc_append(sub_method, jvm_fsub);
  bc_gen_load_op(sub_method, f_zero, jvm_Float);
  bc_append(sub_method, jvm_fcmpl);

  float_if = bc_append(sub_method, jvm_ifne);

  bc_gen_load_op(sub_method, d_one, jvm_Double);
  bc_gen_load_op(sub_method, d_one, jvm_Double);
  bc_append(sub_method, jvm_dsub);
  bc_gen_load_op(sub_method, d_zero, jvm_Double);
  bc_append(sub_method, jvm_dcmpl);

  double_if = bc_append(sub_method, jvm_ifne);

  /* getstatic (System.out):  */
  field_idx = bc_new_fieldref(cur_class_file, "java.lang.System",
     "out", "Ljava.io.PrintStream;");

  bc_append(sub_method, jvm_getstatic, field_idx);

  /* load the arg to println():  */
  bc_push_string_const(sub_method, "Subtraction:       OK");

  /* now invoke println():  */
  meth_idx = bc_new_methodref(cur_class_file, "java.io.PrintStream",
     "println", "(Ljava.lang.String;)V");

  bc_append(sub_method, jvm_invokevirtual, meth_idx);

  bc_gen_return(sub_method);

  target = bc_append(sub_method, jvm_getstatic, field_idx);
  bc_push_string_const(sub_method, "Subtraction:       Failed");
  bc_append(sub_method, jvm_invokevirtual, meth_idx);

  bc_set_branch_target(int_if, target);
  bc_set_branch_target(long_if, target);
  bc_set_branch_target(float_if, target);
  bc_set_branch_target(double_if, target);

  bc_gen_return(sub_method);
}

void gen_mul_test(JVM_CLASS *cur_class_file)
{
  JVM_CODE_GRAPH_NODE *int_if, *long_if, *float_if, *double_if, *target;
  int i, i_one, i_two, l_one, l_two, f_one, f_two, d_one, d_two;
  JVM_METHOD *mul_method;
  int meth_idx, field_idx;

  mul_method = bc_new_method(cur_class_file, "test_mul",
     "()V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  i_one = bc_get_next_local(mul_method, jvm_Int);
  bc_push_int_const(mul_method, 1);
  bc_gen_store_op(mul_method, i_one, jvm_Int);

  i_two = bc_get_next_local(mul_method, jvm_Int);
  bc_push_int_const(mul_method, 2);
  bc_gen_store_op(mul_method, i_two, jvm_Int);

  l_one = bc_get_next_local(mul_method, jvm_Long);
  bc_push_long_const(mul_method, 1);
  bc_gen_store_op(mul_method, l_one, jvm_Long);

  l_two = bc_get_next_local(mul_method, jvm_Long);
  bc_push_long_const(mul_method, 2);
  bc_gen_store_op(mul_method, l_two, jvm_Long);

  f_one = bc_get_next_local(mul_method, jvm_Float);
  bc_push_float_const(mul_method, 1.0);
  bc_gen_store_op(mul_method, f_one, jvm_Float);

  f_two = bc_get_next_local(mul_method, jvm_Float);
  bc_push_float_const(mul_method, 2.0);
  bc_gen_store_op(mul_method, f_two, jvm_Float);

  d_one = bc_get_next_local(mul_method, jvm_Double);
  bc_push_double_const(mul_method, 1.0);
  bc_gen_store_op(mul_method, d_one, jvm_Double);

  d_two = bc_get_next_local(mul_method, jvm_Double);
  bc_push_double_const(mul_method, 2.0);
  bc_gen_store_op(mul_method, d_two, jvm_Double);

  bc_gen_load_op(mul_method, i_one, jvm_Int);
  bc_gen_load_op(mul_method, i_two, jvm_Int);
  bc_append(mul_method, jvm_imul);
  bc_gen_load_op(mul_method, i_two, jvm_Int);

  int_if = bc_append(mul_method, jvm_if_icmpne);

  bc_gen_load_op(mul_method, l_one, jvm_Long);
  bc_gen_load_op(mul_method, l_two, jvm_Long);
  bc_append(mul_method, jvm_lmul);
  bc_gen_load_op(mul_method, l_two, jvm_Long);
  bc_append(mul_method, jvm_lcmp);

  long_if = bc_append(mul_method, jvm_ifne);

  bc_gen_load_op(mul_method, f_one, jvm_Float);
  bc_gen_load_op(mul_method, f_two, jvm_Float);
  bc_append(mul_method, jvm_fmul);
  bc_gen_load_op(mul_method, f_two, jvm_Float);
  bc_append(mul_method, jvm_fcmpl);

  float_if = bc_append(mul_method, jvm_ifne);

  bc_gen_load_op(mul_method, d_one, jvm_Double);
  bc_gen_load_op(mul_method, d_two, jvm_Double);
  bc_append(mul_method, jvm_dmul);
  bc_gen_load_op(mul_method, d_two, jvm_Double);
  bc_append(mul_method, jvm_dcmpl);

  double_if = bc_append(mul_method, jvm_ifne);

  /* getstatic (System.out):  */
  field_idx = bc_new_fieldref(cur_class_file, "java.lang.System",
     "out", "Ljava.io.PrintStream;");

  bc_append(mul_method, jvm_getstatic, field_idx);

  /* load the arg to println():  */
  bc_push_string_const(mul_method, "Multiplication:    OK");

  /* now invoke println():  */
  meth_idx = bc_new_methodref(cur_class_file, "java.io.PrintStream",
     "println", "(Ljava.lang.String;)V");

  bc_append(mul_method, jvm_invokevirtual, meth_idx);

  bc_gen_return(mul_method);

  target = bc_append(mul_method, jvm_getstatic, field_idx);
  bc_push_string_const(mul_method, "Multiplication:    Failed");
  bc_append(mul_method, jvm_invokevirtual, meth_idx);

  bc_set_branch_target(int_if, target);
  bc_set_branch_target(long_if, target);
  bc_set_branch_target(float_if, target);
  bc_set_branch_target(double_if, target);

  bc_gen_return(mul_method);
}

void gen_div_test(JVM_CLASS *cur_class_file)
{
  JVM_CODE_GRAPH_NODE *int_if, *long_if, *float_if, *double_if, *target;
  int i, i_four, i_two, l_four, l_two, f_four, f_two, d_four, d_two;
  JVM_METHOD *div_method;
  int meth_idx, field_idx;

  div_method = bc_new_method(cur_class_file, "test_div",
     "()V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  i_four = bc_get_next_local(div_method, jvm_Int);
  bc_push_int_const(div_method, 4);
  bc_gen_store_op(div_method, i_four, jvm_Int);

  i_two = bc_get_next_local(div_method, jvm_Int);
  bc_push_int_const(div_method, 2);
  bc_gen_store_op(div_method, i_two, jvm_Int);

  l_four = bc_get_next_local(div_method, jvm_Long);
  bc_push_long_const(div_method, 4);
  bc_gen_store_op(div_method, l_four, jvm_Long);

  l_two = bc_get_next_local(div_method, jvm_Long);
  bc_push_long_const(div_method, 2);
  bc_gen_store_op(div_method, l_two, jvm_Long);

  f_four = bc_get_next_local(div_method, jvm_Float);
  bc_push_float_const(div_method, 4.0);
  bc_gen_store_op(div_method, f_four, jvm_Float);

  f_two = bc_get_next_local(div_method, jvm_Float);
  bc_push_float_const(div_method, 2.0);
  bc_gen_store_op(div_method, f_two, jvm_Float);

  d_four = bc_get_next_local(div_method, jvm_Double);
  bc_push_double_const(div_method, 4.0);
  bc_gen_store_op(div_method, d_four, jvm_Double);

  d_two = bc_get_next_local(div_method, jvm_Double);
  bc_push_double_const(div_method, 2.0);
  bc_gen_store_op(div_method, d_two, jvm_Double);

  bc_gen_load_op(div_method, i_four, jvm_Int);
  bc_gen_load_op(div_method, i_two, jvm_Int);
  bc_append(div_method, jvm_idiv);
  bc_gen_load_op(div_method, i_two, jvm_Int);

  int_if = bc_append(div_method, jvm_if_icmpne);

  bc_gen_load_op(div_method, l_four, jvm_Long);
  bc_gen_load_op(div_method, l_two, jvm_Long);
  bc_append(div_method, jvm_ldiv);
  bc_gen_load_op(div_method, l_two, jvm_Long);
  bc_append(div_method, jvm_lcmp);

  long_if = bc_append(div_method, jvm_ifne);

  bc_gen_load_op(div_method, f_four, jvm_Float);
  bc_gen_load_op(div_method, f_two, jvm_Float);
  bc_append(div_method, jvm_fdiv);
  bc_gen_load_op(div_method, f_two, jvm_Float);
  bc_append(div_method, jvm_fcmpl);

  float_if = bc_append(div_method, jvm_ifne);

  bc_gen_load_op(div_method, d_four, jvm_Double);
  bc_gen_load_op(div_method, d_two, jvm_Double);
  bc_append(div_method, jvm_ddiv);
  bc_gen_load_op(div_method, d_two, jvm_Double);
  bc_append(div_method, jvm_dcmpl);

  double_if = bc_append(div_method, jvm_ifne);

  /* getstatic (System.out):  */
  field_idx = bc_new_fieldref(cur_class_file, "java.lang.System",
     "out", "Ljava.io.PrintStream;");

  bc_append(div_method, jvm_getstatic, field_idx);

  /* load the arg to println():  */
  bc_push_string_const(div_method, "Division:          OK");

  /* now invoke println():  */
  meth_idx = bc_new_methodref(cur_class_file, "java.io.PrintStream",
     "println", "(Ljava.lang.String;)V");

  bc_append(div_method, jvm_invokevirtual, meth_idx);

  bc_gen_return(div_method);

  target = bc_append(div_method, jvm_getstatic, field_idx);
  bc_push_string_const(div_method, "Division:          Failed");
  bc_append(div_method, jvm_invokevirtual, meth_idx);

  bc_set_branch_target(int_if, target);
  bc_set_branch_target(long_if, target);
  bc_set_branch_target(float_if, target);
  bc_set_branch_target(double_if, target);

  bc_gen_return(div_method);
}

void gen_rem_test(JVM_CLASS *cur_class_file)
{
  JVM_CODE_GRAPH_NODE *int_if, *long_if, *float_if, *double_if, *target;
  int i, i_one, i_two, i_eleven, l_eleven, l_one, l_two, f_eleven,
      f_one, f_two, d_eleven, d_one, d_two;
  JVM_METHOD *rem_method;
  int meth_idx, field_idx;

  rem_method = bc_new_method(cur_class_file, "test_rem",
     "()V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  i_one = bc_get_next_local(rem_method, jvm_Int);
  bc_push_int_const(rem_method, 1);
  bc_gen_store_op(rem_method, i_one, jvm_Int);

  i_two = bc_get_next_local(rem_method, jvm_Int);
  bc_push_int_const(rem_method, 2);
  bc_gen_store_op(rem_method, i_two, jvm_Int);

  i_eleven = bc_get_next_local(rem_method, jvm_Int);
  bc_push_int_const(rem_method, 11);
  bc_gen_store_op(rem_method, i_eleven, jvm_Int);

  l_one = bc_get_next_local(rem_method, jvm_Long);
  bc_push_long_const(rem_method, 1);
  bc_gen_store_op(rem_method, l_one, jvm_Long);

  l_two = bc_get_next_local(rem_method, jvm_Long);
  bc_push_long_const(rem_method, 2);
  bc_gen_store_op(rem_method, l_two, jvm_Long);

  l_eleven = bc_get_next_local(rem_method, jvm_Long);
  bc_push_long_const(rem_method, 11);
  bc_gen_store_op(rem_method, l_eleven, jvm_Long);

  f_one = bc_get_next_local(rem_method, jvm_Float);
  bc_push_float_const(rem_method, 1.0);
  bc_gen_store_op(rem_method, f_one, jvm_Float);

  f_two = bc_get_next_local(rem_method, jvm_Float);
  bc_push_float_const(rem_method, 2.0);
  bc_gen_store_op(rem_method, f_two, jvm_Float);

  f_eleven = bc_get_next_local(rem_method, jvm_Float);
  bc_push_float_const(rem_method, 11.0);
  bc_gen_store_op(rem_method, f_eleven, jvm_Float);

  d_one = bc_get_next_local(rem_method, jvm_Double);
  bc_push_double_const(rem_method, 1.0);
  bc_gen_store_op(rem_method, d_one, jvm_Double);

  d_two = bc_get_next_local(rem_method, jvm_Double);
  bc_push_double_const(rem_method, 2.0);
  bc_gen_store_op(rem_method, d_two, jvm_Double);

  d_eleven = bc_get_next_local(rem_method, jvm_Double);
  bc_push_double_const(rem_method, 11.0);
  bc_gen_store_op(rem_method, d_eleven, jvm_Double);

  bc_gen_load_op(rem_method, i_eleven, jvm_Int);
  bc_gen_load_op(rem_method, i_two, jvm_Int);
  bc_append(rem_method, jvm_irem);
  bc_gen_load_op(rem_method, i_one, jvm_Int);

  int_if = bc_append(rem_method, jvm_if_icmpne);

  bc_gen_load_op(rem_method, l_eleven, jvm_Long);
  bc_gen_load_op(rem_method, l_two, jvm_Long);
  bc_append(rem_method, jvm_lrem);
  bc_gen_load_op(rem_method, l_one, jvm_Long);
  bc_append(rem_method, jvm_lcmp);

  long_if = bc_append(rem_method, jvm_ifne);

  bc_gen_load_op(rem_method, f_eleven, jvm_Float);
  bc_gen_load_op(rem_method, f_two, jvm_Float);
  bc_append(rem_method, jvm_frem);
  bc_gen_load_op(rem_method, f_one, jvm_Float);
  bc_append(rem_method, jvm_fcmpl);

  float_if = bc_append(rem_method, jvm_ifne);

  bc_gen_load_op(rem_method, d_eleven, jvm_Double);
  bc_gen_load_op(rem_method, d_two, jvm_Double);
  bc_append(rem_method, jvm_drem);
  bc_gen_load_op(rem_method, d_one, jvm_Double);
  bc_append(rem_method, jvm_dcmpl);

  double_if = bc_append(rem_method, jvm_ifne);

  /* getstatic (System.out):  */
  field_idx = bc_new_fieldref(cur_class_file, "java.lang.System",
     "out", "Ljava.io.PrintStream;");

  bc_append(rem_method, jvm_getstatic, field_idx);

  /* load the arg to println():  */
  bc_push_string_const(rem_method, "Remainder:         OK");

  /* now invoke println():  */
  meth_idx = bc_new_methodref(cur_class_file, "java.io.PrintStream",
     "println", "(Ljava.lang.String;)V");

  bc_append(rem_method, jvm_invokevirtual, meth_idx);

  bc_gen_return(rem_method);

  target = bc_append(rem_method, jvm_getstatic, field_idx);
  bc_push_string_const(rem_method, "Remainder:         Failed");
  bc_append(rem_method, jvm_invokevirtual, meth_idx);

  bc_set_branch_target(int_if, target);
  bc_set_branch_target(long_if, target);
  bc_set_branch_target(float_if, target);
  bc_set_branch_target(double_if, target);

  bc_gen_return(rem_method);
}

void gen_neg_test(JVM_CLASS *cur_class_file)
{
  JVM_CODE_GRAPH_NODE *int_if, *long_if, *float_if, *double_if, *target;
  int i, i_negtwo, i_two, l_negtwo, l_two, f_negtwo, f_two, d_negtwo, d_two;
  JVM_METHOD *neg_method;
  int meth_idx, field_idx;

  neg_method = bc_new_method(cur_class_file, "test_neg",
     "()V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  i_negtwo = bc_get_next_local(neg_method, jvm_Int);
  bc_push_int_const(neg_method, -2);
  bc_gen_store_op(neg_method, i_negtwo, jvm_Int);

  i_two = bc_get_next_local(neg_method, jvm_Int);
  bc_push_int_const(neg_method, 2);
  bc_gen_store_op(neg_method, i_two, jvm_Int);

  l_negtwo = bc_get_next_local(neg_method, jvm_Long);
  bc_push_long_const(neg_method, -2);
  bc_gen_store_op(neg_method, l_negtwo, jvm_Long);

  l_two = bc_get_next_local(neg_method, jvm_Long);
  bc_push_long_const(neg_method, 2);
  bc_gen_store_op(neg_method, l_two, jvm_Long);

  f_negtwo = bc_get_next_local(neg_method, jvm_Float);
  bc_push_float_const(neg_method, -2.0);
  bc_gen_store_op(neg_method, f_negtwo, jvm_Float);

  f_two = bc_get_next_local(neg_method, jvm_Float);
  bc_push_float_const(neg_method, 2.0);
  bc_gen_store_op(neg_method, f_two, jvm_Float);

  d_negtwo = bc_get_next_local(neg_method, jvm_Double);
  bc_push_double_const(neg_method, -2.0);
  bc_gen_store_op(neg_method, d_negtwo, jvm_Double);

  d_two = bc_get_next_local(neg_method, jvm_Double);
  bc_push_double_const(neg_method, 2.0);
  bc_gen_store_op(neg_method, d_two, jvm_Double);

  bc_gen_load_op(neg_method, i_two, jvm_Int);
  bc_append(neg_method, jvm_ineg);
  bc_gen_load_op(neg_method, i_negtwo, jvm_Int);

  int_if = bc_append(neg_method, jvm_if_icmpne);

  bc_gen_load_op(neg_method, l_two, jvm_Long);
  bc_append(neg_method, jvm_lneg);
  bc_gen_load_op(neg_method, l_negtwo, jvm_Long);
  bc_append(neg_method, jvm_lcmp);

  long_if = bc_append(neg_method, jvm_ifne);

  bc_gen_load_op(neg_method, f_two, jvm_Float);
  bc_append(neg_method, jvm_fneg);
  bc_gen_load_op(neg_method, f_negtwo, jvm_Float);
  bc_append(neg_method, jvm_fcmpl);

  float_if = bc_append(neg_method, jvm_ifne);

  bc_gen_load_op(neg_method, d_two, jvm_Double);
  bc_append(neg_method, jvm_dneg);
  bc_gen_load_op(neg_method, d_negtwo, jvm_Double);
  bc_append(neg_method, jvm_dcmpl);

  double_if = bc_append(neg_method, jvm_ifne);

  /* getstatic (System.out):  */
  field_idx = bc_new_fieldref(cur_class_file, "java.lang.System",
     "out", "Ljava.io.PrintStream;");

  bc_append(neg_method, jvm_getstatic, field_idx);

  /* load the arg to println():  */
  bc_push_string_const(neg_method, "Negation:          OK");

  /* now invoke println():  */
  meth_idx = bc_new_methodref(cur_class_file, "java.io.PrintStream",
     "println", "(Ljava.lang.String;)V");

  bc_append(neg_method, jvm_invokevirtual, meth_idx);

  bc_gen_return(neg_method);

  target = bc_append(neg_method, jvm_getstatic, field_idx);
  bc_push_string_const(neg_method, "Negation:          Failed");
  bc_append(neg_method, jvm_invokevirtual, meth_idx);

  bc_set_branch_target(int_if, target);
  bc_set_branch_target(long_if, target);
  bc_set_branch_target(float_if, target);
  bc_set_branch_target(double_if, target);

  bc_gen_return(neg_method);
}

void gen_shift_test(JVM_CLASS *cur_class_file)
{
  JVM_CODE_GRAPH_NODE *ishl_if, *lshl_if, *ishr_if, *lshr_if, 
    *iushr_if, *lushr_if, *target;
  int i, i_one, i_two, l_one, l_two;
  JVM_METHOD *shift_method;
  int meth_idx, field_idx;

  shift_method = bc_new_method(cur_class_file, "test_shift",
     "()V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  i_one = bc_get_next_local(shift_method, jvm_Int);
  bc_push_int_const(shift_method, 1);
  bc_gen_store_op(shift_method, i_one, jvm_Int);

  i_two = bc_get_next_local(shift_method, jvm_Int);
  bc_push_int_const(shift_method, 2);
  bc_gen_store_op(shift_method, i_two, jvm_Int);

  l_one = bc_get_next_local(shift_method, jvm_Long);
  bc_push_long_const(shift_method, 1);
  bc_gen_store_op(shift_method, l_one, jvm_Long);

  l_two = bc_get_next_local(shift_method, jvm_Long);
  bc_push_long_const(shift_method, 2);
  bc_gen_store_op(shift_method, l_two, jvm_Long);

  bc_gen_load_op(shift_method, i_one, jvm_Int);
  bc_gen_load_op(shift_method, i_one, jvm_Int);
  bc_append(shift_method, jvm_ishl);
  bc_gen_load_op(shift_method, i_two, jvm_Int);

  ishl_if = bc_append(shift_method, jvm_if_icmpne);

  bc_gen_load_op(shift_method, l_one, jvm_Long);
  bc_gen_load_op(shift_method, i_one, jvm_Int);
  bc_append(shift_method, jvm_lshl);
  bc_gen_load_op(shift_method, l_two, jvm_Long);
  bc_append(shift_method, jvm_lcmp);

  lshl_if = bc_append(shift_method, jvm_ifne);

  bc_gen_load_op(shift_method, i_two, jvm_Int);
  bc_gen_load_op(shift_method, i_one, jvm_Int);
  bc_append(shift_method, jvm_ishr);
  bc_gen_load_op(shift_method, i_one, jvm_Int);

  ishr_if = bc_append(shift_method, jvm_if_icmpne);

  bc_gen_load_op(shift_method, l_two, jvm_Long);
  bc_gen_load_op(shift_method, i_one, jvm_Int);
  bc_append(shift_method, jvm_lshr);
  bc_gen_load_op(shift_method, l_one, jvm_Long);
  bc_append(shift_method, jvm_lcmp);

  lshr_if = bc_append(shift_method, jvm_ifne);

  bc_gen_load_op(shift_method, i_two, jvm_Int);
  bc_gen_load_op(shift_method, i_one, jvm_Int);
  bc_append(shift_method, jvm_iushr);
  bc_gen_load_op(shift_method, i_one, jvm_Int);

  iushr_if = bc_append(shift_method, jvm_if_icmpne);

  bc_gen_load_op(shift_method, l_two, jvm_Long);
  bc_gen_load_op(shift_method, i_one, jvm_Int);
  bc_append(shift_method, jvm_lushr);
  bc_gen_load_op(shift_method, l_one, jvm_Long);
  bc_append(shift_method, jvm_lcmp);

  lushr_if = bc_append(shift_method, jvm_ifne);

  /* getstatic (System.out):  */
  field_idx = bc_new_fieldref(cur_class_file, "java.lang.System",
     "out", "Ljava.io.PrintStream;");

  bc_append(shift_method, jvm_getstatic, field_idx);

  /* load the arg to println():  */
  bc_push_string_const(shift_method, "Shift:             OK");

  /* now invoke println():  */
  meth_idx = bc_new_methodref(cur_class_file, "java.io.PrintStream",
     "println", "(Ljava.lang.String;)V");

  bc_append(shift_method, jvm_invokevirtual, meth_idx);

  bc_gen_return(shift_method);

  target = bc_append(shift_method, jvm_getstatic, field_idx);
  bc_push_string_const(shift_method, "Shift:             Failed");
  bc_append(shift_method, jvm_invokevirtual, meth_idx);

  bc_set_branch_target(ishl_if, target);
  bc_set_branch_target(lshl_if, target);
  bc_set_branch_target(ishr_if, target);
  bc_set_branch_target(lshr_if, target);
  bc_set_branch_target(iushr_if, target);
  bc_set_branch_target(lushr_if, target);

  bc_gen_return(shift_method);
}

void gen_bitwise_or_test(JVM_CLASS *cur_class_file)
{
  JVM_CODE_GRAPH_NODE *int_if, *long_if, *target;
  int i, i_one, i_two, i_three, l_one, l_two, l_three;
  JVM_METHOD *bitwise_or_method;
  int meth_idx, field_idx;

  bitwise_or_method = bc_new_method(cur_class_file, "test_bitwise_or",
     "()V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  i_one = bc_get_next_local(bitwise_or_method, jvm_Int);
  bc_push_int_const(bitwise_or_method, 1);
  bc_gen_store_op(bitwise_or_method, i_one, jvm_Int);

  i_two = bc_get_next_local(bitwise_or_method, jvm_Int);
  bc_push_int_const(bitwise_or_method, 2);
  bc_gen_store_op(bitwise_or_method, i_two, jvm_Int);

  i_three = bc_get_next_local(bitwise_or_method, jvm_Int);
  bc_push_int_const(bitwise_or_method, 3);
  bc_gen_store_op(bitwise_or_method, i_three, jvm_Int);

  l_one = bc_get_next_local(bitwise_or_method, jvm_Long);
  bc_push_long_const(bitwise_or_method, 1);
  bc_gen_store_op(bitwise_or_method, l_one, jvm_Long);

  l_two = bc_get_next_local(bitwise_or_method, jvm_Long);
  bc_push_long_const(bitwise_or_method, 2);
  bc_gen_store_op(bitwise_or_method, l_two, jvm_Long);

  l_three = bc_get_next_local(bitwise_or_method, jvm_Long);
  bc_push_long_const(bitwise_or_method, 3);
  bc_gen_store_op(bitwise_or_method, l_three, jvm_Long);

  bc_gen_load_op(bitwise_or_method, i_one, jvm_Int);
  bc_gen_load_op(bitwise_or_method, i_two, jvm_Int);
  bc_append(bitwise_or_method, jvm_ior);
  bc_gen_load_op(bitwise_or_method, i_three, jvm_Int);

  int_if = bc_append(bitwise_or_method, jvm_if_icmpne);

  bc_gen_load_op(bitwise_or_method, l_one, jvm_Long);
  bc_gen_load_op(bitwise_or_method, l_two, jvm_Long);
  bc_append(bitwise_or_method, jvm_lor);
  bc_gen_load_op(bitwise_or_method, l_three, jvm_Long);
  bc_append(bitwise_or_method, jvm_lcmp);

  long_if = bc_append(bitwise_or_method, jvm_ifne);

  /* getstatic (System.out):  */
  field_idx = bc_new_fieldref(cur_class_file, "java.lang.System",
     "out", "Ljava.io.PrintStream;");

  bc_append(bitwise_or_method, jvm_getstatic, field_idx);

  /* load the arg to println():  */
  bc_push_string_const(bitwise_or_method, "Bitwise OR:        OK");

  /* now invoke println():  */
  meth_idx = bc_new_methodref(cur_class_file, "java.io.PrintStream",
     "println", "(Ljava.lang.String;)V");

  bc_append(bitwise_or_method, jvm_invokevirtual, meth_idx);

  bc_gen_return(bitwise_or_method);

  target = bc_append(bitwise_or_method, jvm_getstatic, field_idx);
  bc_push_string_const(bitwise_or_method, "Bitwise OR:        Failed");
  bc_append(bitwise_or_method, jvm_invokevirtual, meth_idx);

  bc_set_branch_target(int_if, target);
  bc_set_branch_target(long_if, target);

  bc_gen_return(bitwise_or_method);
}

void gen_bitwise_and_test(JVM_CLASS *cur_class_file)
{
  JVM_CODE_GRAPH_NODE *int_if, *long_if, *target;
  int i, i_one, i_two, i_zero, l_one, l_two, l_zero;
  JVM_METHOD *bitwise_and_method;
  int meth_idx, field_idx;

  bitwise_and_method = bc_new_method(cur_class_file, "test_bitwise_and",
     "()V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  i_one = bc_get_next_local(bitwise_and_method, jvm_Int);
  bc_push_int_const(bitwise_and_method, 1);
  bc_gen_store_op(bitwise_and_method, i_one, jvm_Int);

  i_two = bc_get_next_local(bitwise_and_method, jvm_Int);
  bc_push_int_const(bitwise_and_method, 2);
  bc_gen_store_op(bitwise_and_method, i_two, jvm_Int);

  i_zero = bc_get_next_local(bitwise_and_method, jvm_Int);
  bc_push_int_const(bitwise_and_method, 0);
  bc_gen_store_op(bitwise_and_method, i_zero, jvm_Int);

  l_one = bc_get_next_local(bitwise_and_method, jvm_Long);
  bc_push_long_const(bitwise_and_method, 1);
  bc_gen_store_op(bitwise_and_method, l_one, jvm_Long);

  l_two = bc_get_next_local(bitwise_and_method, jvm_Long);
  bc_push_long_const(bitwise_and_method, 2);
  bc_gen_store_op(bitwise_and_method, l_two, jvm_Long);

  l_zero = bc_get_next_local(bitwise_and_method, jvm_Long);
  bc_push_long_const(bitwise_and_method, 0);
  bc_gen_store_op(bitwise_and_method, l_zero, jvm_Long);

  bc_gen_load_op(bitwise_and_method, i_one, jvm_Int);
  bc_gen_load_op(bitwise_and_method, i_two, jvm_Int);
  bc_append(bitwise_and_method, jvm_iand);
  bc_gen_load_op(bitwise_and_method, i_zero, jvm_Int);

  int_if = bc_append(bitwise_and_method, jvm_if_icmpne);

  bc_gen_load_op(bitwise_and_method, l_one, jvm_Long);
  bc_gen_load_op(bitwise_and_method, l_two, jvm_Long);
  bc_append(bitwise_and_method, jvm_land);
  bc_gen_load_op(bitwise_and_method, l_zero, jvm_Long);
  bc_append(bitwise_and_method, jvm_lcmp);

  long_if = bc_append(bitwise_and_method, jvm_ifne);

  /* getstatic (System.out):  */
  field_idx = bc_new_fieldref(cur_class_file, "java.lang.System",
     "out", "Ljava.io.PrintStream;");

  bc_append(bitwise_and_method, jvm_getstatic, field_idx);

  /* load the arg to println():  */
  bc_push_string_const(bitwise_and_method, "Bitwise AND:       OK");

  /* now invoke println():  */
  meth_idx = bc_new_methodref(cur_class_file, "java.io.PrintStream",
     "println", "(Ljava.lang.String;)V");

  bc_append(bitwise_and_method, jvm_invokevirtual, meth_idx);

  bc_gen_return(bitwise_and_method);

  target = bc_append(bitwise_and_method, jvm_getstatic, field_idx);
  bc_push_string_const(bitwise_and_method, "Bitwise AND:       Failed");
  bc_append(bitwise_and_method, jvm_invokevirtual, meth_idx);

  bc_set_branch_target(int_if, target);
  bc_set_branch_target(long_if, target);

  bc_gen_return(bitwise_and_method);
}

void gen_bitwise_xor_test(JVM_CLASS *cur_class_file)
{
  JVM_CODE_GRAPH_NODE *int_if, *long_if, *target;
  int i, i_one, i_two, i_three, l_one, l_two, l_three;
  JVM_METHOD *bitwise_xor_method;
  int meth_idx, field_idx;

  bitwise_xor_method = bc_new_method(cur_class_file, "test_bitwise_xor",
     "()V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  i_one = bc_get_next_local(bitwise_xor_method, jvm_Int);
  bc_push_int_const(bitwise_xor_method, 1);
  bc_gen_store_op(bitwise_xor_method, i_one, jvm_Int);

  i_two = bc_get_next_local(bitwise_xor_method, jvm_Int);
  bc_push_int_const(bitwise_xor_method, 2);
  bc_gen_store_op(bitwise_xor_method, i_two, jvm_Int);

  i_three = bc_get_next_local(bitwise_xor_method, jvm_Int);
  bc_push_int_const(bitwise_xor_method, 3);
  bc_gen_store_op(bitwise_xor_method, i_three, jvm_Int);

  l_one = bc_get_next_local(bitwise_xor_method, jvm_Long);
  bc_push_long_const(bitwise_xor_method, 1);
  bc_gen_store_op(bitwise_xor_method, l_one, jvm_Long);

  l_two = bc_get_next_local(bitwise_xor_method, jvm_Long);
  bc_push_long_const(bitwise_xor_method, 2);
  bc_gen_store_op(bitwise_xor_method, l_two, jvm_Long);

  l_three = bc_get_next_local(bitwise_xor_method, jvm_Long);
  bc_push_long_const(bitwise_xor_method, 3);
  bc_gen_store_op(bitwise_xor_method, l_three, jvm_Long);

  bc_gen_load_op(bitwise_xor_method, i_one, jvm_Int);
  bc_gen_load_op(bitwise_xor_method, i_two, jvm_Int);
  bc_append(bitwise_xor_method, jvm_ixor);
  bc_gen_load_op(bitwise_xor_method, i_three, jvm_Int);

  int_if = bc_append(bitwise_xor_method, jvm_if_icmpne);

  bc_gen_load_op(bitwise_xor_method, l_one, jvm_Long);
  bc_gen_load_op(bitwise_xor_method, l_two, jvm_Long);
  bc_append(bitwise_xor_method, jvm_lxor);
  bc_gen_load_op(bitwise_xor_method, l_three, jvm_Long);
  bc_append(bitwise_xor_method, jvm_lcmp);

  long_if = bc_append(bitwise_xor_method, jvm_ifne);

  /* getstatic (System.out):  */
  field_idx = bc_new_fieldref(cur_class_file, "java.lang.System",
     "out", "Ljava.io.PrintStream;");

  bc_append(bitwise_xor_method, jvm_getstatic, field_idx);

  /* load the arg to println():  */
  bc_push_string_const(bitwise_xor_method, "Bitwise XOR:       OK");

  /* now invoke println():  */
  meth_idx = bc_new_methodref(cur_class_file, "java.io.PrintStream",
     "println", "(Ljava.lang.String;)V");

  bc_append(bitwise_xor_method, jvm_invokevirtual, meth_idx);

  bc_gen_return(bitwise_xor_method);

  target = bc_append(bitwise_xor_method, jvm_getstatic, field_idx);
  bc_push_string_const(bitwise_xor_method, "Bitwise XOR:       Failed");
  bc_append(bitwise_xor_method, jvm_invokevirtual, meth_idx);

  bc_set_branch_target(int_if, target);
  bc_set_branch_target(long_if, target);

  bc_gen_return(bitwise_xor_method);
}

void gen_comparison_test(JVM_CLASS *cur_class_file)
{
  JVM_CODE_GRAPH_NODE *long_if, *fcmpg_if, *fcmpl_if, 
    *dcmpg_if, *dcmpl_if, *target;
  int i, l_eleven, l_two, f_eleven, f_two, d_eleven, d_two;
  JVM_METHOD *compare_method;
  int meth_idx, field_idx;

  compare_method = bc_new_method(cur_class_file, "test_comparison",
     "()V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  l_two = bc_get_next_local(compare_method, jvm_Long);
  bc_push_long_const(compare_method, 2);
  bc_gen_store_op(compare_method, l_two, jvm_Long);

  l_eleven = bc_get_next_local(compare_method, jvm_Long);
  bc_push_long_const(compare_method, 11);
  bc_gen_store_op(compare_method, l_eleven, jvm_Long);

  f_two = bc_get_next_local(compare_method, jvm_Float);
  bc_push_float_const(compare_method, 2.0);
  bc_gen_store_op(compare_method, f_two, jvm_Float);

  f_eleven = bc_get_next_local(compare_method, jvm_Float);
  bc_push_float_const(compare_method, 11.0);
  bc_gen_store_op(compare_method, f_eleven, jvm_Float);

  d_two = bc_get_next_local(compare_method, jvm_Double);
  bc_push_double_const(compare_method, 2.0);
  bc_gen_store_op(compare_method, d_two, jvm_Double);

  d_eleven = bc_get_next_local(compare_method, jvm_Double);
  bc_push_double_const(compare_method, 11.0);
  bc_gen_store_op(compare_method, d_eleven, jvm_Double);

  bc_gen_load_op(compare_method, l_eleven, jvm_Long);
  bc_gen_load_op(compare_method, l_two, jvm_Long);
  bc_append(compare_method, jvm_lcmp);

  long_if = bc_append(compare_method, jvm_ifle);

  bc_gen_load_op(compare_method, f_eleven, jvm_Float);
  bc_gen_load_op(compare_method, f_two, jvm_Float);
  bc_append(compare_method, jvm_fcmpl);

  fcmpl_if = bc_append(compare_method, jvm_ifle);

  bc_gen_load_op(compare_method, f_two, jvm_Float);
  bc_gen_load_op(compare_method, f_eleven, jvm_Float);
  bc_append(compare_method, jvm_fcmpg);

  fcmpg_if = bc_append(compare_method, jvm_ifge);

  bc_gen_load_op(compare_method, d_eleven, jvm_Double);
  bc_gen_load_op(compare_method, d_two, jvm_Double);
  bc_append(compare_method, jvm_dcmpl);

  dcmpl_if = bc_append(compare_method, jvm_ifle);

  bc_gen_load_op(compare_method, d_two, jvm_Double);
  bc_gen_load_op(compare_method, d_eleven, jvm_Double);
  bc_append(compare_method, jvm_dcmpg);

  dcmpg_if = bc_append(compare_method, jvm_ifge);

  /* getstatic (System.out):  */
  field_idx = bc_new_fieldref(cur_class_file, "java.lang.System",
     "out", "Ljava.io.PrintStream;");

  bc_append(compare_method, jvm_getstatic, field_idx);

  /* load the arg to println():  */
  bc_push_string_const(compare_method, "Comparison:        OK");

  /* now invoke println():  */
  meth_idx = bc_new_methodref(cur_class_file, "java.io.PrintStream",
     "println", "(Ljava.lang.String;)V");

  bc_append(compare_method, jvm_invokevirtual, meth_idx);

  bc_gen_return(compare_method);

  target = bc_append(compare_method, jvm_getstatic, field_idx);
  bc_push_string_const(compare_method, "Comparison:        Failed");
  bc_append(compare_method, jvm_invokevirtual, meth_idx);

  bc_set_branch_target(long_if, target);
  bc_set_branch_target(fcmpl_if, target);
  bc_set_branch_target(fcmpg_if, target);
  bc_set_branch_target(dcmpl_if, target);
  bc_set_branch_target(dcmpg_if, target);

  bc_gen_return(compare_method);
}
