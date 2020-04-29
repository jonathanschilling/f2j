#include <stdio.h>
#include "bytecode.h"

int main()
{
  Arithmetic();
  ClassAbstract();
  ClassDeprecated();
  ClassEmptyInterface();
  ClassNoAccFlags();
  ClassNonEmptyInterface();
  ClassNoSourceFile();
  ClassWithFields();
  ClassWithInterfaces();
  ClassWithSuper();
  ClassWithUserDefAttr();
  ConstantPool();
  ControlTransfer();
  ExceptionTable();
  ExceptionTableRecalc();
  Iinc();
  InnerClassAttr();
  LineNumbers();
  LoadStore();
  LocalVariables();
  LookupSwitch();
  MethodDeprecated();
  MethodInvocation();
  MethodWithExceptions();
  ObjectCreation();
  StackManagement();
  Synchronization();
  TableSwitch();
  TypeConversion();
  Goto();
}

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

int Arithmetic(){
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

/* 
 * Generates a class with the JVM_ACC_ABSTRACT access modifier set.
 */

int ClassAbstract()
{
  JVM_METHOD *main_method;
  JVM_CLASS *cur_class_file;

  cur_class_file = bc_new_class("ClassAbstract", NULL, NULL, NULL, 
      JVM_ACC_PUBLIC|JVM_ACC_SUPER|JVM_ACC_ABSTRACT);

  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}

/* 
 * Generates a deprecated class.  javap doesn't seem to indicate
 * that it's deprecated when disassembling it, but I verified that
 * the "Deprecated" attribute is there.  Also javac recognizes that
 * it's deprecated when trying to compile a class that uses this one.
 */

int ClassDeprecated(){
  JVM_METHOD *main_method;
  JVM_CLASS *cur_class_file;

  cur_class_file = bc_new_class("ClassDeprecated", "asdf.f", NULL, NULL, 
      JVM_ACC_PUBLIC|JVM_ACC_SUPER);

  bc_set_class_deprecated(cur_class_file);

  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}

/* 
 * Generates an interface with the JVM_ACC_ABSTRACT and JVM_ACC_INTERFACE
 * access modifiers set.  Since an interface cannot implement any
 * methods, we do not create any methods here.
 */

int ClassEmptyInterface(){
  JVM_CLASS *cur_class_file;

  cur_class_file = bc_new_class("ClassEmptyInterface", "asdf.f", NULL, 
      NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER|JVM_ACC_ABSTRACT|JVM_ACC_INTERFACE);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}

/* 
 * Generates a class with no access modifiers set.
 */

int ClassNoAccFlags(){
  JVM_METHOD *main_method;
  JVM_CLASS *cur_class_file;

  cur_class_file = bc_new_class("ClassNoAccFlags", "asdf.f", NULL, 
      NULL, 0x0);

  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}

/* 
 * Generates an interface with one abstract method.
 */

int ClassNonEmptyInterface(){
  JVM_METHOD *abstract_method;
  JVM_CLASS *cur_class_file;

  cur_class_file = bc_new_class("ClassNonEmptyInterface", "asdf.f", NULL,
      NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER|JVM_ACC_ABSTRACT|JVM_ACC_INTERFACE);

  abstract_method = bc_new_method(cur_class_file, "foo",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_ABSTRACT);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}

/*
 * Generates a class with no SourceFile attribute.
 */

int ClassNoSourceFile(){
  JVM_METHOD *main_method;
  JVM_CLASS *cur_class_file;

  cur_class_file = bc_new_class("ClassNoSourceFile", NULL, NULL, NULL, 
      JVM_ACC_PUBLIC|JVM_ACC_SUPER);

  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}

/* 
 * Generates a class with some fields.
 */

int ClassWithFields(){
  JVM_METHOD *main_method;
  JVM_FIELD *field;
  JVM_CLASS *cur_class_file;
  int intval;
  double doubleval;
  int c;

  cur_class_file = bc_new_class("ClassWithFields", "asdf.f", NULL, NULL, 
      JVM_ACC_PUBLIC|JVM_ACC_SUPER);

  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  bc_add_field(cur_class_file, "public_int",     "I", JVM_ACC_PUBLIC);
  bc_add_field(cur_class_file, "private_int",    "I", JVM_ACC_PRIVATE);
  bc_add_field(cur_class_file, "protected_int",  "I", JVM_ACC_PROTECTED);
  bc_add_field(cur_class_file, "static_int",     "I", JVM_ACC_STATIC);
  bc_add_field(cur_class_file, "final_int",      "I", JVM_ACC_FINAL);
  bc_add_field(cur_class_file, "volatile_int",   "I", JVM_ACC_VOLATILE);
  bc_add_field(cur_class_file, "transient_int",  "I", JVM_ACC_TRANSIENT);

  bc_add_field(cur_class_file, "public_static_int",  "I", JVM_ACC_PUBLIC|JVM_ACC_STATIC);
  bc_add_field(cur_class_file, "private_static_int", "I", JVM_ACC_PRIVATE|JVM_ACC_STATIC);
  bc_add_field(cur_class_file, "public_final_int",   "I", JVM_ACC_PUBLIC|JVM_ACC_FINAL);

  bc_add_field(cur_class_file, "public_object", "Ljava/lang/String;", JVM_ACC_PUBLIC);

  bc_add_field(cur_class_file, "public_int_array",   "[I", JVM_ACC_PUBLIC);

  /* test adding ConstantValue attribute to a field.  first create the
   * field and then add the attribute 
   */

  field = bc_add_field(cur_class_file, "int_with_constantval_attr", "I", 
     JVM_ACC_PUBLIC);

  intval = 666;

  bc_set_constant_value_attr(field, CONSTANT_Integer, &intval);

  field = bc_add_field(cur_class_file, "dbl_with_constantval_attr", "D",
     JVM_ACC_PUBLIC);

  doubleval = 123.456;

  bc_set_constant_value_attr(field, CONSTANT_Double, &doubleval);

  /* test adding Synthetic attribute to a field. */

  field = bc_add_field(cur_class_file, "int_with_synthetic_attr", "I", 
     JVM_ACC_PUBLIC);

  bc_set_field_synthetic(field);

  /* test adding Deprecated attribute to a field. */

  field = bc_add_field(cur_class_file, "int_deprecated", "I", 
     JVM_ACC_PUBLIC);

  bc_set_field_deprecated(field);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}

/* 
 * Generates a class with some interfaces.
 */

int ClassWithInterfaces(){
  JVM_METHOD *main_method, *run_method, *comp_method;
  JVM_CLASS *cur_class_file;

  cur_class_file = bc_new_class("ClassWithInterfaces", "asdf.f", 
      "java.lang.Thread", NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER);

  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  bc_add_class_interface(cur_class_file, "java.lang.Runnable");
  bc_add_class_interface(cur_class_file, "java.lang.Cloneable");
  bc_add_class_interface(cur_class_file, "java.lang.Comparable");
     
  run_method = bc_new_method(cur_class_file, "run",
     "()V", "foo", JVM_ACC_PUBLIC);
  bc_gen_return(run_method);

  comp_method = bc_new_method(cur_class_file, "compareTo",
     "(Ljava/lang/Object;)I", "foo", JVM_ACC_PUBLIC);
  bc_append(comp_method, jvm_iconst_0);
  bc_gen_return(comp_method);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}

/* 
 * Generates a class with a particular superclass.
 */

int ClassWithSuper(){
  JVM_METHOD *main_method;
  JVM_CLASS *cur_class_file;

  cur_class_file = bc_new_class("ClassWithSuper", "asdf.f", 
      "java.lang.Thread", NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER);

  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}

/* 
 * Shows how to add a user-defined attribute to a class.
 */

int ClassWithUserDefAttr() {
  char *attr_text = "this is a user-defined attribute";
  JVM_METHOD *main_method;
  JVM_FIELD *field;
  JVM_CLASS *cur_class_file;
  int intval;
  int c;

  cur_class_file = bc_new_class("ClassWithUserDefAttr", NULL, NULL, NULL, 
      JVM_ACC_PUBLIC|JVM_ACC_SUPER);

  bc_add_user_defined_class_attr(cur_class_file, "MyAttribute", 
     strlen(attr_text), attr_text);

  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}

/* 
 * Inserting different kinds of constants into the constant pool.
 */

int ConstantPool(){
  JVM_METHOD *main_method, *test_method;
  JVM_CLASS *cur_class_file;

  int dbl_idx, float_idx, int_idx, long_idx, str_idx, class_idx,
    utf8_idx, field_idx, meth_idx, interface_idx, name_idx;

  double x = 2.2;
  float y = 1.1;
  int a = 666;
  long long l = 123456;
  char *utf8 = "utf8 string";
  char *str = "hello world";
  char *class_str = "java.lang.String";

  CP_INFO *str_node;

  cur_class_file = bc_new_class("ConstantPool", "asdf.f", NULL, NULL, 
      JVM_ACC_PUBLIC|JVM_ACC_SUPER);

  dbl_idx = cp_find_or_insert(cur_class_file, CONSTANT_Double, &x);
  float_idx = cp_find_or_insert(cur_class_file, CONSTANT_Float, &y);
  int_idx = cp_manual_insert(cur_class_file, CONSTANT_Integer, &a);
  long_idx = cp_find_or_insert(cur_class_file, CONSTANT_Long, &l);
  utf8_idx = cp_find_or_insert(cur_class_file, CONSTANT_Utf8, utf8);
  class_idx = cp_find_or_insert(cur_class_file, CONSTANT_Class, class_str);
  str_idx = cp_find_or_insert(cur_class_file, CONSTANT_String, str);

  field_idx = bc_new_fieldref(cur_class_file, "java.lang.Boolean", 
     "TRUE", "Ljava.lang.Boolean;");
  meth_idx = bc_new_methodref(cur_class_file, "java.lang.Boolean", 
     "getBoolean", "(Ljava.lang.String;)Z");
  interface_idx = bc_new_interface_methodref(cur_class_file, 
     "java.util.Enumeration", "hasMoreElements", "()Z");
  name_idx = bc_new_name_and_type(cur_class_file, "blah", "I");

  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  bc_append(main_method, jvm_ldc2_w, dbl_idx);
  bc_append(main_method, jvm_pop2);

  bc_append(main_method, jvm_ldc2_w, long_idx);
  bc_append(main_method, jvm_pop2);

  bc_append(main_method, jvm_new, class_idx);
  bc_append(main_method, jvm_pop);

  bc_append(main_method, jvm_ldc, float_idx);
  bc_append(main_method, jvm_pop);

  bc_append(main_method, jvm_ldc, int_idx);
  bc_append(main_method, jvm_pop);

  bc_append(main_method, jvm_ldc, str_idx);
  bc_append(main_method, jvm_pop);

  bc_append(main_method, jvm_getstatic, field_idx);
  bc_append(main_method, jvm_pop);

  bc_append(main_method, jvm_ldc, str_idx);
  bc_append(main_method, jvm_invokestatic, meth_idx);
  bc_append(main_method, jvm_pop);

  bc_gen_return(main_method);

  test_method = bc_new_method(cur_class_file, "test",
     "(Ljava/util/Enumeration;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);
  bc_append(test_method, jvm_aload_0);
  bc_append(test_method, jvm_invokeinterface, interface_idx, 1);
  bc_append(test_method, jvm_pop);
  bc_gen_return(test_method);

  /* To see a dump of the constant pool, uncomment the following line. */
  /* cp_dump(cur_class_file); */

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}

/*
 * Generates various control transfer instructions mentioned in
 * section 3.11.7 of the JVM spec except for the following:
 *  -goto and goto_w which are tested in ExceptionTable.c
 *  -tableswitch which is tested in Tableswitch.c
 *  -lookupswitch which is tested in Lookupswitch.c
 */

int ControlTransfer(){
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

/* 
 * Generates two exception handlers and the exception table entries
 * for them.
 */

int ExceptionTable(){
  JVM_METHOD *main_method, *foo_method;
  JVM_CLASS *cur_class_file;
  JVM_CODE_GRAPH_NODE *if_node, *start, *end, *handler;
  int str_idx, meth_idx, field_idx;
  JVM_EXCEPTION_TABLE_ENTRY *et_entry;
  char *io_str, *arith_str;
  int elvnum;

  io_str = "io exception";
  arith_str = "arithmetic exception";

  cur_class_file = bc_new_class("ExceptionTable", "asdf.f", NULL, NULL, 
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
  meth_idx = bc_new_methodref(cur_class_file, "ExceptionTable", "a", "(I)V");
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

  /* return from the exception handler */

  et_entry = bc_new_exception_table_entry(main_method, start, end, handler, 
     "java.io.IOException");
  bc_add_exception_handler(main_method, et_entry);

  /* the exception handler falls through to the following code */

  /* iconst_0: */
  bc_set_branch_target(end, start = bc_push_int_const(main_method, 1));

  /* invokestatic <Method void a(int)>: */
  meth_idx = bc_new_methodref(cur_class_file, "ExceptionTable",
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

/* 
 * Generates a goto which jumps over the exception handler, but creates
 * an exception handler so big that the library converts the goto to goto_w
 * and therefore must recalculate the addresses of the other instructions
 * and the entries in the exception table.
 */

int ExceptionTableRecalc(){
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

/*
 * Tests whether the library is properly generating wide instructions
 * when the index operand to the iinc instruction exceeds an unsigned 
 * byte value or when the immediate operand exceeds a signed byte value.
 */

int Iinc(){
  JVM_METHOD *main_method;
  JVM_CLASS *cur_class_file;

  cur_class_file = bc_new_class("Iinc", "iinc.f", NULL, 
      NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER);
  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  bc_append(main_method, jvm_iconst_0);
  bc_gen_store_op(main_method, 1, jvm_Int);
  bc_append(main_method, jvm_iconst_0);
  bc_gen_store_op(main_method, 255, jvm_Int);
  bc_append(main_method, jvm_iconst_0);
  bc_gen_store_op(main_method, 300, jvm_Int);

  bc_gen_iinc(main_method, 1, -1);
  bc_gen_iinc(main_method, 255, -1);
  bc_gen_iinc(main_method, 300, -1);
  bc_gen_iinc(main_method, 255, 127);
  bc_gen_iinc(main_method, 255, 300);
  bc_gen_iinc(main_method, 255, -127);
  bc_gen_iinc(main_method, 255, -128);
  bc_gen_iinc(main_method, 255, -129);
  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}

/*
 * Generates a class with an InnerClasses attribute.
 */

int InnerClassAttr(){
  JVM_METHOD *main_method;
  JVM_CLASS *cur_class_file;
  JVM_CLASS *inner_class_file;
  JVM_CODE_GRAPH_NODE *op;

  /* first generate a class to represent the referenced inner class */

  inner_class_file = bc_new_class("inner", "asdf.f", NULL,
      NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER);
  bc_add_default_constructor(inner_class_file, JVM_ACC_PUBLIC);

  bc_write_class(inner_class_file, ".");
  bc_free_class(inner_class_file);


  /* now create the class which has the InnerClasses attribute */

  cur_class_file = bc_new_class("InnerClassAttr", "asdf.f", NULL, 
      NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER);
  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  op = bc_push_int_const(main_method, 12);
  bc_gen_store_op(main_method, 1, jvm_Int);

  op = bc_gen_return(main_method);

  bc_add_inner_classes_attr(cur_class_file, "inner", "InnerClassAttr",
     "foo", JVM_ACC_PUBLIC);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}

/*
 * Generate a method with a line number table.
 */

int LineNumbers(){
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

/*
 * Generates all the various load/store instructions mentioned in
 * section 3.11.2 of the JVM spec.
 */

int LoadStore(){
  JVM_METHOD *main_method;
  JVM_METHOD *method;
  JVM_CLASS *cur_class_file;
  int i;

  cur_class_file = bc_new_class("LoadStore", "asdf.f", NULL, 
      NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER);
  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);
  bc_gen_return(main_method);

  method = bc_new_method(cur_class_file, "loadstore",
     "()V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  /* Loading integer constants */

  bc_push_int_const(method, 10);                  /* bipush */
  bc_push_int_const(method, 300);                 /* sipush */
  bc_push_int_const(method, 999999);              /* ldc */
  bc_push_int_const(method, -1);                  /* iconst_m1 */
  bc_push_int_const(method, 0);                   /* iconst_0 */
  bc_push_int_const(method, 1);                   /* iconst_1 */
  bc_push_int_const(method, 2);                   /* iconst_2 */
  bc_push_int_const(method, 3);                   /* iconst_3 */
  bc_push_int_const(method, 4);                   /* iconst_4 */
  bc_push_int_const(method, 5);                   /* iconst_5 */

  /* Loading object constant */
  bc_push_null_const(method);                     /* aconst_null */

  /* Loading float constants */
  bc_push_float_const(method, 0.0);               /* fconst_0 */
  bc_push_float_const(method, 1.0);               /* fconst_1 */
  bc_push_float_const(method, 2.0);               /* fconst_2 */
  bc_push_float_const(method, 9.9);               /* ldc */

  /* Loading string constants */
  bc_push_string_const(method, "foo");            /* ldc */

  /* Loading double constants */
  bc_push_double_const(method, 0.0);              /* dconst_0 */
  bc_push_double_const(method, 1.0);              /* dconst_1 */
  bc_push_double_const(method, 3.3);              /* ldc2_w */

  /* Loading long constants */
  bc_push_long_const(method, 0);                  /* lconst_0 */
  bc_push_long_const(method, 1);                  /* lconst_1 */
  bc_push_long_const(method, 55);                 /* ldc2_w */

  /* Force a bunch of junk into the constant pool to test
   * whether the wide load instruction gets generated on
   * subsequent calls to bc_push_int_const, etc.
   */

  for(i=3;i<255;i++) {
    double dval;

    dval = (double)i;
    cp_find_or_insert(cur_class_file, CONSTANT_Double, (void*)&dval);
  }

  /* now the following calls should generate "ldc_w". 
   * double and long always generate the wide "ldc2_w", 
   * so don't worry about testing them here as they
   * were tested above.
   */

  bc_push_int_const(method, 888888);              /* ldc_w */
  bc_push_float_const(method, 8.8);               /* ldc_w */
  bc_push_string_const(method, "bar");            /* ldc_w */

  /* Storing/Loading integer local variables */

  bc_push_int_const(method, 0);                   /* iconst_0 */
  bc_gen_store_op(method, 0, jvm_Int);          /* istore_0 */
  bc_gen_load_op(method, 0, jvm_Int);           /* iload_0 */

  bc_push_int_const(method, 0);                   /* iconst_0 */
  bc_gen_store_op(method, 1, jvm_Int);          /* istore_1 */
  bc_gen_load_op(method, 1, jvm_Int);           /* iload_1 */

  bc_push_int_const(method, 0);                   /* iconst_0 */
  bc_gen_store_op(method, 2, jvm_Int);          /* istore_2 */
  bc_gen_load_op(method, 2, jvm_Int);           /* iload_2 */

  bc_push_int_const(method, 0);                   /* iconst_0 */
  bc_gen_store_op(method, 3, jvm_Int);          /* istore_3 */
  bc_gen_load_op(method, 3, jvm_Int);           /* iload_3 */

  bc_push_int_const(method, 0);                   /* iconst_0 */
  bc_gen_store_op(method, 4, jvm_Int);          /* istore */
  bc_gen_load_op(method, 4, jvm_Int);           /* iload */

  /* Storing/Loading integer local variables */

  bc_push_long_const(method, 0);                  /* lconst_0 */
  bc_gen_store_op(method, 0, jvm_Long);         /* lstore_0 */
  bc_gen_load_op(method, 0, jvm_Long);          /* lload_0 */

  bc_push_long_const(method, 0);                  /* lconst_0 */
  bc_gen_store_op(method, 1, jvm_Long);         /* lstore_1 */
  bc_gen_load_op(method, 1, jvm_Long);          /* lload_1 */

  bc_push_long_const(method, 0);                  /* lconst_0 */
  bc_gen_store_op(method, 2, jvm_Long);         /* lstore_2 */
  bc_gen_load_op(method, 2, jvm_Long);          /* lload_2 */

  bc_push_long_const(method, 0);                  /* lconst_0 */
  bc_gen_store_op(method, 3, jvm_Long);         /* lstore_3 */
  bc_gen_load_op(method, 3, jvm_Long);          /* lload_3 */

  bc_push_long_const(method, 0);                  /* lconst_0 */
  bc_gen_store_op(method, 4, jvm_Long);         /* lstore */
  bc_gen_load_op(method, 4, jvm_Long);          /* lload */

  /* Storing/Loading float local variables */

  bc_push_float_const(method, 0.0);               /* fconst_0 */
  bc_gen_store_op(method, 0, jvm_Float);        /* fstore_0 */
  bc_gen_load_op(method, 0, jvm_Float);         /* fload_0 */

  bc_push_float_const(method, 0.0);               /* fconst_0 */
  bc_gen_store_op(method, 1, jvm_Float);        /* fstore_1 */
  bc_gen_load_op(method, 1, jvm_Float);         /* fload_1 */
  
  bc_push_float_const(method, 0.0);               /* fconst_0 */
  bc_gen_store_op(method, 2, jvm_Float);        /* fstore_2 */
  bc_gen_load_op(method, 2, jvm_Float);         /* fload_2 */

  bc_push_float_const(method, 0.0);               /* fconst_0 */
  bc_gen_store_op(method, 3, jvm_Float);        /* fstore_3 */
  bc_gen_load_op(method, 3, jvm_Float);         /* fload_3 */

  bc_push_float_const(method, 0.0);               /* fconst_0 */
  bc_gen_store_op(method, 4, jvm_Float);        /* fstore */
  bc_gen_load_op(method, 4, jvm_Float);         /* fload */

  /* Storing/Loading double local variables */

  bc_push_double_const(method, 0.0);              /* dconst_0 */
  bc_gen_store_op(method, 0, jvm_Double);       /* dstore_0 */
  bc_gen_load_op(method, 0, jvm_Double);        /* dload_0 */

  bc_push_double_const(method, 0.0);              /* dconst_0 */
  bc_gen_store_op(method, 1, jvm_Double);       /* dstore_1 */
  bc_gen_load_op(method, 1, jvm_Double);        /* dload_1 */

  bc_push_double_const(method, 0.0);              /* dconst_0 */
  bc_gen_store_op(method, 2, jvm_Double);       /* dstore_2 */
  bc_gen_load_op(method, 2, jvm_Double);        /* dload_2 */

  bc_push_double_const(method, 0.0);              /* dconst_0 */
  bc_gen_store_op(method, 3, jvm_Double);       /* dstore_3 */
  bc_gen_load_op(method, 3, jvm_Double);        /* dload_3 */

  bc_push_double_const(method, 0.0);              /* dconst_0 */
  bc_gen_store_op(method, 4, jvm_Double);       /* dstore */
  bc_gen_load_op(method, 4, jvm_Double);        /* dload */

  /* Storing/Loading object local variables */

  bc_push_null_const(method);                     /* aconst_null */
  bc_gen_store_op(method, 0, jvm_Object);       /* astore_0 */
  bc_gen_load_op(method, 0, jvm_Object);        /* aload_0 */

  bc_push_null_const(method);                     /* aconst_null */
  bc_gen_store_op(method, 1, jvm_Object);       /* astore_1 */
  bc_gen_load_op(method, 1, jvm_Object);        /* aload_1 */

  bc_push_null_const(method);                     /* aconst_null */
  bc_gen_store_op(method, 2, jvm_Object);       /* astore_2 */
  bc_gen_load_op(method, 2, jvm_Object);        /* aload_2 */

  bc_push_null_const(method);                     /* aconst_null */
  bc_gen_store_op(method, 3, jvm_Object);       /* astore_3 */
  bc_gen_load_op(method, 3, jvm_Object);        /* aload_3 */

  bc_push_null_const(method);                     /* aconst_null */
  bc_gen_store_op(method, 4, jvm_Object);       /* astore */
  bc_gen_load_op(method, 4, jvm_Object);        /* aload */

  /* Array store/load instructions */

  bc_gen_new_array(method, 10, jvm_Byte);
  bc_append(method, jvm_dup);       /* dup for load */
  bc_push_int_const(method, 0);
  bc_push_int_const(method, 22);
  bc_gen_array_store_op(method, jvm_Byte);
  bc_push_int_const(method, 0);
  bc_gen_array_load_op(method, jvm_Byte);

  bc_gen_new_array(method, 10, jvm_Short);
  bc_append(method, jvm_dup);       /* dup for load */
  bc_push_int_const(method, 0);
  bc_push_int_const(method, 22);
  bc_gen_array_store_op(method, jvm_Short);
  bc_push_int_const(method, 0);
  bc_gen_array_load_op(method, jvm_Short);

  bc_gen_new_array(method, 10, jvm_Int);
  bc_append(method, jvm_dup);       /* dup for load */
  bc_push_int_const(method, 0);
  bc_push_int_const(method, 22);
  bc_gen_array_store_op(method, jvm_Int);
  bc_push_int_const(method, 0);
  bc_gen_array_load_op(method, jvm_Int);

  bc_gen_new_array(method, 10, jvm_Long);
  bc_append(method, jvm_dup);       /* dup for load */
  bc_push_int_const(method, 0);
  bc_push_long_const(method, 22);
  bc_gen_array_store_op(method, jvm_Long);
  bc_push_int_const(method, 0);
  bc_gen_array_load_op(method, jvm_Long);

  bc_gen_new_array(method, 10, jvm_Char);
  bc_append(method, jvm_dup);       /* dup for load */
  bc_push_int_const(method, 0);
  bc_push_int_const(method, 22);
  bc_gen_array_store_op(method, jvm_Char);
  bc_push_int_const(method, 0);
  bc_gen_array_load_op(method, jvm_Char);

  bc_gen_new_array(method, 10, jvm_Float);
  bc_append(method, jvm_dup);       /* dup for load */
  bc_push_int_const(method, 0);
  bc_push_float_const(method, 22.2);
  bc_gen_array_store_op(method, jvm_Float);
  bc_push_int_const(method, 0);
  bc_gen_array_load_op(method, jvm_Float);

  bc_gen_new_array(method, 10, jvm_Double);
  bc_append(method, jvm_dup);       /* dup for load */
  bc_push_int_const(method, 0);
  bc_push_double_const(method, 22.2);
  bc_gen_array_store_op(method, jvm_Double);
  bc_push_int_const(method, 0);
  bc_gen_array_load_op(method, jvm_Double);

  bc_gen_new_object_array(method, 10, "java.lang.String");
  bc_append(method, jvm_dup);       /* dup for load */
  bc_push_int_const(method, 0);
  bc_push_null_const(method);
  bc_gen_array_store_op(method, jvm_Object);
  bc_push_int_const(method, 0);
  bc_gen_array_load_op(method, jvm_Object);

  /* Testing whether wide instruction is generated for large local var nums */

  bc_push_int_const(method, 0);                     /* iconst_0 */
  bc_gen_store_op(method, 300, jvm_Int);          /* istore */
  bc_gen_load_op(method, 300, jvm_Int);           /* iload */

  bc_push_long_const(method, 0);                    /* lconst_0 */
  bc_gen_store_op(method, 300, jvm_Long);         /* lstore */
  bc_gen_load_op(method, 300, jvm_Long);          /* lload */

  bc_push_float_const(method, 0.0);                 /* fconst_0 */
  bc_gen_store_op(method, 300, jvm_Float);        /* fstore */
  bc_gen_load_op(method, 300, jvm_Float);         /* fload */

  bc_push_double_const(method, 0.0);                /* dconst_0 */
  bc_gen_store_op(method, 300, jvm_Double);       /* dstore */
  bc_gen_load_op(method, 300, jvm_Double);        /* dload */

  bc_push_null_const(method);                       /* aconst_null */
  bc_gen_store_op(method, 300, jvm_Object);       /* astore */
  bc_gen_load_op(method, 300, jvm_Object);        /* aload */
  
  bc_gen_return(method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}

/*
 * Generate a method with a local variable table.  the "start" of the
 * local variable should be set to the first op after a value has been
 * assigned to the variable (see section 4.7.9 of the JVM spec).  if the
 * variable is valid through the end of the method, the the "end" field
 * does not need to be set.  it will automatically be set to the last
 * instruction in the method.  otherwise, the "end" should be set manually
 * to the last instruction for which the variable would be valid.
 */

int LocalVariables(){
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

/*
 * Generate two switch blocks with a large span in the case numbers so
 * that a lookupswitch instruction will be used instead of tableswitch.
 * The second switch is after a goto which will get converted to a goto_w, 
 * so we can test whether the second switch padding and branch targets 
 * get recalculated correctly.
 */

int LookupSwitch() {
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

/* 
 * Generates a class with a deprecated method and a method with the
 * "Synthetic" attribute set.
 */

int MethodDeprecated() {
  JVM_METHOD *main_method, *dep_method, *syn_method;
  JVM_CLASS *cur_class_file;

  cur_class_file = bc_new_class("MethodDeprecated", "asdf.f", NULL, NULL, 
      JVM_ACC_PUBLIC|JVM_ACC_SUPER);

  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  dep_method = bc_new_method(cur_class_file, "foo",
     "()V", JVM_ACC_PUBLIC);
  bc_set_method_deprecated(dep_method);
  bc_gen_return(dep_method);

  syn_method = bc_new_method(cur_class_file, "synth",
     "()V", JVM_ACC_PUBLIC);
  bc_set_method_synthetic(syn_method);
  bc_gen_return(syn_method);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}

/* 
 * Generates some of the method invocation and return instructions
 * mentioned in section 3.11.8 of the JVM spec.  Most of the method
 * invocation instructions are generated in other test programs, so
 * those are not tested here.  This program primarily tests the 
 * bc_gen_return function to make sure that the correct return instruction
 * is generated for the given method descriptor.
 */

int MethodInvocation() {
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

/* 
 * Generates several methods which are declared to throw exceptions.
 */

int MethodWithExceptions() {
  JVM_METHOD *main_method, *no_exc_meth, *one_exc_meth, *two_exc_meth;
  JVM_CLASS *cur_class_file;

  cur_class_file = bc_new_class("MethodWithExceptions", "asdf.f", NULL, NULL, 
      JVM_ACC_PUBLIC|JVM_ACC_SUPER);

  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  no_exc_meth = bc_new_method(cur_class_file, "no_exceptions",
     "()V", JVM_ACC_PUBLIC);
  bc_gen_return(no_exc_meth);

  one_exc_meth = bc_new_method(cur_class_file, "one_exception",
     "()V", JVM_ACC_PUBLIC);
  bc_add_method_exception(one_exc_meth, "java.lang.IllegalAccessException");
  bc_gen_return(one_exc_meth);

  two_exc_meth = bc_new_method(cur_class_file, "two_exceptions",
     "()V", JVM_ACC_PUBLIC);
  bc_add_method_exception(two_exc_meth, "java.lang.reflect.InvocationTargetException");
  bc_add_method_exception(two_exc_meth, "java.lang.IllegalAccessException");
  bc_gen_return(two_exc_meth);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}

/*
 * Generates the various object creation and manipulation instructions 
 * mentioned in section 3.11.5 of the JVM spec (except for creating 
 * one-dimensional arrays and array load/store instructions which are 
 * tested in LoadStore.c).
 */

int ObjectCreation(){
  JVM_METHOD *main_method;
  JVM_METHOD *foo_method;
  JVM_CLASS *cur_class_file;
  int meth_idx;
  int i, obj_var, arr_var;

  cur_class_file = bc_new_class("ObjectCreation", "asdf.f", NULL, 
      NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER);
  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);
  bc_add_field(cur_class_file, "x", "I", JVM_ACC_PUBLIC);
  bc_add_field(cur_class_file, "y", "I", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  foo_method = bc_new_method(cur_class_file, "foo",
     "()V", JVM_ACC_PUBLIC);
  bc_gen_return(foo_method);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  /* create a new instance of this class and call the method "foo" */

  obj_var = bc_get_next_local(main_method, jvm_Object);

  bc_gen_obj_instance_default(main_method, "ObjectCreation");

  bc_gen_store_op(main_method, obj_var, jvm_Object);
  bc_gen_load_op(main_method, obj_var, jvm_Object);

  meth_idx = bc_new_methodref(cur_class_file, "ObjectCreation",
     "foo", "()V");

  bc_append(main_method, jvm_invokevirtual, meth_idx);

  /* create new multi-dimensional array */
  
  arr_var = bc_get_next_local(main_method, jvm_Object);
  bc_push_int_const(main_method, 6);
  bc_push_int_const(main_method, 2);
  bc_new_multi_array(main_method, 2, "[[I");
  bc_gen_store_op(main_method, arr_var, jvm_Object);
  bc_gen_load_op(main_method, arr_var, jvm_Object);
  bc_push_int_const(main_method, 3);
  bc_append(main_method, jvm_aaload);
  bc_push_int_const(main_method, 1);
  bc_push_int_const(main_method, 5);
  bc_append(main_method, jvm_iastore);
  
  /* getting array length */
  bc_gen_load_op(main_method, arr_var, jvm_Object);
  bc_append(main_method, jvm_arraylength);

  /* accessing fields */

  bc_gen_load_op(main_method, obj_var, jvm_Object);
  bc_gen_load_op(main_method, obj_var, jvm_Object);
  bc_get_field(main_method, "ObjectCreation", "x", "I");
  bc_push_int_const(main_method,666);
  bc_append(main_method, jvm_iadd);
  bc_put_field(main_method, "ObjectCreation", "x", "I");

  bc_get_static(main_method, "ObjectCreation", "y", "I");
  bc_push_int_const(main_method,666);
  bc_append(main_method, jvm_iadd);
  bc_put_static(main_method, "ObjectCreation", "y", "I");

  /* bc_gen_instanceof */
  bc_gen_load_op(main_method, obj_var, jvm_Object);
  bc_gen_instanceof(main_method, "java.lang.Object");

  /* bc_gen_checkcast */
  bc_gen_load_op(main_method, obj_var, jvm_Object);
  bc_gen_checkcast(main_method, "java.lang.Object");

  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}

/*
 * Generates the various operand stack management instructions 
 * mentioned in section 3.11.6 of the JVM spec.
 */

int StackManagement(){
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

/* 
 * Generates two exception handlers and the exception table entries
 * for them.
 */

int Synchronization(){
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

/*
 * Generate two switch blocks.  The second switch is after a goto
 * which will get converted to a goto_w, so we can test whether the
 * second switch padding and branch targets get recalculated correctly.
 */

int TableSwitch() {
  JVM_METHOD *main_method;
  JVM_CLASS *cur_class_file;
  int meth_idx, out_idx;
  JVM_CODE_GRAPH_NODE *tswitch, *case1, *case2, *case1_break, *case2_break,
    *target, *goto_node, *after_goto;
  int i;

  cur_class_file = bc_new_class("coverage/test/TableSwitch", "asdf.f", NULL, 
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
  bc_add_switch_case(tswitch, case1, 1);
  bc_push_string_const(main_method, "one");
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
  bc_add_switch_case(tswitch, case1, 1);
  bc_push_string_const(main_method, "one");
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
  bc_add_switch_case(tswitch, case1, 5);
  bc_push_string_const(main_method, "five");
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

/*
 * Generates all the various type conversion instructions mentioned in
 * section 3.11.4 of the JVM spec.
 */

int TypeConversion(){
  JVM_METHOD *main_method;
  JVM_CLASS *cur_class_file;
  int int_var, long_var, float_var, double_var, byte_var, char_var, short_var;
  int i;

  cur_class_file = bc_new_class("coverage.test.TypeConversion", "asdf.f", NULL, 
      NULL, JVM_ACC_PUBLIC|JVM_ACC_SUPER);
  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  int_var = bc_get_next_local(main_method, jvm_Int);
  long_var = bc_get_next_local(main_method, jvm_Long);
  float_var = bc_get_next_local(main_method, jvm_Float);
  double_var = bc_get_next_local(main_method, jvm_Double);
  char_var = bc_get_next_local(main_method, jvm_Char);
  byte_var = bc_get_next_local(main_method, jvm_Byte);
  short_var = bc_get_next_local(main_method, jvm_Short);

  /* Widening conversions */

  bc_push_int_const(main_method, 10);
  bc_append(main_method, jvm_i2l);
  bc_gen_store_op(main_method, long_var, jvm_Long);

  bc_push_int_const(main_method, 10);
  bc_append(main_method, jvm_i2f);
  bc_gen_store_op(main_method, float_var, jvm_Float);

  bc_push_int_const(main_method, 10);
  bc_append(main_method, jvm_i2d);
  bc_gen_store_op(main_method, double_var, jvm_Double);

  bc_push_long_const(main_method, 10);
  bc_append(main_method, jvm_l2f);
  bc_gen_store_op(main_method, float_var, jvm_Float);

  bc_push_long_const(main_method, 10);
  bc_append(main_method, jvm_l2d);
  bc_gen_store_op(main_method, double_var, jvm_Double);

  bc_push_float_const(main_method, 10.0);
  bc_append(main_method, jvm_f2d);
  bc_gen_store_op(main_method, double_var, jvm_Double);

  /* Narrowing conversions */

  bc_push_int_const(main_method, 10);
  bc_append(main_method, jvm_i2b);
  bc_gen_store_op(main_method, byte_var, jvm_Byte);

  bc_push_int_const(main_method, 10);
  bc_append(main_method, jvm_i2c);
  bc_gen_store_op(main_method, char_var, jvm_Char);

  bc_push_int_const(main_method, 10);
  bc_append(main_method, jvm_i2s);
  bc_gen_store_op(main_method, short_var, jvm_Short);

  bc_push_long_const(main_method, 10);
  bc_append(main_method, jvm_l2i);
  bc_gen_store_op(main_method, int_var, jvm_Int);

  bc_push_float_const(main_method, 10.0);
  bc_append(main_method, jvm_f2i);
  bc_gen_store_op(main_method, int_var, jvm_Int);

  bc_push_float_const(main_method, 10.0);
  bc_append(main_method, jvm_f2l);
  bc_gen_store_op(main_method, long_var, jvm_Long);

  bc_push_double_const(main_method, 10.0);
  bc_append(main_method, jvm_d2i);
  bc_gen_store_op(main_method, int_var, jvm_Int);

  bc_push_double_const(main_method, 10.0);
  bc_append(main_method, jvm_d2l);
  bc_gen_store_op(main_method, long_var, jvm_Long);

  bc_push_double_const(main_method, 10.0);
  bc_append(main_method, jvm_d2f);
  bc_gen_store_op(main_method, float_var, jvm_Float);

  bc_gen_return(main_method);

  bc_write_class(cur_class_file, ".");
  bc_free_class(cur_class_file);

  return 0;
}

/*
 * Generates goto instructions with 'labeled' targets.
 */

int Goto(){
  JVM_METHOD *main_method, *dummy_method;
  JVM_CLASS *cur_class_file;
  JVM_CODE_GRAPH_NODE *goto_node, *ret_node, *foo;
  int meth_idx, out_idx, x;

  cur_class_file = bc_new_class("Goto", "asdf.f", NULL, 
      "coverage/test", JVM_ACC_PUBLIC|JVM_ACC_SUPER);
  bc_add_default_constructor(cur_class_file, JVM_ACC_PUBLIC);

  main_method = bc_new_method(cur_class_file, "main",
     "([Ljava/lang/String;)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  dummy_method = bc_new_method(cur_class_file, "dummy",
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

  /* length will always be 0 since we didn't add any instructions to
   * dummy_method.
   */
  x = bc_get_code_length(dummy_method);
  if(x==0) {
    bc_remove_method(dummy_method);
    bc_free_method(dummy_method);
  }

  foo=bc_get_next_instr(goto_node);
  bc_set_stack_depth(goto_node, 0);
  bc_get_last_opcode(main_method);

  dummy_method = bc_new_method(cur_class_file, "dummy",
     "(DD)V", JVM_ACC_PUBLIC|JVM_ACC_STATIC);

  bc_set_gen_status(dummy_method, FALSE);
  foo = bc_append(dummy_method, jvm_nop);
  bc_set_gen_status(dummy_method, TRUE);
  free(foo);

  x = bc_get_next_local(dummy_method, jvm_Int);
  bc_push_int_const(dummy_method, 1);
  bc_gen_store_op(dummy_method, x, jvm_Int);
  bc_release_local(dummy_method,jvm_Int);

  x = bc_get_next_local(dummy_method, jvm_Long);
  bc_push_long_const(dummy_method, 1);
  bc_gen_store_op(dummy_method, x, jvm_Long);
  bc_release_local(dummy_method,jvm_Long);

  bc_set_cur_local_num(dummy_method, 250);
  bc_append(dummy_method, jvm_xxxunusedxxx);
  bc_append(dummy_method, jvm_nop);
  bc_append(dummy_method, jvm_return);

  bc_write_class(cur_class_file, NULL);
  bc_free_class(cur_class_file);

  return 0;
}
