#ifndef _CLASS_H
#define _CLASS_H

#include<stdio.h>
#include "bytecode.h"

static BOOL
  check_distance(JVM_OPCODE, int, int);

static void
  write_constant_pool(JVM_CLASS *, FILE *),
  write_interfaces(JVM_CLASS *, FILE *),
  write_fields(JVM_CLASS *, FILE *),
  write_methods(JVM_CLASS *, FILE *),
  write_code(Dlist, FILE *),
  write_exception_table(struct ExceptionTable *, int, FILE *),
  write_u1(u1, FILE *),
  write_u2(u2, FILE *),
  write_u3(u4, FILE *),
  write_u4(u4, FILE *),
  write_tableswitch(JVM_CODE_GRAPH_NODE *, FILE *),
  write_lookupswitch(JVM_CODE_GRAPH_NODE *, FILE *),
  dec_stack(JVM_METHOD *, int),
  inc_stack(JVM_METHOD *, int),
  calc_offsets(JVM_METHOD *meth, JVM_CODE_GRAPH_NODE *);

static int
  find_label(Dlist, const char *),
  write_attributes(JVM_CLASS *, Dlist, FILE *),
  num_empty_switch_cases(JVM_CODE_GRAPH_NODE *),
  switch_entry_compare(const void *, const void *),
  setup_tableswitch(JVM_CODE_GRAPH_NODE *),
  setup_lookupswitch(JVM_CODE_GRAPH_NODE *),
  finalizeMethod(JVM_METHOD *),
  get_stack_increment(JVM_METHOD *, JVM_OPCODE, u4),
  get_stack_decrement(JVM_METHOD *, JVM_OPCODE, u4),
  get_stack_dec_field_acc(JVM_CLASS *, JVM_OPCODE, u4),
  get_stack_dec_invocation(JVM_CLASS *, JVM_OPCODE, u4),
  get_stack_inc_field_acc(JVM_CLASS *, JVM_OPCODE, u4),
  get_stack_inc_invocation(JVM_CLASS *, JVM_OPCODE, u4),
  traverse_code(JVM_METHOD *);

static JVM_STACK_INFO
  *calc_stack(char *);

static Dlist
  get_list_node(Dlist, JVM_CODE_GRAPH_NODE *);

static double
  math_pow(double, double);

static JVM_OPCODE
  get_method_return_op(char *);

static FILE
  *open_output_classfile(JVM_CLASS *, char *);

#endif
