#ifndef _CONSTANT_POOL_H
#define _CONSTANT_POOL_H

#include <string.h>
#include "bytecode.h"

static int
  cp_find_function_body(JVM_CLASS *, JVM_CONSTANT, const void *, BOOL),
  cp_lookup_utf8(JVM_CLASS *, const void *),
  cp_lookup_int(JVM_CLASS *, const void *),
  cp_lookup_float(JVM_CLASS *, const void *),
  cp_lookup_long(JVM_CLASS *, const void *),
  cp_lookup_double(JVM_CLASS *, const void *),
  cp_lookup_class(JVM_CLASS *, const void *),
  cp_lookup_ref(JVM_CLASS *, JVM_CONSTANT, const void *),
  cp_lookup_nameandtype(JVM_CLASS *, const void *),
  cp_lookup_string(JVM_CLASS *, const void *),
  cp_insert(JVM_CLASS *, CP_INFO *),
  insert_class(JVM_CLASS *, const void *),
  insert_ref(JVM_CLASS *, JVM_CONSTANT, const void *),
  insert_nameandtype(JVM_CLASS *, const void *),
  insert_utf8(JVM_CLASS *, const void *),
  insert_int_constant(JVM_CLASS *, const void *, BOOL),
  insert_float_constant(JVM_CLASS *, const void *, BOOL),
  insert_long_constant(JVM_CLASS *, const void *, BOOL),
  insert_double_constant(JVM_CLASS *, const void *, BOOL),
  insert_string_constant(JVM_CLASS *, const void *, BOOL),
  insert_constant(JVM_CLASS *, int, const void *, BOOL);

static BOOL
  isBigEndian();

const char * jvm_constant_tags[] = {
  "Unknown CONSTANT",
  "CONSTANT_Utf8",
  "Unknown CONSTANT",
  "CONSTANT_Integer",
  "CONSTANT_Float",
  "CONSTANT_Long",
  "CONSTANT_Double",
  "CONSTANT_Class",
  "CONSTANT_String",
  "CONSTANT_Fieldref",
  "CONSTANT_Methodref",
  "CONSTANT_InterfaceMethodref",
  "CONSTANT_NameAndType"
};

#endif
