/*
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */

/*****************************************************************************
 * codegen.h                                                                 *
 *                                                                           *
 * Definitions of constants related to code generation.                      *
 *                                                                           *
 *****************************************************************************/

#ifndef _CODEGEN_H
#define _CODEGEN_H

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>
#include"f2j.h"
#include"f2jparse.tab.h"
#include"codegen.h"
#include"f2jmem.h"

/*****************************************************************************
 * Following are some fully-qualified class names and method descriptors     *
 * for commonly used methods.                                                *
 *                                                                           *
 * JL_STRING is the fully-qualified name of the String class                 *
 * STR_CONST_DESC is the descriptor for the String constructor               *
 * TRIM_DESC is the descriptor for java.lang.String.trim()                   *
 * STREQV_DESC is the descriptor for java.lang.String.equalsIgnoreCase()     *
 * SUBSTR_DESC is the descriptor for java.lang.String.substring(int,int)     *
 * F2J_UTIL defines the default name of the f2java utility package.          *
 * UTIL_CLASS is where the insertString() method is defined.                 *
 * INS_DESC is the desc for insertString, used for LHS substring assignments *
 * JL_SYSTEM is the fully-qualified name of the System class, for System.out *
 * OUT_DESC is the desc for System.out, the standard output stream.          *
 * STRBUF_DESC is the desc for StringBuffer's constructor.                   *
 *                                                                           *
 *****************************************************************************/

#define JL_STRING "java/lang/String"
#define JL_CHAR "java/lang/Character"
#define JL_OBJECT "java/lang/Object"
#define STR_CONST_DESC "(Ljava/lang/String;)V"
#define TRIM_DESC "()Ljava/lang/String;"
#define STREQV_DESC "(Ljava/lang/String;)Z"
#define SUBSTR_DESC "(II)Ljava/lang/String;"
#define STRLEN_DESC "()I"
#define F2J_UTIL "org/netlib/util"
#define UTIL_CLASS "org/netlib/util/Util"
#define INS_DESC "(Ljava/lang/String;Ljava/lang/String;II)Ljava/lang/String;"
#define JL_SYSTEM "java/lang/System"
#define PRINTSTREAM "java/io/PrintStream"
#define OUT_DESC "Ljava/io/PrintStream;"
#define STRINGBUFFER "java/lang/StringBuffer"
#define STRBUF_DESC "(Ljava/lang/String;)V"
#define REGIONMATCHES_DESC "(ILjava/lang/String;II)Z"
#define TOSTRING_DESC "()Ljava/lang/String;"
#define CHARAT_DESC "(I)C"
#define COMPARE_DESC "(Ljava/lang/String;)I"
#define EASYIN_CLASS "org/netlib/util/EasyIn"
#define EASYIN_DESC "()V"
#define ETIME_CLASS "org/netlib/util/Etime"
#define ETIME_DESC "()V"
#define IOEXCEPTION "java/io/IOException"
#define METHOD_CLASS "java/lang/reflect/Method"
#define GETMETHODS_DESC "()[Ljava/lang/reflect/Method;"
#define JL_CLASS "java/lang/Class"
#define GETCLASS_DESC "()Ljava/lang/Class;"
#define INVOKE_DESC "(Ljava/lang/Object;[Ljava/lang/Object;)Ljava/lang/Object;"
#define THROWABLE_CLASS "java/lang/Throwable"
#define GETMSG_DESC "()Ljava/lang/String;"
#define TOLOWER_DESC "()Ljava/lang/String;"
#define EXIT_DESC "(I)V"
#define PAUSE_DESC "(Ljava/lang/String;)V"
#define PAUSE_NOARG_DESC "()V"
#define INVOKE_EXCEPTION "java/lang/reflect/InvocationTargetException"
#define ACCESS_EXCEPTION "java/lang/IllegalAccessException"

#define THREEARG_MAX_FUNC "Util.max"
#define THREEARG_MIN_FUNC "Util.min"

#define CB_PREFIX "common_block/"
#define CB_DELIMITER '|'
#define CB_SEPARATOR ','

#define MAX(a, b)  (((a) > (b)) ? (a) : (b))

/*****************************************************************************
 * comment out the following line to disable the generation of VCG control   *
 * flow graphs.                                                              *
 *****************************************************************************/
/* #define VCG_CONTROL_FLOW */

/*****************************************************************************
 * Definitions of code generation status.  These are used to set the target  *
 * language that f2java is currently generating.                             *
 *****************************************************************************/

#define JAVA_ONLY     1
#define JVM_ONLY      2
#define JAVA_AND_JVM  3

#define MAX_CODE_LEN 65535

/*****************************************************************************
 * Function prototypes:                                                      *
 *****************************************************************************/

#ifdef VCG_CONTROL_FLOW
void cfg_emit(Dlist, char *);
#endif

char 
  * tok2str(int),
  * strdup ( const char * ),
  * lowercase ( char * ),
  * get_common_prefix(char *),
  * getVarDescriptor(AST *),
  * char_substitution(char *, int, int),
  * get_return_type_from_descriptor(char *),
  * get_wrapper_from_desc(char *),
  * get_field_desc_from_ident(AST *),
  * get_desc_from_arglist(AST *),
  * get_adapter_desc(char *, AST *),
  * getNameFromCommonDesc(char *, int),
  * getFieldDescFromCommonDesc(char *, int),
  * getMergedName(AST *),
  * getMergedDescriptor(AST *, enum returntype),
  * getCommonVarName(AST *);

METHODTAB
  * methodscan (METHODTAB * , char * );

void 
  pushConst(JVM_METHOD *, AST *),
  pushVar(JVM_CLASS *, JVM_METHOD *, enum returntype, BOOL,
    char *, char *, char *, int, BOOL),
  storeVar(JVM_CLASS *, JVM_METHOD *, enum returntype, BOOL,
    char *, char *, char *, int, BOOL),
  arg_array_assign_emit(JVM_CLASS *, JVM_METHOD *, int,
    int, int, enum returntype),
  arg_assignment_emit(JVM_CLASS *, JVM_METHOD *, int, 
    int, int, BOOL, enum returntype),
  read_implied_loop_bytecode_emit(JVM_METHOD *, AST *),
  write_implied_loop_bytecode_emit(JVM_METHOD *, AST *),
  forloop_bytecode_emit(JVM_METHOD *, AST *),
  forloop_end_bytecode(JVM_METHOD *, AST *),
  LHS_bytecode_emit(JVM_METHOD *, AST *),
  stop_emit(JVM_METHOD *, AST *),
  pause_emit(JVM_METHOD *, AST *),
  external_emit(JVM_METHOD *, AST *),
  maxmin_intrinsic_emit(JVM_METHOD *, AST *, METHODTAB *, char *, char *),
  max_intrinsic_emit (JVM_METHOD *, AST *, METHODTAB *),
  min_intrinsic_emit (JVM_METHOD *, AST *, METHODTAB *),
  while_emit(JVM_METHOD *, AST *),
  format_name_emit(JVM_METHOD *, AST *),
  format_list_emit(JVM_METHOD *, AST *, AST **),
  one_arg_write_emit(JVM_METHOD *, AST *),
  substring_assign_emit(JVM_METHOD *, AST *),
  dint_intrinsic_emit(JVM_METHOD *, AST *, METHODTAB *),
  emit_call_args_known(JVM_METHOD *, AST *, char *, BOOL),
  emit_call_args_unknown(JVM_METHOD *, AST *),
  emit_call_arguments(JVM_METHOD *, AST *, BOOL),
  aint_intrinsic_emit(JVM_METHOD *, AST *, METHODTAB *),
  intrinsic_arg_emit(JVM_METHOD *, AST *, enum returntype),
  intrinsic0_call_emit(JVM_METHOD *, AST *, METHODTAB *),
  intrinsic_call_emit(JVM_METHOD *, AST *, METHODTAB *, enum returntype),
  intrinsic2_call_emit(JVM_METHOD *, AST *, METHODTAB *, enum returntype),
  intrinsic_lexical_compare_emit(JVM_METHOD *, AST *, METHODTAB *),
  intrinsic_emit(JVM_METHOD *, AST *),
  implied_loop_emit(JVM_METHOD *, AST *, void (*)(JVM_METHOD *, AST *), 
                                                 void (*)(JVM_METHOD *, AST*)),
  format_emit(JVM_METHOD *, AST *, AST **),
  read_implied_loop_sourcecode_emit(JVM_METHOD *, AST *),
  scalar_emit(JVM_METHOD *, AST *, HASHNODE *),
  write_implied_loop_sourcecode_emit(JVM_METHOD *, AST *),
  array_emit(JVM_METHOD *, AST *),
  emit_interface(AST *),
  substring_emit(JVM_METHOD *, AST *),
  subcall_emit(JVM_METHOD *, AST *),
  emit_methcall(FILE *, AST *),
  name_emit (JVM_METHOD *, AST *),
  print_eqv_list(AST *, FILE *),
  open_output_file(AST *, char *),
  print_string_initializer(JVM_METHOD *, AST *),
  typedec_emit_all_static(JVM_METHOD *, AST *),
  vardec_emit(JVM_METHOD *, AST *, enum returntype, char *),
  assign_varnums_to_locals(JVM_METHOD *, AST *),
  local_emit(JVM_METHOD *, AST *),
  emit_adapters(void),
  newarray_emit(JVM_METHOD *, enum returntype),
  constructor (AST *),
  typedec_emit (JVM_METHOD *, AST *),
  data_emit(JVM_METHOD *, AST *),
  equiv_emit (JVM_METHOD *, AST *),
  call_emit (JVM_METHOD *, AST *),
  forloop_emit (JVM_METHOD *, AST *),
  blockif_emit (JVM_METHOD *, AST *),
  logicalif_emit (JVM_METHOD *, AST *),
  arithmeticif_emit (JVM_METHOD *, AST *),
  goto_emit (JVM_METHOD *, AST *),
  computed_goto_emit (JVM_METHOD *, AST *),
  label_emit (JVM_METHOD *, AST *),
  write_emit (JVM_METHOD *, AST *),
  common_emit(AST *),
  read_emit (JVM_METHOD *, AST *),
  emit_invocations(void),
  merge_equivalences(AST *),
  print_equivalences(AST *),
  emit_prolog_comments(AST *),
  emit_javadoc_comments(AST *),
  insert_fields(AST *),
  return_emit(JVM_METHOD *),
  end_emit(JVM_METHOD *),
  emit (AST *),
  field_emit(AST *),
  invoke_constructor(JVM_METHOD *, char *, AST *, char *),
  set_bytecode_status(JVM_METHOD *, int),
  inline_format_emit(JVM_METHOD *, AST *, BOOL),
  assign_emit (JVM_METHOD *, AST *),
  expr_emit(JVM_METHOD *, AST *),
  substring_expr_emit(JVM_METHOD *, AST *),
  relationalop_emit(JVM_METHOD *, AST *),
  logicalop_emit(JVM_METHOD *, AST *),
  constant_expr_emit(JVM_METHOD *, AST *),
  unaryop_emit(JVM_METHOD *, AST *),
  binaryop_emit(JVM_METHOD *, AST *),
  power_emit(JVM_METHOD *, AST *),
  parenthesized_expr_emit(JVM_METHOD *, AST *),
  else_emit (AST *),
  insert_adapter(AST *),
  insert_methcall(Dlist, AST *),
  reflect_declarations_emit(JVM_METHOD *, AST *),
  data_scalar_emit(JVM_METHOD *, enum returntype, AST *, AST *, int),
  func_array_emit(JVM_METHOD *, AST *, char *, int, int),
  methcall_obj_array_emit(JVM_METHOD *, AST *, int),
  adapter_emit_from_descriptor(JVM_METHOD *, JVM_METHODREF *, AST *),
  adapter_args_emit_from_descriptor(JVM_METHOD *, AST *, char *),
  adapter_temps_emit_from_descriptor(JVM_METHOD *, AST *, char *),
  adapter_methcall_emit_from_descriptor(JVM_METHOD *, AST *, int, JVM_METHODREF *, char *),
  adapter_assign_emit_from_descriptor(JVM_METHOD *, AST *, int, char *),
  adapter_tmp_assign_emit(JVM_METHOD *, int, enum returntype),
  adapter_assign_emit(JVM_METHOD *, int, int, int, char *),
  adapter_array_assign_emit(JVM_METHOD *, int, int, int, char *),
  arrayacc_arg_emit(JVM_METHOD *, AST *, char *, BOOL),
  arrayref_arg_emit(JVM_METHOD *, AST *, char *),
  scalar_arg_emit(JVM_METHOD *, AST *, char *, char *),
  wrapped_arg_emit(JVM_METHOD *, AST *, char *),
  initialize_lists(void),
  free_lists();

int
  assign_varnums_to_arguments(AST *),
  cast_data_stmt(AST *, int),
  cgPassByRef(char *),
  dl_int_examine(Dlist),
  needs_adapter(AST *),
  idxNeedsDecr(AST *),
  method_name_emit (JVM_METHOD *, AST *, BOOL),
  data_repeat_emit(JVM_METHOD *, AST *, AST *, unsigned int),
  methcall_arglist_emit(AST *),
  num_locals_in_descriptor(char *),
  adapter_methcall_arg_emit(JVM_METHOD *, AST *, int, int, char *),
  determine_var_length(HASHNODE *);

double
  eval_const_expr(AST *);

HASHNODE 
  * format_lookup(SYMTABLE *, char *);

JVM_CODE_GRAPH_NODE
  * elseif_emit (JVM_METHOD *, AST *);

AST
  * label_search(Dlist, int),
  * dl_astnode_examine(Dlist),
  * dl_name_search(Dlist, char *),
  * addnode(void),
  * data_var_emit(JVM_METHOD *, AST *, AST *, HASHNODE *),
  * data_implied_loop_emit(JVM_METHOD *, AST * , AST *),
  * data_array_emit(JVM_METHOD *, int , AST *, AST *),
  * format_item_emit(JVM_METHOD *, AST *, AST **);

enum returntype
  get_type_from_field_desc(char *),
  get_type(char *);

JVM_METHODREF
  * get_method_name(AST *, BOOL),
  * get_methodref(AST *),
  * find_commonblock(char *, Dlist),
  * find_method(char *, Dlist);

BOOL
  adapter_insert_from_descriptor(AST *, AST *, char *),
  is_static(AST *),
  is_local(AST *),
  isArrayNoIdx(AST *);

struct var_info
 * get_var_info(AST *),
 * push_array_var(JVM_METHOD *, AST *);


#endif
