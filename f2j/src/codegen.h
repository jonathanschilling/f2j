
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
#include"class.h"
#include"f2jparse.tab.h"
#include"constant_pool.h"
#include"codegen.h"
#include"opcodes.h"
#include"graph.h"

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
#define STREQV_DESC "()Z"
#define SUBSTR_DESC "(II)Ljava/lang/String;"
#define F2J_UTIL "org/netlib/util"
#define UTIL_CLASS "org/netlib/util/Util"
#define INS_DESC "(Ljava/lang/String;Ljava/lang/String;II)Ljava/lang/String;"
#define JL_SYSTEM "java/lang/System"
#define PRINTSTREAM "java/io/PrintStream"
#define OUT_DESC "Ljava/io/PrintStream;"
#define STRINGBUFFER "java/lang/StringBuffer"
#define STRBUF_DESC "(Ljava/lang/String;)V"
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
#define INVOKE_EXCEPTION "java/lang/reflect/InvocationTargetException"
#define ACCESS_EXCEPTION "java/lang/IllegalAccessException"

#define THREEARG_MAX_FUNC "Util.max"
#define THREEARG_MIN_FUNC "Util.min"

#define MAX(a, b)  (((a) > (b)) ? (a) : (b))

/*****************************************************************************
 * Definitions for an expandable string structure.  STR_INIT is the initial  *
 * size of the string, while STR_CHUNK is the number of bytes by which we    *
 * increment the string when it is too small.                                *
 *****************************************************************************/

#define STR_INIT  50
#define STR_CHUNK 20

struct _str {
  unsigned int size;
  char *val;
};

/*****************************************************************************
 * Definitions of code generation status.  These are used to set the target  *
 * language that f2java is currently generating.                             *
 *****************************************************************************/

#define JAVA_ONLY     1
#define JVM_ONLY      2
#define JAVA_AND_JVM  3

/*****************************************************************************
 * this structure holds information about the state of the stack before and  *
 * after a method call.  to correctly calculate the maximum stack depth, we  *
 * need to know how many arguments an invoke[static,virtual,etc] instruction *
 * will pop off the stack.  even though there is only one return value, it   *
 * can occupy zero, one, or two stack entries depending on the return type   *
 * of the method.                                                            *
 *****************************************************************************/

struct stack_info {
  int arg_len,       /* depth of stack when this method is invoked           */
      ret_len;       /* depth of stack when this method returns              */
};

/*****************************************************************************
 * Function prototypes:                                                      *
 *****************************************************************************/

char 
  * tok2str(int),
  * strdup ( const char * ),
  * print_nodetype ( AST * ),
  * lowercase ( char * ),
  * skipToken(char *),
  * get_common_prefix(char *),
  * getVarDescriptor(AST *),
  * char_substitution(char *, int, int),
  * get_full_classname(char *),
  * get_desc_from_arglist(AST *);

METHODTAB
  * methodscan (METHODTAB * , char * );

void 
  external_emit(AST *),
  maxmin_intrinsic_emit(AST *, char *, METHODTAB *, char *, char *),
  max_intrinsic_emit (AST *, char *, METHODTAB *),
  min_intrinsic_emit (AST *, char *, METHODTAB *),
  arg_assignment_emit(int, int, int, BOOLEAN, enum returntype),
  calcOffsets(CodeGraphNode *),
  traverse_code(Dlist),
  while_emit(AST *),
  format_name_emit(AST *),
  format_list_emit(AST *, AST **),
  one_arg_write_emit(AST *),
  forloop_end_bytecode(AST *),
  substring_assign_emit(AST *),
  dint_intrinsic_emit(AST *, METHODTAB *),
  emit_call_args_known(AST *, HASHNODE *, BOOLEAN),
  emit_call_args_unknown(AST *),
  emit_call_arguments(AST *, BOOLEAN),
  aint_intrinsic_emit(AST *, METHODTAB *),
  intrinsic_arg_emit(AST *, enum returntype),
  intrinsic_call_emit(AST *, METHODTAB *, enum returntype),
  intrinsic2_call_emit(AST *, METHODTAB *, enum returntype),
  intrinsic_lexical_compare_emit(AST *, METHODTAB *),
  intrinsic_emit(AST *),
  implied_loop_emit(AST *, void (AST *), void (AST*)),
  format_emit(AST *, AST **),
  read_implied_loop_bytecode_emit(AST *),
  read_implied_loop_sourcecode_emit(AST *),
  scalar_emit(AST *, HASHNODE *),
  write_implied_loop_bytecode_emit(AST *),
  write_implied_loop_sourcecode_emit(AST *),
  array_emit(AST *, HASHNODE *),
  emit_interface(AST *),
  substring_emit(AST *),
  subcall_emit(AST *),
  emit_methcall(FILE *, AST *),
  name_emit (AST *),
  print_eqv_list(AST *, FILE *),
  addField(char *, char *),
  open_output_file(AST *, char *),
  print_string_initializer(AST *),
  vardec_emit(AST *, enum returntype),
  emit_adapters(void),
  newarray_emit(AST *),
  constructor (AST *),
  typedec_emit (AST *),
  data_emit(AST *),
  spec_emit (AST *),
  equiv_emit (AST *),
  call_emit (AST *),
  forloop_emit (AST *),
  blockif_emit (AST *),
  logicalif_emit (AST *),
  arithmeticif_emit (AST *),
  goto_emit (AST *),
  computed_goto_emit (AST *),
  label_emit (AST *),
  write_emit (AST *),
  common_emit(AST *),
  read_emit (AST *),
  emit_invocations(AST *),
  merge_equivalences(AST *),
  print_equivalences(AST *),
  emit_prolog_comments(AST *),
  emit_javadoc_comments(AST *),
  insert_fields(AST *),
  assign_local_vars(AST *),
  return_emit(void),
  end_emit(AST *),
  emit (AST *),
  pushConst(AST *),
  field_emit(AST *),
  pushIntConst(int),
  pushDoubleConst(double),
  pushStringConst(char *),
  pushVar(enum returntype, BOOLEAN, char *, char *, char *, unsigned int, BOOLEAN),
  dec_stack(int),
  iinc_emit(unsigned int, int),
  invoke_constructor(char *, AST *, char *),
  set_bytecode_status(int),
  inline_format_emit(AST *, BOOLEAN),
  endNewMethod(struct ClassFile *, struct method_info *, char *, char *, unsigned int, Dlist),
  releaseLocal(enum returntype),
  assign_emit (AST *),
  expr_emit(AST *),
  forloop_bytecode_emit(AST *),
  else_emit (AST *),
  LHS_bytecode_emit(AST *),
  insert_adapter(AST *),
  insert_methcall(Dlist, AST *),
  reflect_declarations_emit(AST *),
  invocation_exception_handler_emit(ExceptionTableEntry *),
  data_scalar_emit(enum returntype, AST *, AST *, int),
  func_array_emit(AST *, HASHNODE *, char *, int, int),
  methcall_obj_array_emit(AST *, int),
  inc_stack(int);

int
  isPassByRef(char *),
  dl_int_examine(Dlist),
  opWidth(enum _opcode),
  getNextLocal(enum returntype),
  needs_adapter(AST *),
  idxNeedsDecr(AST *),
  getStackIncrement(enum _opcode, u4),
  getStackDecrement(enum _opcode, u4),
  method_name_emit (AST *, BOOLEAN),
  data_repeat_emit(AST *, unsigned int),
  methcall_arglist_emit(AST *),
  num_locals_in_descriptor(char *),
  determine_var_length(HASHNODE *);

double
  eval_const_expr(AST *);

HASHNODE 
  * format_lookup(SYMTABLE *, char *);

struct ClassFile 
  * newClassFile(char *,char *);

struct attribute_info
  * newCodeAttribute(void),
  * newExceptionsAttribute(Dlist);

struct method_info 
  * beginNewMethod(unsigned int);

CodeGraphNode
  * bytecode0(enum _opcode),
  * bytecode1(enum _opcode, u4),
  * nodeAtPC(int),
  * gen_store_op(unsigned int, enum returntype),
  * gen_load_op(unsigned int, enum returntype),
  * newGraphNode(enum _opcode, u4),
  * elseif_emit (AST *);

AST
  * label_search(Dlist, int),
  * dl_astnode_examine(Dlist),
  * dl_name_search(Dlist, char *),
  * find_label(Dlist, int),
  * addnode(void),
  * data_var_emit(AST *, AST *, HASHNODE *),
  * data_implied_loop_emit(AST * , AST *),
  * data_array_emit(int , AST *, AST *, int),
  * format_item_emit(AST *, AST **);

enum returntype
  get_type(char *);

struct _str
  * strAppend(struct _str *, char *);

METHODREF
  * get_method_name(AST *, BOOLEAN);

struct stack_info * calcStack(char *);

METHODREF
  * find_method(char *, Dlist);

BOOLEAN
  adapter_insert_from_table(AST *, AST *, HASHNODE *),
  adapter_insert_from_descriptor(AST *, AST *, char *);

#endif
