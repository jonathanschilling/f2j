/*
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */

/*****************************************************************************
 * opcodes.h                                                                 *
 *                                                                           *
 * Definitions of opcodes related to code generation.                        *
 *                                                                           *
 *****************************************************************************/

#ifndef _OPCODES_H
#define _OPCODES_H

/*****************************************************************************
 * MAX_RETURNS is the number of data types.                                  *
 * MAX_DIMS is the maximum number of array dimensions supported.             *
 * OBJECT_TYPE identifies the type 'Object'.                                 *
 * CPIDX_MAX is the maximum value for a 1-byte constant pool index.          *
 *****************************************************************************/

#define MAX_DIMS    3
#define OBJECT_TYPE 7
#define CPIDX_MAX 255
#define MAX_RETURNS 7

#define JSTR     "Ljava/lang/String;"
#define JSTR_ARR "[Ljava/lang/String;"
#define JOBJ     "Ljava/lang/Object;"
#define JOBJ_ARR "[Ljava/lang/Object;"

/* the following structure represents a JVM instruction:  */
typedef struct _jvm_opcode {
  char *op;                   /* character representation of opcode       */
  u1 width;                   /* width in bytes of the opcode + operands  */
  u1 stack_pre;               /* stack before the operation               */
  u1 stack_post;              /* stack after the operation                */
} JVM_OPCODE;

/*****************************************************************************
 * enumeration of all the java opcodes.                                      *
 *****************************************************************************/

enum _opcode {
  jvm_nop = 0x0,
  jvm_aconst_null,
  jvm_iconst_m1,
  jvm_iconst_0,
  jvm_iconst_1,
  jvm_iconst_2,
  jvm_iconst_3,
  jvm_iconst_4,
  jvm_iconst_5,
  jvm_lconst_0,
  jvm_lconst_1,
  jvm_fconst_0,
  jvm_fconst_1,
  jvm_fconst_2,
  jvm_dconst_0,
  jvm_dconst_1,
  jvm_bipush,
  jvm_sipush,
  jvm_ldc,
  jvm_ldc_w,
  jvm_ldc2_w,
  jvm_iload,
  jvm_lload,
  jvm_fload,
  jvm_dload,
  jvm_aload,
  jvm_iload_0,
  jvm_iload_1,
  jvm_iload_2,
  jvm_iload_3,
  jvm_lload_0,
  jvm_lload_1,
  jvm_lload_2,
  jvm_lload_3,
  jvm_fload_0,
  jvm_fload_1,
  jvm_fload_2,
  jvm_fload_3,
  jvm_dload_0,
  jvm_dload_1,
  jvm_dload_2,
  jvm_dload_3,
  jvm_aload_0,
  jvm_aload_1,
  jvm_aload_2,
  jvm_aload_3,
  jvm_iaload,
  jvm_laload,
  jvm_faload,
  jvm_daload,
  jvm_aaload,
  jvm_baload,
  jvm_caload,
  jvm_saload,
  jvm_istore,
  jvm_lstore,
  jvm_fstore,
  jvm_dstore,
  jvm_astore,
  jvm_istore_0,
  jvm_istore_1,
  jvm_istore_2,
  jvm_istore_3,
  jvm_lstore_0,
  jvm_lstore_1,
  jvm_lstore_2,
  jvm_lstore_3,
  jvm_fstore_0,
  jvm_fstore_1,
  jvm_fstore_2,
  jvm_fstore_3,
  jvm_dstore_0,
  jvm_dstore_1,
  jvm_dstore_2,
  jvm_dstore_3,
  jvm_astore_0,
  jvm_astore_1,
  jvm_astore_2,
  jvm_astore_3,
  jvm_iastore,
  jvm_lastore,
  jvm_fastore,
  jvm_dastore,
  jvm_aastore,
  jvm_bastore,
  jvm_castore,
  jvm_sastore,
  jvm_pop,
  jvm_pop2,
  jvm_dup,
  jvm_dup_x1,
  jvm_dup_x2,
  jvm_dup2,
  jvm_dup2_x1,
  jvm_dup2_x2,
  jvm_swap,
  jvm_iadd,
  jvm_ladd,
  jvm_fadd,
  jvm_dadd,
  jvm_isub,
  jvm_lsub,
  jvm_fsub,
  jvm_dsub,
  jvm_imul,
  jvm_lmul,
  jvm_fmul,
  jvm_dmul,
  jvm_idiv,
  jvm_ldiv,
  jvm_fdiv,
  jvm_ddiv,
  jvm_irem,
  jvm_lrem,
  jvm_frem,
  jvm_drem,
  jvm_ineg,
  jvm_lneg,
  jvm_fneg,
  jvm_dneg,
  jvm_ishl,
  jvm_lshl,
  jvm_ishr,
  jvm_lshr,
  jvm_iushr,
  jvm_lushr,
  jvm_iand,
  jvm_land,
  jvm_ior,
  jvm_lor,
  jvm_ixor,
  jvm_lxor,
  jvm_iinc,
  jvm_i2l,
  jvm_i2f,
  jvm_i2d,
  jvm_l2i,
  jvm_l2f,
  jvm_l2d,
  jvm_f2i,
  jvm_f2l,
  jvm_f2d,
  jvm_d2i,
  jvm_d2l,
  jvm_d2f,
  jvm_i2b,
  jvm_i2c,
  jvm_i2s,
  jvm_lcmp,
  jvm_fcmpl,
  jvm_fcmpg,
  jvm_dcmpl,
  jvm_dcmpg,
  jvm_ifeq,
  jvm_ifne,
  jvm_iflt,
  jvm_ifge,
  jvm_ifgt,
  jvm_ifle,
  jvm_if_icmpeq,
  jvm_if_icmpne,
  jvm_if_icmplt,
  jvm_if_icmpge,
  jvm_if_icmpgt,
  jvm_if_icmple,
  jvm_if_acmpeq,
  jvm_if_acmpne,
  jvm_goto,
  jvm_jsr,
  jvm_ret,
  jvm_tableswitch,
  jvm_lookupswitch,
  jvm_ireturn,
  jvm_lreturn,
  jvm_freturn,
  jvm_dreturn,
  jvm_areturn,
  jvm_return,
  jvm_getstatic,
  jvm_putstatic,
  jvm_getfield,
  jvm_putfield,
  jvm_invokevirtual,
  jvm_invokespecial,
  jvm_invokestatic,
  jvm_invokeinterface,
  jvm_xxxunusedxxx,      /* opcode 186 not used */
  jvm_new,
  jvm_newarray,
  jvm_anewarray,
  jvm_arraylength,
  jvm_athrow,
  jvm_checkcast,
  jvm_instanceof,
  jvm_monitorenter,
  jvm_monitorexit,
  jvm_wide,
  jvm_multianewarray,
  jvm_ifnull,
  jvm_ifnonnull,
  jvm_goto_w,
  jvm_jsr_w,
  jvm_breakpoint,
  /* skip 203 - 253 */
  jvm_impdep1 = 254,
  jvm_impdep2
};

/* data types for f2java primitives: */
extern char *returnstring[MAX_RETURNS+1];

/* Mapping between f2java data types and array data types.. */
extern u2 jvm_array_type[MAX_RETURNS+1];

/* table of Java's wrapper classes.  we only expect to use the numeric ones  */
extern char * numeric_wrapper[MAX_RETURNS+1];

/* descriptors for the valueOf() method for the various wrapper classes.     */
extern char * wrapper_valueOf_descriptor[MAX_RETURNS+1];

/* descriptors for java/lang/String's valueOf() methods                      */
extern char * string_valueOf_descriptor[MAX_RETURNS+1];

/* descriptors for the StringBuffer.append() methods                      */
extern char * append_descriptor[MAX_RETURNS+1];

/* descriptors for the numeric wrapper classes' toString() methods           */
extern char * toString_descriptor[MAX_RETURNS+1];

/* descriptors of PrintStream's print() and println() methods */
extern char * println_descriptor[MAX_RETURNS+1];

/* table of numericValue methods (e.g. doubleValue(), intValue(), etc. */
extern char * numericValue_method[MAX_RETURNS+1];

/* method descriptors corresponding to the above methods.                    */
extern char * numericValue_descriptor[MAX_RETURNS+1];
extern char *field_descriptor[MAX_RETURNS+1][MAX_DIMS+1];
extern char *wrapped_field_descriptor[MAX_RETURNS+1][MAX_DIMS+1];

/* types for scalars passed by reference:    */
extern char *wrapper_returns[MAX_RETURNS+1];

/* fully qualified wrapper names:   */
extern char *full_wrappername[MAX_RETURNS+1];

/* descriptors of the wrappers' .val fields   */
extern char *val_descriptor[MAX_RETURNS+1];

/* descriptors for the wrapper classes' constructors:         */
extern char *wrapper_descriptor[MAX_RETURNS+1];

/* names of the standard Java wrappers:  */
extern char *java_wrapper[MAX_RETURNS+1];

/* opcodes to push initial primitive values:   */
extern enum _opcode init_opcodes[MAX_RETURNS+1];

/* opcodes to store local variables:         */
extern enum _opcode store_opcodes[MAX_RETURNS+1];

/* opcodes to load local variables:         */
extern enum _opcode load_opcodes[MAX_RETURNS+1];

/* opcodes to load array elements:  */
extern enum _opcode array_load_opcodes[MAX_RETURNS+1];

/* opcodes to store array elements:  */
extern enum _opcode array_store_opcodes[MAX_RETURNS+1];

/* opcodes to return a value from a function:  */
extern enum _opcode return_opcodes[MAX_RETURNS+1];

/* shorthand opcodes for storing local variables:  */
extern enum _opcode short_store_opcodes[MAX_RETURNS+1][4];

/* shorthand opcodes for loading local variables:  */
extern enum _opcode short_load_opcodes[MAX_RETURNS+1][4];

/* shorthand opcodes for loading integer constants:  */
extern enum _opcode iconst_opcodes[7];

/* initial values for above data types:  */
extern char *init_vals[MAX_RETURNS+1];

/* descriptors for EasyIn's read methods */
extern char *input_descriptors[MAX_RETURNS+1];

/* input functions to read various data types:   */
extern char *input_func[MAX_RETURNS+1];

/* input functions that detect EOF:    */
extern char *input_func_eof[MAX_RETURNS+1];

/* addition opcodes, indexed by vartype:   */
extern enum _opcode add_opcode[MAX_RETURNS+1];

/* subtraction opcodes, indexed by vartype:  */
extern enum _opcode sub_opcode[MAX_RETURNS+1];

/* division opcodes, indexed by vartype:   */
extern enum _opcode div_opcode[MAX_RETURNS+1];

/* multiplication opcodes, indexed by vartype:   */
extern enum _opcode mul_opcode[MAX_RETURNS+1];

/* negation opcodes, indexed by vartype:    */
extern enum _opcode neg_opcode[MAX_RETURNS+1];

/* integer comparison opcodes, indexed by vartype.        */
extern enum _opcode icmp_opcode[];

/* comparison ops for relational expressions.  */
extern enum _opcode dcmp_opcode[];

/* The following is a table of type conversion opcodes. */
extern enum _opcode typeconv_matrix[MAX_RETURNS+1][MAX_RETURNS+1];

extern JVM_OPCODE jvm_opcode[];

#endif
