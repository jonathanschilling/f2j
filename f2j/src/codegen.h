
/*****************************************************************************
 * codegen.h                                                                 *
 *                                                                           *
 * Definitions of constants related to code generation.                      *
 *                                                                           *
 *****************************************************************************/

#ifndef _CODEGEN_H
#define _CODEGEN_H

/*****************************************************************************
 * MAX_RETURNS represents the number of elements in the returnstring         *
 * array (see below).  OBJECT_TYPE identifies the type 'Object'.             *
 * CPIDX_MAX is the maximum value for a 1-byte constant pool index.          *
 *****************************************************************************/

#define MAX_RETURNS 7
#define MAX_DIMS    3
#define OBJECT_TYPE 7
#define CPIDX_MAX 255

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

/*****************************************************************************
 * Definitions for an expandable string structure.  STR_INIT is the initial  *
 * size of the string, while STR_CHUNK is the number of bytes by which we    *
 * increment the string when it is too small.                                *
 *****************************************************************************/

#define STR_INIT  50
#define STR_CHUNK 20

struct _str {
  int size;
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

#endif
