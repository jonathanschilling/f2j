
/*****************************************************************************
 * class.h                                                                   *
 *                                                                           *
 * Header file defining class file structures.                               *
 *                                                                           *
 *****************************************************************************/

#ifndef _CLASS_H
#define _CLASS_H

#include "dlist.h"

enum _constant_tags {
  CONSTANT_Utf8 = 1,              /*   1  */
   /* note missing number 2 */
  CONSTANT_Integer = 3,           /*   3  */
  CONSTANT_Float,                 /*   4  */
  CONSTANT_Long,                  /*   5  */
  CONSTANT_Double,                /*   6  */
  CONSTANT_Class,                 /*   7  */
  CONSTANT_String,                /*   8  */
  CONSTANT_Fieldref,              /*   9  */
  CONSTANT_Methodref,             /*  10  */
  CONSTANT_InterfaceMethodref,    /*  11  */
  CONSTANT_NameAndType            /*  12  */
};

static char * constant_tags [] = {
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

typedef unsigned char  u1;
typedef unsigned short u2;
typedef unsigned int   u4;

struct ClassFile {
  u4 magic;                    /* class file magic number: 0xCAFEBABE         */
  u2 minor_version;            /* minor version of the class file             */
  u2 major_version;            /* major version of the class file             */
  u2 constant_pool_count;      /* num entries in constant pool + 1            */
  Dlist constant_pool;         /* constant pool:constant_pool_count-1 entries */
  u2 access_flags;             /* access permissions for this class           */
  u2 this_class;               /* cp index to entry representing this class   */
  u2 super_class;              /* cp index to superclass or 0 for Object      */
  u2 interfaces_count;         /* number of superinterfaces for this class    */
  u2 * interfaces;             /* list of interfaces (each entry a cp index)  */
  u2 fields_count;             /* num fields, both class vars & instance vars */
  struct field_info * fields;  /* list of fields declared in this class       */
  u2 methods_count;            /* number of methods in this class             */
  struct method_info *methods; /* list of methods                             */
  u2 attributes_count;         /* number of attributes for this class         */
  struct attribute_info *attributes;  /* only SourceFile & Deprecated here    */
};

struct CONSTANT_Class_info {
  u2 name_index;              /* index into constant pool                    */
};

struct CONSTANT_Fieldref_info {
  u2 class_index;             /* cp index of class which declares this field */
  u2 name_and_type_index;     /* cp index of name & descriptor of this field */
};

struct CONSTANT_Methodref_info {
  u2 class_index;             /* cp index of class which declares this field */
  u2 name_and_type_index;     /* cp index of name & descriptor of this field */
};

struct CONSTANT_InterfaceMethodref_info {
  u2 class_index;             /* cp index of class which declares this field */
  u2 name_and_type_index;     /* cp index of name & descriptor of this field */
};

struct CONSTANT_String_info {
  u2 string_index;            /* cp index of Utf8 rep of this string         */
};

struct CONSTANT_Integer_info {
  u4 bytes;                   /* the integer value                           */
};

struct CONSTANT_Float_info {
  u4 bytes;                   /* the float value                             */
};

struct CONSTANT_Long_info {
  u4 high_bytes;              /* the high bytes of the long value            */
  u4 low_bytes;               /* the low bytes of the long value             */
};

struct CONSTANT_Double_info {
  union {
    struct _hilo {
      u4 high_bytes;              /* the high bytes of the double value          */
      u4 low_bytes;               /* the low bytes of the double value           */
    } hilo;
    double dblbytes;
  } bytes;
};

struct CONSTANT_NameAndType_info {
  u2 name_index;              /* cp index of name or <init> stored as Utf8   */
  u2 descriptor_index;        /* cp index of valid field, method descriptor  */
};

struct CONSTANT_Utf8_info {
  u2 length;                  /* # bytes, not necessarily string length      */
  u1 *bytes;                  /* byte array containing the Utf8 string       */
};

struct cp_info {
  u1 tag;
  union {
    struct CONSTANT_Class_info                 Class;
    struct CONSTANT_Fieldref_info              Fieldref;
    struct CONSTANT_Methodref_info             Methodref;
    struct CONSTANT_InterfaceMethodref_info    InterfaceMethodref;
    struct CONSTANT_String_info                String;
    struct CONSTANT_Integer_info               Integer;
    struct CONSTANT_Float_info                 Float;
    struct CONSTANT_Long_info                  Long;
    struct CONSTANT_Double_info                Double;
    struct CONSTANT_NameAndType_info           NameAndType;
    struct CONSTANT_Utf8_info                  Utf8;
  } cpnode;
};
  
struct field_info {
  u2 access_flags;            /* access flags mask, see table 4.4 in vm spec */
  u2 name_index;              /* cp index of field name, rep. as Utf8 string */
  u2 descriptor_index;        /* cp index of valid field descriptor          */
  u2 attributes_count;        /* number of additional field attributes       */

  struct attribute_info
     *attributes;             /* attributes of this field                    */
};

struct method_info {
  u2 access_flags;            /* access flags mask, see table 4.5 in vm spec */
  u2 name_index;              /* cp index of methodname, <init>, or <clinit> */
  u2 descriptor_index;        /* cp index of valid method descriptor         */
  u2 attributes_count;        /* number of additional method attributes      */

  struct attribute_info 
     *attributes;             /* attributes of this method                   */
};

struct attribute_info {
  u2 attribute_name_index;    /* cp index to name of attribute (in Utf8)     */
  u4 attribute_length;        /* # bytes pointed to by the info field        */
  u1 * info;                  /* ptr to byte array representing attribute    */
};

struct ConstantValue_attribute {
  u2 attribute_name_index;    /* cp index to name ("ConstantValue")          */
  u4 attribute_length;        /* length must be 2 for ConstantValue          */
  u2 constantvalue_index;     /* cp index to the actual constant value       */
};

struct Code_attribute {
  u2 attribute_name_index;    /* cp index to name ("Code")                   */
  u4 attribute_length;        /* length of the attribute in bytes            */
  u2 max_stack;               /* max depth of operand stack for this method  */
  u2 max_locals;              /* max num of local variables including params */
  u4 code_length;             /* number of bytes in the code array           */
  u1 * code;                  /* byte array containing code for this method  */
  u2 exception_table_length;  /* number of entries in the exception table    */
  struct {
    u2 start_pc;              /* valid index into code of starting opcode    */
    u2 end_pc;                /* valid index into code of ending opcode      */
    u2 handler_pc;            /* start of exception handler code             */
    u2 catch_type;            /* cp index of exception class to catch        */
  } * exception_table;
  u2 attributes_count;        /* number of additional code attributes        */

  struct attribute_info
     *attributes;             /* attributes of this code                     */
};

struct Exceptions_attribute {
  u2 attribute_name_index;    /* cp index to name ("Exceptions")             */
  u4 attribute_length;        /* length of the attribute in bytes            */
  u2 number_of_exceptions;    /* number of entries in exception_index_table  */
  u2 *exception_index_table;  /* table of exceptions a method can throw      */
};

struct Synthetic_attribute {
  u2 attribute_name_index;    /* cp index to name ("Synthetic")              */
  u4 attribute_length;        /* length must be zero for Synthetic attribute */
};

struct SourceFile_attribute {
  u2 attribute_name_index;    /* cp index to name ("SourceFile")             */
  u4 attribute_length;        /* length must be 2 for SourceFile attribute   */
  u2 sourcefile_index;        /* cp index to name of source file (in Utf8)   */
};

struct LineNumberTable_attribute {
  u2 attribute_name_index;    /* cp index to name ("LineNumberTable")        */
  u4 attribute_length;        /* length of the attribute in bytes            */
  u2 line_number_table_length; /* number of entries in line_number_table     */
  struct {  
    u2 start_pc;              /* idx to code where original src stmt begins  */
    u2 line_number;           /* the corresponding original line number      */
  } * line_number_table;
};

struct LocalVariableTable_attribute {
  u2 attribute_name_index;    /* cp index to name ("LocalVariableTable")     */
  u4 attribute_length;        /* length of the attribute in bytes            */
  u2 local_variable_table_length; /* number of entries in line_number_table  */
  struct {  
    u2 start_pc;              /* start idx of valid range for this variable  */
    u2 length;                /* offset from start_pc marking end of range   */
    u2 name_index;            /* cp index to name of variable                */
    u2 descriptor_index;      /* cp index to descriptor for variable         */
    u2 index;                 /* this variable's index into local var table  */
  } * local_variable_table;
};

enum _opcode {       /* enumeration of all the java opcodes */
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
#endif
