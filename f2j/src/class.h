
/*****************************************************************************
 * class.h                                                                   *
 *                                                                           *
 * Header file defining class file structures.                               *
 *                                                                           *
 *****************************************************************************/

#ifndef _CLASS_H
#define _CLASS_H

#include<stdio.h>
#include "dlist.h"

/*
 * Definitions of class/field/method modifiers:
 */

#define ACC_PUBLIC       0x0001
#define ACC_PRIVATE      0x0002
#define ACC_PROTECTED    0x0004
#define ACC_STATIC       0x0008
#define ACC_FINAL        0x0010
#define ACC_SYNCHRONIZED 0x0020
#define ACC_SUPER        0x0020
#define ACC_NATIVE       0x0100
#define ACC_INTERFACE    0x0200
#define ACC_ABSTRACT     0x0400
#define ACC_STRICT       0x0800

/*
 * array data types for newarray opcode.
 */
#define T_UNUSED  0
#define T_BOOLEAN 4
#define T_CHAR    5
#define T_FLOAT   6
#define T_DOUBLE  7
#define T_BYTE    8
#define T_SHORT   9
#define T_INT    10
#define T_LONG   11

#define JVM_MAGIC 0xCAFEBABEu
#define JVM_MINOR_VER 3
#define JVM_MAJOR_VER 45

FILE * fopen_fullpath(char *, char *);

/*
 * Structures representing the JVM class file.
 */

enum _constant_tags {
  CONSTANT_Utf8 = 1,              /*   1  */
                   /* note missing tag 2  */
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
  Dlist fields;                /* list of fields declared in this class       */
  u2 methods_count;            /* number of methods in this class             */
  Dlist methods;               /* list of methods                             */
  u2 attributes_count;         /* number of attributes for this class         */
  Dlist attributes;            /* only SourceFile & Deprecated allowed here   */
};

struct CONSTANT_Class_info {
  u2 name_index;              /* index into constant pool                    */
};

struct CONSTANT_Methodref_info {
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
  u4 high_bytes;              /* the high bytes of the double value          */
  u4 low_bytes;               /* the low bytes of the double value           */
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
    struct CONSTANT_Methodref_info             Methodref;
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
  Dlist attributes;           /* attributes of this field                    */
};

struct method_info {
  u2 access_flags;            /* access flags mask, see table 4.5 in vm spec */
  u2 name_index;              /* cp index of methodname, <init>, or <clinit> */
  u2 descriptor_index;        /* cp index of valid method descriptor         */
  u2 attributes_count;        /* number of additional method attributes      */
  Dlist attributes;           /* attributes of this method                   */
};

struct ConstantValue_attribute {
  u2 constantvalue_index;     /* cp index to the actual constant value       */
};

struct ExceptionTable {
  u2 start_pc;              /* index into code of start opcode (inclusive) */
  u2 end_pc;                /* index into code of end opcode (exclusive)   */
  u2 handler_pc;            /* start of exception handler code             */
  u2 catch_type;            /* cp index of exception class to catch        */
};

struct Code_attribute {
  u2 max_stack;               /* max depth of operand stack for this method  */
  u2 max_locals;              /* max num of local variables including params */
  u4 code_length;             /* number of bytes in the code array           */
  Dlist code;                 /* list containing code for this method        */
  u2 exception_table_length;  /* number of entries in the exception table    */

  struct ExceptionTable * exception_table;  /* table of exception handlers   */

  u2 attributes_count;        /* number of additional code attributes        */
  Dlist attributes;           /* attributes of this code                     */
};

struct Exceptions_attribute {
  u2 number_of_exceptions;    /* number of entries in exception_index_table  */
  Dlist exception_index_table;/* table of exceptions a method can throw      */
};

struct SourceFile_attribute {
  u2 sourcefile_index;        /* cp index to name of source file (in Utf8)   */
};

struct LineNumberTable_attribute {
  u2 line_number_table_length; /* number of entries in line_number_table     */
  struct {  
    u2 start_pc;              /* idx to code where original src stmt begins  */
    u2 line_number;           /* the corresponding original line number      */
  } * line_number_table;
};

struct LocalVariableTable_attribute {
  u2 local_variable_table_length; /* number of entries in line_number_table  */
  struct {  
    u2 start_pc;              /* start idx of valid range for this variable  */
    u2 length;                /* offset from start_pc marking end of range   */
    u2 name_index;            /* cp index to name of variable                */
    u2 descriptor_index;      /* cp index to descriptor for variable         */
    u2 index;                 /* this variable's index into local var table  */
  } * local_variable_table;
};

struct attribute_info {
  u2 attribute_name_index;    /* cp index to name of attribute (in Utf8)     */
  u4 attribute_length;        /* # bytes pointed to by the info field        */
  union {
    struct ConstantValue_attribute      * ConstantValue;
    struct Code_attribute               * Code;
    struct Exceptions_attribute         * Exceptions;
    void                                * Synthetic;
    struct SourceFile_attribute         * SourceFile;
    struct LineNumberTable_attribute    * LineNumberTable;
    struct LocalVariableTable_attribute * LocalVariableTable;
  } attr;
};

/*
 * Function prototypes. 
 */

void   write_class(struct ClassFile *);
void   write_constant_pool(struct ClassFile *, FILE *); 
void   write_interfaces(struct ClassFile *, FILE *); 
void   write_fields(struct ClassFile *, FILE *); 
void   write_methods(struct ClassFile *, FILE *); 
void   write_u1(u1, FILE *);
void   write_u2(u2, FILE *);
void   write_u4(u4, FILE *);

int    write_attributes(Dlist, Dlist, FILE *);

FILE * open_output_classfile(struct ClassFile *);

#endif
