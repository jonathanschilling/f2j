
/*****************************************************************************
 * class.h                                                                   *
 *                                                                           *
 * Header file defining class file structures.                               *
 *                                                                           *
 *****************************************************************************/

typedef unsigned char  u1;
typedef unsigned short u2;
typedef unsigned int   u4;

struct ClassFile {
  u4 magic;                   /* class file magic number: 0xCAFEBABE         */
  u2 minor_version;           /* minor version of the class file             */
  u2 major_version;           /* major version of the class file             */
  u2 constant_pool_count;     /* num entries in constant pool + 1            */
  cp_info * constant_pool;    /* constant pool:constant_pool_count-1 entries */
  u2 access_flags;            /* access permissions for this class           */
  u2 this_class;              /* cp index to entry representing this class   */
  u2 super_class;             /* cp index to superclass or 0 for Object      */
  u2 interfaces_count;        /* number of superinterfaces for this class    */
  u2 * interfaces;            /* list of interfaces (each entry a cp index)  */
  u2 fields_count;            /* num fields, both class vars & instance vars */
  field_info * fields;        /* list of fields declared in this class       */
  u2 methods_count;           /* number of methods in this class             */
  method_info * methods;      /* list of methods                             */
  u2 attributes_count;        /* number of attributes for this class         */
  attribute_info *attributes; /* can only use SourceFile & Deprecated here   */
};

struct cp_info {
  u1 tag;                     /* the type of structure stored at this entry  */
  u1 *info;                   /* byte array representing the structure       */
};

struct CONSTANT_Class_info {
  u1 tag;                     /* tag = CONSTANT_Class                        */
  u2 name_index;              /* index into constant pool                    */
};

struct CONSTANT_Fieldref_info {
  u1 tag;                     /* tag = CONSTANT_Fieldref                     */
  u2 class_index;             /* cp index of class which declares this field */
  u2 name_and_type_index;     /* cp index of name & descriptor of this field */
};

struct CONSTANT_Methodref_info {
  u1 tag;                     /* tag = CONSTANT_Methodref                    */
  u2 class_index;             /* cp index of class which declares this field */
  u2 name_and_type_index;     /* cp index of name & descriptor of this field */
};

struct CONSTANT_InterfaceMethodref_info {
  u1 tag;                     /* tag = CONSTANT_InterfaceMethodref           */
  u2 class_index;             /* cp index of class which declares this field */
  u2 name_and_type_index;     /* cp index of name & descriptor of this field */
};

struct CONSTANT_String_info {
  u1 tag;                     /* tag = CONSTANT_String                       */
  u2 string_index;            /* cp index of Utf8 rep of this string         */
};

struct CONSTANT_Integer_info {
  u1 tag;                     /* tag = CONSTANT_Integer                      */
  u4 bytes;                   /* the integer value                           */
};

struct CONSTANT_Float_info {
  u1 tag;                     /* tag = CONSTANT_Float                        */
  u4 bytes;                   /* the float value                             */
};

struct CONSTANT_Long_info {
  u1 tag;                     /* tag = CONSTANT_Long                         */
  u4 high_bytes;              /* the high bytes of the long value            */
  u4 low_bytes;               /* the low bytes of the long value             */
};

struct CONSTANT_Double_info {
  u1 tag;                     /* tag = CONSTANT_Double                       */
  u4 high_bytes;              /* the high bytes of the double value          */
  u4 low_bytes;               /* the low bytes of the double value           */
};

struct CONSTANT_NameAndType_info {
  u1 tag;                     /* tag = CONSTANT_NameAndType                  */
  u2 name_index;              /* cp index of name or <init> stored as Utf8   */
  u2 descriptor_index;        /* cp index of valid field, method descriptor  */
};

struct CONSTANT_Utf8_info {
  u1 tag;                     /* tag = CONSTANT_Utf8                         */
  u2 length;                  /* # bytes, not necessarily string length      */
  u1 *bytes;                  /* byte array containing the Utf8 string       */
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
  nop = 0x0,
  aconst_null,
  iconst_m1,
  iconst_0,
  iconst_1,
  iconst_2,
  iconst_3,
  iconst_4,
  iconst_5,
  lconst_0,
  lconst_1,
  fconst_0,
  fconst_1,
  fconst_2,
  dconst_0,
  dconst_1,
  bipush,
  sipush,
  ldc,
  ldc_w,
  ldc2_w,
  iload,
  lload,
  fload,
  dload,
  aload,
  iload_0,
  iload_1,
  iload_2,
  iload_3,
  lload_0,
  lload_1,
  lload_2,
  lload_3,
  fload_0,
  fload_1,
  fload_2,
  fload_3,
  dload_0,
  dload_1,
  dload_2,
  dload_3,
  aload_0,
  aload_1,
  aload_2,
  aload_3,
  iaload,
  laload,
  faload,
  daload,
  aaload,
  baload,
  caload,
  saload,
  istore,
  lstore,
  fstore,
  dstore,
  astore,
  istore_0,
  istore_1,
  istore_2,
  istore_3,
  lstore_0,
  lstore_1,
  lstore_2,
  lstore_3,
  fstore_0,
  fstore_1,
  fstore_2,
  fstore_3,
  dstore_0,
  dstore_1,
  dstore_2,
  dstore_3,
  astore_0,
  astore_1,
  astore_2,
  astore_3,
  iastore,
  lastore,
  fastore,
  dastore,
  aastore,
  bastore,
  castore,
  sastore,
  pop,
  pop2,
  dup,
  dup_x1
  dup_x2,
  dup2,
  dup2_x1,
  dup2_x2,
  swap,
  iadd,
  ladd,
  fadd,
  dadd,
  isub,
  lsub,
  fsub,
  dsub,
  imul,
  lmul,
  fmul,
  dmul,
  idiv,
  ldiv,
  fdiv,
  ddiv,
  irem,
  lrem,
  frem,
  drem,
  ineg,
  lneg,
  fneg,
  dneg,
  ishl,
  lshl,
  ishr,
  lshr,
  iushr,
  lushr,
  iand,
  land,
  ior,
  lor,
  ixor,
  lxor,
  iinc,
  i2l,
  i2f,
  i2d,
  l2i,
  l2f,
  l2d,
  f2i,
  f2l,
  f2d,
  d2i,
  d2l,
  d2f,
  i2b,
  i2c,
  i2s,
  lcmp,
  fcmpl,
  fcmpg,
  dcmpl,
  dcmpg,
  ifeq,
  ifne,
  iflt,
  ifge,
  ifgt,
  ifle,
  if_icmpeq,
  if_icmpne,
  if_icmplt,
  if_icmpge,
  if_icmpgt,
  if_icmple,
  if_acmpeq,
  if_acmpne,
  goto,
  jsr,
  ret,
  tableswitch,
  lookupswitch,
  ireturn,
  lreturn,
  freturn,
  dreturn,
  areturn,
  return,
  getstatic,
  putstatic,
  getfield,
  putfield,
  invokevirtual,
  invokespecial,
  invokestatic,
  invokeinterface,
  xxxunusedxxx,      /* opcode 186 not used */
  new,
  newarray,
  anewarray,
  arraylength,
  athrow,
  checkcast,
  instanceof,
  monitorenter,
  monitorexit,
  wide,
  multianewarray,
  ifnull,
  ifnonnull,
  goto_w,
  jsr_w,
  breakpoint,
    /* skip 203 - 253 */
  impdep1 = 254,
  impdep2
};
