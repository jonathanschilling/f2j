/** @file globals.c
 * Contains global variables for the library.
 */

#include "bytecode.h"

/**
 * This table stores the number of constant pool entries required
 * by each of the constant pool data types.
 */

const int cp_entry_width[] = 
{
  1,   /*   no tag 0                       */
  1,   /* CONSTANT_Utf8                    */
  1,   /*   tag 2 intentionally missing    */
  1,   /* CONSTANT_Integer                 */
  1,   /* CONSTANT_Float,                  */
  2,   /* CONSTANT_Long,                   */
  2,   /* CONSTANT_Double,                 */
  1,   /* CONSTANT_Class,                  */
  1,   /* CONSTANT_String,                 */
  1,   /* CONSTANT_Fieldref,               */
  1,   /* CONSTANT_Methodref,              */
  1,   /* CONSTANT_InterfaceMethodref,     */
  1,   /* CONSTANT_NameAndType             */
};

/**
 * This table stores the number of local variable entries 
 * required by each of the JVM data types.
 */

const int jvm_localvar_width[] = {
  1,   /* jvm_Byte    */
  1,   /* jvm_Short   */
  1,   /* jvm_Int     */
  2,   /* jvm_Long    */
  1,   /* jvm_Char    */
  1,   /* jvm_Float   */
  2,   /* jvm_Double  */
  1    /* jvm_Object  */
};

/**
 * This table stores the operands for the newarray instruction.
 */

const int jvm_newarray_type[] = {
  JVM_T_BYTE,     /* jvm_Byte    */
  JVM_T_SHORT,    /* jvm_Short   */
  JVM_T_INT,      /* jvm_Int     */
  JVM_T_LONG,     /* jvm_Long    */
  JVM_T_CHAR,     /* jvm_Char    */
  JVM_T_FLOAT,    /* jvm_Float   */
  JVM_T_DOUBLE,   /* jvm_Double  */
  JVM_T_UNUSED    /* jvm_Object  */
};

/**
 * Shorthand opcodes for loading integer constants -1 through 5.
 */

const JVM_OPCODE jvm_iconst_op[7] =
{
  jvm_iconst_m1,
  jvm_iconst_0,
  jvm_iconst_1,
  jvm_iconst_2,
  jvm_iconst_3,
  jvm_iconst_4,
  jvm_iconst_5
};

/**
 * Opcodes to load local variables.
 */

const JVM_OPCODE jvm_load_op[JVM_MAX_RETURNS+1] =
{
  jvm_iload,
  jvm_iload,
  jvm_iload,
  jvm_lload,
  jvm_iload,
  jvm_fload,
  jvm_dload,
  jvm_aload
};

/**
 * Opcodes to load from arrays.
 */ 

const JVM_OPCODE jvm_array_load_op[JVM_MAX_RETURNS+1] =
{ 
  jvm_baload,
  jvm_saload,
  jvm_iaload,
  jvm_laload,
  jvm_caload,
  jvm_faload,
  jvm_daload,
  jvm_aaload
};

/**
 * Opcodes to store local variables.
 */

const JVM_OPCODE jvm_store_op[JVM_MAX_RETURNS+1] =
{
  jvm_istore,
  jvm_istore,
  jvm_istore,
  jvm_lstore,
  jvm_istore,
  jvm_fstore,
  jvm_dstore,
  jvm_astore
};

/**
 * Opcodes to store into arrays.
 */

const JVM_OPCODE jvm_array_store_op[JVM_MAX_RETURNS+1] =
{
  jvm_bastore,
  jvm_sastore,
  jvm_iastore,
  jvm_lastore,
  jvm_castore,
  jvm_fastore,
  jvm_dastore,
  jvm_aastore
};

/**
 * Shorthand opcodes for storing local variables 0 through 3.
 */

const JVM_OPCODE jvm_short_store_op[JVM_MAX_RETURNS+1][4] =
{
  {jvm_istore_0, jvm_istore_1, jvm_istore_2, jvm_istore_3},
  {jvm_istore_0, jvm_istore_1, jvm_istore_2, jvm_istore_3},
  {jvm_istore_0, jvm_istore_1, jvm_istore_2, jvm_istore_3},
  {jvm_lstore_0, jvm_lstore_1, jvm_lstore_2, jvm_lstore_3},
  {jvm_istore_0, jvm_istore_1, jvm_istore_2, jvm_istore_3},
  {jvm_fstore_0, jvm_fstore_1, jvm_fstore_2, jvm_fstore_3},
  {jvm_dstore_0, jvm_dstore_1, jvm_dstore_2, jvm_dstore_3},
  {jvm_astore_0, jvm_astore_1, jvm_astore_2, jvm_astore_3}
};

/**
 * Shorthand opcodes for loading local variables 0 through 3.
 */

const JVM_OPCODE jvm_short_load_op[JVM_MAX_RETURNS+1][4] =
{
  {jvm_iload_0, jvm_iload_1, jvm_iload_2, jvm_iload_3},
  {jvm_iload_0, jvm_iload_1, jvm_iload_2, jvm_iload_3},
  {jvm_iload_0, jvm_iload_1, jvm_iload_2, jvm_iload_3},
  {jvm_lload_0, jvm_lload_1, jvm_lload_2, jvm_lload_3},
  {jvm_iload_0, jvm_iload_1, jvm_iload_2, jvm_iload_3},
  {jvm_fload_0, jvm_fload_1, jvm_fload_2, jvm_fload_3},
  {jvm_dload_0, jvm_dload_1, jvm_dload_2, jvm_dload_3},
  {jvm_aload_0, jvm_aload_1, jvm_aload_2, jvm_aload_3}
};

/**
 * This table stores information about all the JVM instructions.
 * Each entry has four parts: 
 *
 *   -# opcode - string representation of the opcode
 *   -# width - total width of the instruction plus operands
 *   -# pre-stack - number of stack items popped before issuing the instruction
 *   -# post-stack - number of stack items pushed after issuing the instruction
 */

const JVM_OP_INFO jvm_opcode[] = {
  {"nop",            1,  0,  0},
  {"aconst_null",    1,  0,  1},
  {"iconst_m1",      1,  0,  1},
  {"iconst_0",       1,  0,  1},
  {"iconst_1",       1,  0,  1},
  {"iconst_2",       1,  0,  1},
  {"iconst_3",       1,  0,  1},
  {"iconst_4",       1,  0,  1},
  {"iconst_5",       1,  0,  1},
  {"lconst_0",       1,  0,  2},
  {"lconst_1",       1,  0,  2},
  {"fconst_0",       1,  0,  1},
  {"fconst_1",       1,  0,  1},
  {"fconst_2",       1,  0,  1},
  {"dconst_0",       1,  0,  2},
  {"dconst_1",       1,  0,  2},
  {"bipush",         2,  0,  1},
  {"sipush",         3,  0,  1},
  {"ldc",            2,  0,  1},
  {"ldc_w",          3,  0,  1},
  {"ldc2_w",         3,  0,  2},
  {"iload",          2,  0,  1},
  {"lload",          2,  0,  2},
  {"fload",          2,  0,  1},
  {"dload",          2,  0,  2},
  {"aload",          2,  0,  1},
  {"iload_0",        1,  0,  1},
  {"iload_1",        1,  0,  1},
  {"iload_2",        1,  0,  1},
  {"iload_3",        1,  0,  1},
  {"lload_0",        1,  0,  2},
  {"lload_1",        1,  0,  2},
  {"lload_2",        1,  0,  2},
  {"lload_3",        1,  0,  2},
  {"fload_0",        1,  0,  1},
  {"fload_1",        1,  0,  1},
  {"fload_2",        1,  0,  1},
  {"fload_3",        1,  0,  1},
  {"dload_0",        1,  0,  2},
  {"dload_1",        1,  0,  2},
  {"dload_2",        1,  0,  2},
  {"dload_3",        1,  0,  2},
  {"aload_0",        1,  0,  1},
  {"aload_1",        1,  0,  1},
  {"aload_2",        1,  0,  1},
  {"aload_3",        1,  0,  1},
  {"iaload",         1,  2,  1},
  {"laload",         1,  2,  2},
  {"faload",         1,  2,  1},
  {"daload",         1,  2,  2},
  {"aaload",         1,  2,  1},
  {"baload",         1,  2,  1},
  {"caload",         1,  2,  1},
  {"saload",         1,  2,  1},
  {"istore",         2,  1,  0},
  {"lstore",         2,  2,  0},
  {"fstore",         2,  1,  0},
  {"dstore",         2,  2,  0},
  {"astore",         2,  1,  0},
  {"istore_0",       1,  1,  0},
  {"istore_1",       1,  1,  0},
  {"istore_2",       1,  1,  0},
  {"istore_3",       1,  1,  0},
  {"lstore_0",       1,  2,  0},
  {"lstore_1",       1,  2,  0},
  {"lstore_2",       1,  2,  0},
  {"lstore_3",       1,  2,  0},
  {"fstore_0",       1,  1,  0},
  {"fstore_1",       1,  1,  0},
  {"fstore_2",       1,  1,  0},
  {"fstore_3",       1,  1,  0},
  {"dstore_0",       1,  2,  0},
  {"dstore_1",       1,  2,  0},
  {"dstore_2",       1,  2,  0},
  {"dstore_3",       1,  2,  0},
  {"astore_0",       1,  1,  0},
  {"astore_1",       1,  1,  0},
  {"astore_2",       1,  1,  0},
  {"astore_3",       1,  1,  0},
  {"iastore",        1,  3,  0},
  {"lastore",        1,  4,  0},
  {"fastore",        1,  3,  0},
  {"dastore",        1,  4,  0},
  {"aastore",        1,  3,  0},
  {"bastore",        1,  3,  0},
  {"castore",        1,  3,  0},
  {"sastore",        1,  3,  0},
  {"pop",            1,  1,  0},
  {"pop2",           1,  2,  0},
  {"dup",            1,  1,  2},
  {"dup_x1",         1,  2,  3},
  {"dup_x2",         1,  3,  4},
  {"dup2",           1,  2,  4},
  {"dup2_x1",        1,  3,  5},
  {"dup2_x2",        1,  4,  6},
  {"swap",           1,  2,  2},
  {"iadd",           1,  2,  1},
  {"ladd",           1,  4,  2},
  {"fadd",           1,  2,  1},
  {"dadd",           1,  4,  2},
  {"isub",           1,  2,  1},
  {"lsub",           1,  4,  2},
  {"fsub",           1,  2,  1},
  {"dsub",           1,  4,  2},
  {"imul",           1,  2,  1},
  {"lmul",           1,  4,  2},
  {"fmul",           1,  2,  1},
  {"dmul",           1,  4,  2},
  {"idiv",           1,  2,  1},
  {"ldiv",           1,  4,  2},
  {"fdiv",           1,  2,  1},
  {"ddiv",           1,  4,  2},
  {"irem",           1,  2,  1},
  {"lrem",           1,  4,  2},
  {"frem",           1,  2,  1},
  {"drem",           1,  4,  2},
  {"ineg",           1,  1,  1},
  {"lneg",           1,  2,  2},
  {"fneg",           1,  1,  1},
  {"dneg",           1,  2,  2},
  {"ishl",           1,  2,  1},
  {"lshl",           1,  3,  2},
  {"ishr",           1,  2,  1},
  {"lshr",           1,  3,  2},
  {"iushr",          1,  2,  1},
  {"lushr",          1,  3,  2},
  {"iand",           1,  2,  1},
  {"land",           1,  4,  2},
  {"ior",            1,  2,  1},
  {"lor",            1,  4,  2},
  {"ixor",           1,  2,  1},
  {"lxor",           1,  4,  2},
  {"iinc",           3,  0,  0},
  {"i2l",            1,  1,  2},
  {"i2f",            1,  1,  1},
  {"i2d",            1,  1,  2},
  {"l2i",            1,  2,  1},
  {"l2f",            1,  2,  1},
  {"l2d",            1,  2,  2},
  {"f2i",            1,  1,  1},
  {"f2l",            1,  1,  2},
  {"f2d",            1,  1,  2},
  {"d2i",            1,  2,  1},
  {"d2l",            1,  2,  2},
  {"d2f",            1,  2,  1},
  {"i2b",            1,  1,  1},
  {"i2c",            1,  1,  1},
  {"i2s",            1,  1,  1},
  {"lcmp",           1,  4,  1},
  {"fcmpl",          1,  2,  1},
  {"fcmpg",          1,  2,  1},
  {"dcmpl",          1,  4,  1},
  {"dcmpg",          1,  4,  1},
  {"ifeq",           3,  1,  0},
  {"ifne",           3,  1,  0},
  {"iflt",           3,  1,  0},
  {"ifge",           3,  1,  0},
  {"ifgt",           3,  1,  0},
  {"ifle",           3,  1,  0},
  {"if_icmpeq",      3,  2,  0},
  {"if_icmpne",      3,  2,  0},
  {"if_icmplt",      3,  2,  0},
  {"if_icmpge",      3,  2,  0},
  {"if_icmpgt",      3,  2,  0},
  {"if_icmple",      3,  2,  0},
  {"if_acmpeq",      3,  2,  0},
  {"if_acmpne",      3,  2,  0},
  {"goto",           3,  0,  0},
  {"jsr",            3,  0,  1},
  {"ret",            2,  0,  0},
  {"tableswitch",   13,  1,  0},
  {"lookupswitch",   9,  1,  0},
  {"ireturn",        1,  1,  0},
  {"lreturn",        1,  2,  0},
  {"freturn",        1,  1,  0},
  {"dreturn",        1,  2,  0},
  {"areturn",        1,  1,  0},
  {"return",         1,  0,  0},
  {"getstatic",      3,  0,  1},
  {"putstatic",      3,  1,  0},
  {"getfield",       3,  1,  9},
  {"putfield",       3,  9,  0},
  {"invokevirtual",  3,  9,  0},
  {"invokespecial",  3,  9,  0},
  {"invokestatic",   3,  9,  0},
  {"invokeinterface",5,  9,  0},
  {"UNUSED",         1,  0,  0},
  {"new",            3,  0,  1},
  {"newarray",       2,  1,  1},
  {"anewarray",      3,  1,  1},
  {"arraylength",    1,  1,  1},
  {"athrow",         1,  1,  0},
  {"checkcast",      3,  1,  1},
  {"instanceof",     3,  1,  1},
  {"monitorenter",   1,  1,  0},
  {"monitorexit",    1,  1,  0},
  {"wide",           1,  0,  0},
  {"multianewarray", 4,  9,  1},  
  {"ifnull",         3,  1,  0},
  {"ifnonnull",      3,  1,  0},
  {"goto_w",         5,  0,  0},
  {"jsr_w",          5,  0,  1},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0},
  {"UNUSED",         1,  0,  0}
};
