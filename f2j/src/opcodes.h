
/*****************************************************************************
 * opcodes.h                                                                 *
 *                                                                           *
 * Definitions of opcodes related to code generation.                        *
 *                                                                           *
 *****************************************************************************/

#ifndef _OPCODES_H
#define _OPCODES_H

/* data types for f2java primitives: */

char *returnstring[MAX_RETURNS+1] =
{
  "String",
  "String",
  "complex",
  "double",
  "float",
  "int",
  "boolean",
  "Object"
};

/* Mapping between f2java data types and array data types.. used when        */
/* issuing the newarray opcode:                                              */

u2 jvm_array_type[MAX_RETURNS+1] = {
  T_UNUSED, 
  T_UNUSED, 
  T_DOUBLE, 
  T_DOUBLE, 
  T_FLOAT, 
  T_INT, 
  T_BOOLEAN, 
  T_UNUSED
};

/* table of Java's wrapper classes.  we only expect to use the numeric ones  */
char * numeric_wrapper[MAX_RETURNS+1] = {
  "java/lang/String",
  "java/lang/String",
  "java/lang/Double",
  "java/lang/Double",
  "java/lang/Float",
  "java/lang/Integer",
  "java/lang/Boolean",
  "java/lang/Object"
};

/* descriptors for the valueOf() method for the various wrapper classes.     */
char * wrapper_valueOf_descriptor[MAX_RETURNS+1] = {
  "(Ljava/lang/Object;)Ljava/lang/String;",
  "(Ljava/lang/Object;)Ljava/lang/String;",
  "(Ljava/lang/String;)Ljava/lang/Double;",
  "(Ljava/lang/String;)Ljava/lang/Double;",
  "(Ljava/lang/String;)Ljava/lang/Float;",
  "(Ljava/lang/String;)Ljava/lang/Integer;",
  "(Ljava/lang/String;)Ljava/lang/Boolean;",
  "(Ljava/lang/Object;)Ljava/lang/Object;"  /* invalid, but not used */
};

/* descriptors for java/lang/String's valueOf() methods                      */
char * string_valueOf_descriptor[MAX_RETURNS+1] = {
  "asdfjklasdfjkldjf",        /* not used */
  "asdfjklasdfjkldjf",        /* not used */
  "(D)Ljava/lang/String;",
  "(D)Ljava/lang/String;",
  "(F)Ljava/lang/String;",
  "(I)Ljava/lang/String;",
  "(Z)Ljava/lang/String;",
  "asdfjklasdfjkldjf"         /* not used */
};

/* descriptors for the StringBuffer.append() methods                      */
char * append_descriptor[MAX_RETURNS+1] = {
  "(Ljava/lang/String;)Ljava/lang/StringBuffer;",
  "(Ljava/lang/String;)Ljava/lang/StringBuffer;",
  "(D)Ljava/lang/StringBuffer;",
  "(D)Ljava/lang/StringBuffer;",
  "(F)Ljava/lang/StringBuffer;",
  "(I)Ljava/lang/StringBuffer;",
  "(Z)Ljava/lang/StringBuffer;",
  "(Ljava/lang/Object;)Ljava/lang/StringBuffer;",
};

/* descriptors for the numeric wrapper classes' toString() methods           */
char * toString_descriptor[MAX_RETURNS+1] = {
  "()Ljava/lang/String;",
  "()Ljava/lang/String;",
  "(D)Ljava/lang/String;",
  "(D)Ljava/lang/String;",
  "(F)Ljava/lang/String;",
  "(I)Ljava/lang/String;",
  "(Z)Ljava/lang/String;",
  "()Ljava/lang/String;"
};

/* descriptors of PrintStream's print() and println() methods */
char * println_descriptor[MAX_RETURNS+1] = {
  "(Ljava/lang/String;)V",
  "(Ljava/lang/String;)V",
  "(D)V",
  "(D)V",
  "(F)V",
  "(I)V",
  "(Z)V",
  "(Ljava/lang/Object;)V",
};

/* table of numericValue methods (e.g. doubleValue(), intValue(), etc.
 * again, we do not expect to look up String data types in this table,
 * so those values may be invalid.                                          
 */
char * numericValue_method[MAX_RETURNS+1] = {
  "toString",
  "toString",
  "doubleValue",
  "doubleValue",
  "floatValue",
  "intValue",
  "booleanValue",
  "toString"
};

/* method descriptors corresponding to the above methods.                    */
char * numericValue_descriptor[MAX_RETURNS+1] = {
  "()Ljava/lang/String;",
  "()Ljava/lang/String;",
  "()D",
  "()D",
  "()F",
  "()I",
  "()Z",
  "()Ljava/lang/String;"
};

#define JSTR     "Ljava/lang/String;"
#define JSTR_ARR "[Ljava/lang/String;"
#define JOBJ     "Ljava/lang/Object;"
#define JOBJ_ARR "[Ljava/lang/Object;"

/* you'll notice that both the 1D and 2D descriptors are both actually
 * declared 1D.  if we want to implement 'real' 2D arrays, then this
 * matrix (and the following wrapped_field_descriptor) should be updated.
 */

char *field_descriptor[MAX_RETURNS+1][MAX_DIMS+1] = {
  {JSTR, JSTR_ARR,JSTR_ARR,JSTR_ARR},
  {JSTR, JSTR_ARR,JSTR_ARR,JSTR_ARR},
  {"D", "[D", "[D", "[D"},
  {"D", "[D", "[D", "[D"},
  {"F", "[F", "[F", "[F"},
  {"I", "[I", "[I", "[I"},
  {"Z", "[Z", "[Z", "[Z"},
  {JOBJ, JOBJ_ARR,JOBJ_ARR,JOBJ_ARR}
};

char *wrapped_field_descriptor[MAX_RETURNS+1][MAX_DIMS+1] = {
  {"Lorg/netlib/util/StringW;",
   "[Ljava/lang/String;",
   "[Ljava/lang/String;",
   "[Ljava/lang/String;"},
  {"Lorg/netlib/util/StringW;",
   "[Ljava/lang/String;",
   "[Ljava/lang/String;",
   "[Ljava/lang/String;"},
  {"Lorg/netlib/util/complexW;",
   "[Lorg/netlib/util/complexW;",
   "[Lorg/netlib/util/complexW;",
   "[Lorg/netlib/util/complexW;"},
  {"Lorg/netlib/util/doubleW;",
   "[D",
   "[D",
   "[D"},
  {"Lorg/netlib/util/floatW;",
   "[F",
   "[F",
   "[F"},
  {"Lorg/netlib/util/intW;",
   "[I",
   "[I",
   "[I"},
  {"Lorg/netlib/util/booleanW;",
   "[Z",
   "[Z",
   "[Z"},
  {"Ljava/lang/Object;",
   "[Ljava/lang/Object;",
   "[Ljava/lang/Object;",
   "[Ljava/lang/Object;"}
};

/* types for scalars passed by reference:    */
char *wrapper_returns[MAX_RETURNS+1] = 
{
  "StringW",
  "StringW",
  "complexW",
  "doubleW",
  "floatW",
  "intW",
  "booleanW",
  "Object"
};

/* fully qualified wrapper names:   */
char *full_wrappername[MAX_RETURNS+1] =
{
  "org/netlib/util/StringW",
  "org/netlib/util/StringW",
  "org/netlib/util/complexW",
  "org/netlib/util/doubleW",
  "org/netlib/util/floatW",
  "org/netlib/util/intW",
  "org/netlib/util/booleanW",
  "java/lang/Object"
};

/* descriptors of the wrappers' .val fields   */
char *val_descriptor[MAX_RETURNS+1] =
{
  "Ljava/lang/String;",
  "Ljava/lang/String;",
  "D",
  "D",
  "F",
  "I",
  "Z",
  "Ljava/lang/Object;"
};

/* descriptors for the wrapper classes' constructors:         */
char *wrapper_descriptor[MAX_RETURNS+1] =
{
  "(Ljava/lang/String;)V",
  "(Ljava/lang/String;)V",
  "(Lorg/netlib/Complex;)V",
  "(D)V",
  "(F)V",
  "(I)V",
  "(Z)V",
  "",
};

/* names of the standard Java wrappers:  */
char *java_wrapper[MAX_RETURNS+1] =
{
  "String",
  "String",
  "Complex",
  "Double",
  "Float",
  "Integer",
  "Boolean",
  "Object"
};

/* opcodes to push initial primitive values:   */
enum _opcode init_opcodes[MAX_RETURNS+1] =
{
  jvm_nop,
  jvm_nop,
  jvm_dconst_0,
  jvm_dconst_0,
  jvm_fconst_0,
  jvm_iconst_0,
  jvm_iconst_0,
  jvm_nop
};

/* opcodes to load local variables:         */
enum _opcode load_opcodes[MAX_RETURNS+1] =
{
  jvm_aload,
  jvm_aload,
  jvm_dload,
  jvm_dload,
  jvm_fload,
  jvm_iload,
  jvm_iload,
  jvm_aload
};

/* opcodes to load array elements:  */
enum _opcode array_load_opcodes[MAX_RETURNS+1] =
{
  jvm_aaload,
  jvm_aaload,
  jvm_daload,
  jvm_daload,
  jvm_faload,
  jvm_iaload,
  jvm_baload,
  jvm_aaload
};

/* opcodes to store array elements:  */
enum _opcode array_store_opcodes[MAX_RETURNS+1] =
{
  jvm_aastore,
  jvm_aastore,
  jvm_dastore,
  jvm_dastore,
  jvm_fastore,
  jvm_iastore,
  jvm_bastore,
  jvm_aastore
};

/* opcodes to return a value from a function:  */
enum _opcode return_opcodes[MAX_RETURNS+1] =
{
  jvm_areturn,
  jvm_areturn,
  jvm_dreturn,
  jvm_dreturn,
  jvm_freturn,
  jvm_ireturn,
  jvm_ireturn,
  jvm_areturn
};

/* shorthand opcodes for loading local variables:  */
enum _opcode short_load_opcodes[MAX_RETURNS+1][4] =
{
  {jvm_aload_0, jvm_aload_1, jvm_aload_2, jvm_aload_3},
  {jvm_aload_0, jvm_aload_1, jvm_aload_2, jvm_aload_3},
  {jvm_dload_0, jvm_dload_1, jvm_dload_2, jvm_dload_3},
  {jvm_dload_0, jvm_dload_1, jvm_dload_2, jvm_dload_3},
  {jvm_fload_0, jvm_fload_1, jvm_fload_2, jvm_fload_3},
  {jvm_iload_0, jvm_iload_1, jvm_iload_2, jvm_iload_3},
  {jvm_iload_0, jvm_iload_1, jvm_iload_2, jvm_iload_3},
  {jvm_aload_0, jvm_aload_1, jvm_aload_2, jvm_aload_3}
};

/* shorthand opcodes for loading integer constants:  */
enum _opcode iconst_opcodes[7] =
{
  jvm_iconst_m1,
  jvm_iconst_0,
  jvm_iconst_1,
  jvm_iconst_2,
  jvm_iconst_3,
  jvm_iconst_4,
  jvm_iconst_5
};

/* initial values for above data types:  */
char *init_vals[MAX_RETURNS+1] =
{
  "\" \"",
  "\" \"",
  "0",
  "0.0",
  "0.0",
  "0",
  "false"
};

/* input functions to read various data types:   */
char *input_func[MAX_RETURNS+1] =
{
  "readChars",
  "readChars",
  "readComplex",
  "readDouble",
  "readFloat",
  "readInt",
  "readBoolean"
};

/* input functions that detect EOF:    */
char *input_func_eof[MAX_RETURNS+1] =
{
  "readchars",
  "readchars",
  "readcomplex",
  "readdouble",
  "readfloat",
  "readint",
  "readboolean"
};

/* addition opcodes, indexed by vartype:   */
enum _opcode add_opcode[MAX_RETURNS+1] =
{
  jvm_nop,
  jvm_nop,
  jvm_nop,
  jvm_dadd,
  jvm_fadd,
  jvm_iadd,
  jvm_nop
};

/* subtraction opcodes, indexed by vartype:  */
enum _opcode sub_opcode[MAX_RETURNS+1] =  
{
  jvm_nop,
  jvm_nop,
  jvm_nop,
  jvm_dsub,
  jvm_fsub,
  jvm_isub,
  jvm_nop
};

/* division opcodes, indexed by vartype:   */
enum _opcode div_opcode[MAX_RETURNS+1] =  
{
  jvm_nop,
  jvm_nop,
  jvm_nop,
  jvm_ddiv,
  jvm_fdiv,
  jvm_idiv,
  jvm_nop
};

/* multiplication opcodes, indexed by vartype:   */
enum _opcode mul_opcode[MAX_RETURNS+1] =  
{
  jvm_nop,
  jvm_nop,
  jvm_nop,
  jvm_dmul,
  jvm_fmul,
  jvm_imul,
  jvm_nop
};

/* negation opcodes, indexed by vartype:    */
enum _opcode neg_opcode[MAX_RETURNS+1] =  
{
  jvm_nop,
  jvm_nop,
  jvm_nop,
  jvm_dneg,
  jvm_fneg,
  jvm_ineg,
  jvm_nop
};

/* integer comparison opcodes, indexed by vartype.        * 
 * first entry is unused because enum _relop starts at 1  */
enum _opcode icmp_opcode[] = {
  jvm_nop,      
  jvm_if_icmpeq,
  jvm_if_icmpne,
  jvm_if_icmplt,
  jvm_if_icmple,
  jvm_if_icmpgt,
  jvm_if_icmpge,
  jvm_if_icmpge
};

/* comparison ops for relational expressions.  note that the logic is
 * reversed.. that is, this array is indexed by the relops, but each entry
 * contains the reverse relop (e.g. .lt. -> ifgt) except for .eq. and .ne.
 * first entry is unused because enum _relop starts at 1
 */

enum _opcode dcmp_opcode[] = {
  jvm_nop,
  jvm_ifeq,
  jvm_ifne,
  jvm_ifgt,
  jvm_ifge,
  jvm_iflt,
  jvm_ifle
};

/* The following is a table of type conversion opcodes.  to find the
 * appropriate opcode for the conversion, go to the row of the type to
 * convert FROM and scan across to the column of the type to convert TO.
 * most of these entries are blank (NOP) because type promotion does not
 * apply to strings, booleans, etc.   note: most of these are nop because
 * we dont intend to encounter such conversions (or they are unsupported).
 */
enum _opcode typeconv_matrix[MAX_RETURNS+1][MAX_RETURNS+1] =
{
            /* char   |string |complex|double |float  |integer|logical|obj   */
/* char    */ {jvm_nop,jvm_nop,jvm_nop,jvm_nop,jvm_nop,jvm_nop,jvm_nop,jvm_nop},
/* string  */ {jvm_nop,jvm_nop,jvm_nop,jvm_nop,jvm_nop,jvm_nop,jvm_nop,jvm_nop},
/* complex */ {jvm_nop,jvm_nop,jvm_nop,jvm_nop,jvm_nop,jvm_nop,jvm_nop,jvm_nop},
/* double  */ {jvm_nop,jvm_nop,jvm_nop,jvm_nop,jvm_d2f,jvm_d2i,jvm_nop,jvm_nop},
/* float   */ {jvm_nop,jvm_nop,jvm_nop,jvm_f2d,jvm_nop,jvm_f2i,jvm_nop,jvm_nop},
/* integer */ {jvm_nop,jvm_nop,jvm_nop,jvm_i2d,jvm_i2f,jvm_nop,jvm_nop,jvm_nop},
/* logical */ {jvm_nop,jvm_nop,jvm_nop,jvm_nop,jvm_nop,jvm_nop,jvm_nop,jvm_nop},
/* object  */ {jvm_nop,jvm_nop,jvm_nop,jvm_nop,jvm_nop,jvm_nop,jvm_nop,jvm_nop}

};

/* the following structure represents a JVM instruction:  */
typedef struct _jvm_opcode {
  char *op;                   /* character representation of opcode       */
  u1 width;                   /* width in bytes of the opcode + operands  */
  u1 stack_pre;               /* stack before the operation               */
  u1 stack_post;              /* stack after the operation                */
} JVM_OPCODE;

JVM_OPCODE jvm_opcode[] = {
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
  {"tableswitch",   10,  1,  0},
  {"lookupswitch",  10,  1,  0},
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
} ;

#endif
