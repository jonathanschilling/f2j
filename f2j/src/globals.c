/*
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */

/*****************************************************************************
 * globals.c                                                                 *
 *                                                                           *
 * This file contains a lot of globals that are common to many parts of the  *
 * f2java system.                                                            *
 *                                                                           *
 * The following several tables have their last entry initialized            *
 * to `NULL'.  This allows each table to be traversed by a while()           *
 * loop: 'while (tab->entry)' loops until entry is NULL, then                *
 * gracefully exits.  Similarly, a for() loop can be used, for example:      *
 * 'for (tab;tab;tab++)' traverses tab until the NULL last entry is          *
 * reached. See the 'keyscan()' and 'methodscan()' procedures.               *
 *                                                                           *
 *****************************************************************************/

#include"f2j.h"
#include"f2jparse.tab.h" 
#include"opcodes.h"

int 
  lineno,                  /* current line number                            */
  statementno,             /* current statement number                       */
  func_stmt_num,           /* current statement number within this function  */
  ignored_formatting,      /* number of format statements ignored            */
  bad_format_count,        /* number of invalid format stmts encountered     */
  locals,                  /* number of local variables in current unit      */
  stacksize;               /* size of stack for current unit                 */

FILE 
  *ifp,                    /* input file pointer                             */
  *jasminfp,               /* jasmin output file pointer                     */
  *vcgfp,                  /* VCG output file pointer                        */
  *indexfp;                /* method and descriptor index for all prog units */

char 
  *inputfilename,          /* name of the input file                         */
  *package_name,           /* what to name the package, e.g. org.netlib.blas */
  *output_dir;             /* path to which f2java should store class files  */

BOOL 
  omitWrappers,            /* should we try to optimize use of wrappers      */
  genInterfaces,           /* should we generate simplified interfaces       */
  genJavadoc,              /* should we generate javadoc-compatible comments */
  noOffset,                /* should we generate offset args in interfaces   */
  bigEndian;               /* byte order (1 = big, 0 = little)               */

SYMTABLE 
  *type_table,             /* General symbol table                           */
  *external_table,         /* external functions                             */
  *intrinsic_table,        /* intrinsic functions                            */
  *args_table,             /* arguments to the current unit                  */
  *array_table,            /* array variables                                */
  *format_table,           /* format statements                              */
  *data_table,             /* variables contained in DATA statements         */
  *save_table,             /* variables contained in SAVE statements         */
  *common_table,           /* variables contained in COMMON statements       */
  *parameter_table,        /* PARAMETER variables                            */
  *function_table,         /* table of functions                             */
  *java_keyword_table,     /* table of Java reserved words                   */
  *jasmin_keyword_table,   /* table of Jasmin reserved words                 */
  *blas_routine_table,     /* table of BLAS routines                         */
  *common_block_table,     /* COMMON blocks                                  */
  *global_func_table,      /* Global function table                          */
  *global_common_table,    /* Global COMMON table                            */
  *generic_table;          /* table of the generic intrinsic functions       */

Dlist constants_table,     /* constants (for bytecode constant pool gen.)    */
  descriptor_table;        /* list of method descriptors from *.f2j files    */

/*****************************************************************************
 * Statement starting keywords. The only violation of this                   *
 * in fortran 77 is the keyword THEN following a closing                     *
 * parentheses (')').                                                        *
 *****************************************************************************/

KWDTAB tab_stmt[] =
{
    {"CALL", CALL, 0},
    {"CLOSE", CLOSE, 0},
    {"COMMON", COMMON, 0},
    {"CONTINUE", CONTINUE, 0},
    {"DATA", DATA, 0},
    {"DIMENSION", DIMENSION, 0},
    {"DO", DO, 0},
    {"ENDIF", ENDIF, 0},
    {"END", END, 0},
    {"ELSEIF", ELSEIF, 0},
    {"ELSE", ELSE, 0},
    {"ENTRY", ENTRY, 0},
    {"EQUIVALENCE", EQUIVALENCE, 0},
    {"EXTERNAL", EXTERNAL, 0},
    {"FORMAT", FORMAT, 0},
    {"FUNCTION", FUNCTION, 0},
    {"GOTO", GOTO, 0},
    {"IF", IF, 0},
    {"NONE", NONE, 0},
    {"IMPLICIT", IMPLICIT, 0},
    {"INTRINSIC", INTRINSIC, 0},
    {"PARAMETER", PARAMETER, 0},
    {"PROGRAM", PROGRAM, 0},
    {"READ", READ, 0},
    {"RETURN", RETURN, 0},
    {"REWIND", REWIND, 0},
    {"SAVE", SAVE, 0},
    {"STOP", STOP, 0},
    {"SUBROUTINE", SUBROUTINE, 0},
    {"THEN", THEN, 0},
    {"WRITE", WRITE, 0},
    {"PRINT", PRINT, 0},
    { NULL, 0, 0}  /* Ends a scanning loop.  See comment above. */
};

/*****************************************************************************
 *  The type tokens MUST appear at the beginning of a                        *
 * statement, and must occur before any of the                               *
 * executable statements.                                                    *
 *****************************************************************************/

KWDTAB tab_type[] =
{
    {"DOUBLEPRECISION", TYPE, Double},
    {"REAL*8", TYPE, Double},
    {"REAL*4", TYPE, Double},
    {"REAL", TYPE, Double},

    {"INTEGER*4", TYPE, Integer},
    {"INTEGER", TYPE, Integer},

    {"LOGICAL*4", TYPE, Logical},
    {"LOGICAL", TYPE, Logical},

    {"DOUBLECOMPLEX", TYPE, Complex},
    {"COMPLEX*16", TYPE, Complex},
    {"COMPLEX*8", TYPE, Complex},
    {"COMPLEX", TYPE, Complex},

    {"CHARACTER", TYPE, String},
    { NULL, 0, 0}  /* Ends a scanning loop.  See comment above. */
};

/*****************************************************************************
 *  Miscellaneous tokens.  None of these tokens may                          *
 * appear at the beginning fo a statement.                                   *
 *****************************************************************************/

KWDTAB tab_toks[] =
{
    {"\n", NL, 0},   /*  Statement separator. */
    {"+", PLUS, 0},
    {"-", MINUS, 0},
    {"(", OP, 0},
    {")", CP, 0},
    {"**", POW, 0},
    {"*", STAR, 0},
    {"//", CAT, 0},
    {"/", DIV, 0},
    {",", CM, 0},
    {"=", EQ, 0},
    {":", COLON, 0},
    {".NOT.", NOT, 0},
    {".AND.", AND, 0},
    {".OR.", OR, 0},
    {".EQV.", EQV, 0},
    {".NEQV.", NEQV, 0},
    {".EQ.", RELOP, rel_eq},
    {".NE.", RELOP, rel_ne},
    {".LT.", RELOP, rel_lt},
    {".LE.", RELOP, rel_le},
    {".GT.", RELOP, rel_gt},
    {".GE.", RELOP, rel_ge},
    {".TRUE.", TrUE, 1},
    {".FALSE.", FaLSE, 0},
    {"FMT", FMT, 0},
    { NULL, 0, 0}  /*  Ensures that the scanning loop ends if nothing is matched. */
};

/*****************************************************************************
 * Tokens found within a READ statement.  There are probably more that       *
 * should be here, but so far I just have END.                               *
 *****************************************************************************/

KWDTAB read_toks[] =
{
    {"END", END, 0},
    { NULL, 0, 0}  /*  Ensures that the scanning loop ends if nothing is matched. */
};

/*****************************************************************************
 * This table lists stuff that can be handled with java methods.  The        *
 * pattern is {"fortran name", "java method"}.  Some of the fortran names    *
 * are intrinsic to fortran and java, others are intrinsic only to java and  *
 * replace function or sub-routine calls in the lapack or blas source.       *
 * This table may have to be extended to handle jasmin opcodes.              *
 *****************************************************************************/

METHODTAB intrinsic_toks[]=
{
  /* Fortran     Java              Class           Method         Descriptor * 
   *   Name      Name               Name             Name                    * 
   *                                                                         */

  /* Type conversion intrinsics */
  {ifunc_INT, "INT",    "(int)",             "Unused",           "Unused",           "Unused", IRDC_ARGS, Integer},
  {ifunc_IFIX, "IFIX",   "(int)",             "Unused",           "Unused",           "Unused", REAL_ARG, Integer},
  {ifunc_IDINT, "IDINT",  "(int)",             "Unused",           "Unused",           "Unused", DOUBLE_ARG, Integer},
  {ifunc_REAL, "REAL",   "(float)",          "Unused",           "Unused",           "Unused", IRDC_ARGS, Float},
  {ifunc_FLOAT, "FLOAT",  "(float)",          "Unused",           "Unused",           "Unused", INT_ARG, Float},
  {ifunc_SNGL, "SNGL",   "(float)",          "Unused",           "Unused",           "Unused", DOUBLE_ARG, Float},
  {ifunc_DBLE, "DBLE",   "(double)",          "Unused",           "Unused",           "Unused", IRDC_ARGS, Double},
  {ifunc_CMPLX, "CMPLX",  "(Complex)",         "Unused",           "Unused",           "Unused", IRDC_ARGS, Complex},
  {ifunc_ICHAR, "ICHAR",  "(int)",             "Unused",           "Unused",           "Unused", CS_ARGS, Integer},
  {ifunc_CHAR, "CHAR",   "(char)",            "Unused",           "Unused",           "Unused", INT_ARG, Character},

  /* Truncation */
  {ifunc_AINT, "AINT",   "(int)",             "Unused",           "Unused",           "Unused", RD_ARGS, Float},
  {ifunc_DINT, "DINT",   "(int)",             "Unused",           "Unused",           "Unused", DOUBLE_ARG, Double},

  /* Nearest Whole Number - call NINT/IDNINT and then cast to Float/Double */
  {ifunc_ANINT, "ANINT",  "Util.nint",             "org/netlib/util/Util",           "nint",           "(F)I", RD_ARGS, Float},
  {ifunc_DNINT, "DNINT",  "Util.idnint",           "org/netlib/util/Util",           "idnint",           "(D)I", DOUBLE_ARG, Double},

  /* Nearest Integer */
  {ifunc_NINT, "NINT",   "Util.nint",             "org/netlib/util/Util",           "nint",           "(F)I", RD_ARGS, Integer},
  {ifunc_IDNINT, "IDNINT", "Util.idnint",         "org/netlib/util/Util",           "idnint",           "(D)I", DOUBLE_ARG, Integer},

  /* Absolute Value */
  {ifunc_ABS, "ABS",    "Math.abs",          "java/lang/Math",   "abs",              "(F)F", IRDC_ARGS, Double},
  {ifunc_IABS, "IABS",   "Math.abs",          "java/lang/Math",   "abs",              "(I)I", INT_ARG, Integer},
  {ifunc_DABS, "DABS",   "Math.abs",          "java/lang/Math",   "abs",              "(D)D", DOUBLE_ARG, Double},
  {ifunc_CABS, "CABS",   "Math.abs",          "java/lang/Math",   "abs",              "(F)F", COMPLEX_ARG, Float},

  /* Remaindering - directly supported in bytecode by irem, drem, etc */
  {ifunc_MOD, "MOD",    "Unused", "Unused", "Unused", "Unused", IRD_ARGS, Integer},
  {ifunc_AMOD, "AMOD",   "Unused", "Unused", "Unused", "Unused", REAL_ARG, Float},
  {ifunc_DMOD, "DMOD",   "Unused", "Unused", "Unused", "Unused", DOUBLE_ARG, Double},

  /* Transfer of Sign */
  {ifunc_SIGN, "SIGN",   "Util.sign",             "org/netlib/util/Util",           "sign",           "(FF)F", IRD_ARGS, Float},
  {ifunc_ISIGN, "ISIGN",  "Util.isign",             "org/netlib/util/Util",           "isign",           "(II)I", INT_ARG, Integer},
  {ifunc_DSIGN, "DSIGN",  "Util.dsign",             "org/netlib/util/Util",           "dsign",           "(DD)D", DOUBLE_ARG, Double},

  /* Positive Difference */
  {ifunc_DIM, "DIM",    "Util.dim",          "org/netlib/util/Util",   "dim",              "(FF)F", IRD_ARGS, Float},
  {ifunc_IDIM, "IDIM",   "Util.idim",          "org/netlib/util/Util",   "idim",              "(II)I", INT_ARG, Integer},
  {ifunc_DDIM, "DDIM",   "Util.ddim",          "org/netlib/util/Util",   "ddim",              "(DD)D", DOUBLE_ARG, Double},

  /* Double Precision Product of two reals.  implement as (double)a1 * (double)a2  */
  {ifunc_DPROD, "DPROD",  "Unused",          "Unused",   "Unused",              "Unused", REAL_ARG, Double},

  /* Choosing Largest Value */
  {ifunc_MAX,   "MAX",    "Math.max",          "java/lang/Math",   "max",              "(DD)D", IRD_ARGS, Double},
  {ifunc_MAX0,  "MAX0",   "Math.max",          "java/lang/Math",   "max",              "(II)I", INT_ARG, Integer},
  {ifunc_AMAX1, "AMAX1",  "Math.max",          "java/lang/Math",   "max",              "(FF)F", REAL_ARG, Float},
  {ifunc_DMAX1, "DMAX1",  "Math.max",          "java/lang/Math",   "max",              "(DD)D", DOUBLE_ARG, Double},
  {ifunc_AMAX0, "AMAX0",  "Math.max",          "java/lang/Math",   "max",              "(II)I", INT_ARG, Float},
  {ifunc_MAX1,  "MAX1",   "Math.max",          "java/lang/Math",   "max",              "(FF)F", REAL_ARG, Integer},

  /* Choosing Smallest Value */
  {ifunc_MIN,   "MIN",    "Math.min",          "java/lang/Math",   "min",              "(DD)D", IRD_ARGS, Double},
  {ifunc_MIN0,  "MIN0",   "Math.min",          "java/lang/Math",   "min",              "(II)I", INT_ARG, Integer},
  {ifunc_AMIN1, "AMIN1",  "Math.min",          "java/lang/Math",   "min",              "(FF)F", REAL_ARG, Float},
  {ifunc_DMIN1, "DMIN1",  "Math.min",          "java/lang/Math",   "min",              "(DD)D", DOUBLE_ARG, Double},
  {ifunc_AMIN0, "AMIN0",  "Math.min",          "java/lang/Math",   "min",              "(II)I", INT_ARG, Float},
  {ifunc_MIN1,  "MIN1",   "Math.min",          "java/lang/Math",   "min",              "(FF)F", REAL_ARG, Integer},

  /* Length of Character Entity */
  {ifunc_LEN, "LEN",    "Unused",             "Unused",           "Unused",           "Unused", CS_ARGS, Integer},

  /* Location of Substring a2 in String a1 */
  {ifunc_INDEX, "INDEX",    "(int)",             "Unused",           "Unused",           "Unused", CS_ARGS, Integer},

  /* Imaginary Part of Complex Arg */
  {ifunc_AIMAG, "AIMAG",    "(int)",             "Unused",           "Unused",           "Unused", COMPLEX_ARG, Float},

  /* Conjuagate of Complex Argument */
  {ifunc_CONJG, "CONJG",    "(int)",             "Unused",           "Unused",           "Unused", COMPLEX_ARG, Complex},

  /* Sqare Root */
  {ifunc_SQRT, "SQRT",   "Math.sqrt",         "java/lang/Math",   "sqrt",             "(F)F", RDC_ARGS, Float},
  {ifunc_DSQRT, "DSQRT",  "Math.sqrt",         "java/lang/Math",   "sqrt",             "(D)D", DOUBLE_ARG, Double},
  {ifunc_CSQRT, "CSQRT",  "Math.sqrt",         "java/lang/Math",   "sqrt",             "(D)D", COMPLEX_ARG, Complex},

  /* Exponential */
  {ifunc_EXP, "EXP",    "Math.exp",          "java/lang/Math",   "exp",              "(D)D", RDC_ARGS, Float},
  {ifunc_DEXP, "DEXP",    "Math.exp",          "java/lang/Math",   "exp",              "(D)D", DOUBLE_ARG, Double},
  {ifunc_CEXP, "CEXP",    "Math.exp",          "java/lang/Math",   "exp",              "(D)D", COMPLEX_ARG, Complex},

  /* Natural Logarithm */
  {ifunc_LOG, "LOG",    "Math.log",          "java/lang/Math",   "log",              "(D)D", RDC_ARGS, Double},
  {ifunc_ALOG, "ALOG",    "Math.log",          "java/lang/Math",   "log",              "(D)D", REAL_ARG, Float},
  {ifunc_DLOG, "DLOG",    "Math.log",          "java/lang/Math",   "log",              "(D)D", DOUBLE_ARG, Double},
  {ifunc_CLOG, "CLOG",    "Math.log",          "java/lang/Math",   "log",              "(D)D", COMPLEX_ARG, Complex},

  /* Common Logarithm - use java's log function then divide by 2.30258509 */
  {ifunc_LOG10, "LOG10",  "Util.log10",          "org/netlib/util/Util",   "log10",              "(D)D", RD_ARGS, Double},
  {ifunc_ALOG10, "ALOG10",  "Util.log10",          "org/netlib/util/Util",   "log10",              "(D)D", REAL_ARG, Float},
  {ifunc_DLOG10, "DLOG10",  "Util.log10",          "org/netlib/util/Util",   "log10",              "(D)D", DOUBLE_ARG, Double},

  /* Sine */
  {ifunc_SIN, "SIN",    "Math.sin",          "java/lang/Math",   "sin",              "(D)D", RDC_ARGS, Double},
  {ifunc_DSIN, "DSIN",    "Math.sin",          "java/lang/Math",   "sin",              "(D)D", DOUBLE_ARG, Double},
  {ifunc_CSIN, "CSIN",    "Math.sin",          "java/lang/Math",   "sin",              "(D)D", COMPLEX_ARG, Complex},

  /* Cosine */
  {ifunc_COS, "COS",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", RDC_ARGS, Double},
  {ifunc_DCOS, "DCOS",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", DOUBLE_ARG, Double},
  {ifunc_CCOS, "CCOS",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", COMPLEX_ARG, Complex},

  /* Tangent */
  {ifunc_TAN, "TAN",    "Math.tan",          "java/lang/Math",   "tan",              "(D)D", RD_ARGS, Double},
  {ifunc_DTAN, "DTAN",    "Math.tan",          "java/lang/Math",   "tan",              "(D)D", DOUBLE_ARG, Double},

  /* Arcsine */
  {ifunc_ASIN, "ASIN",    "Math.asin",          "java/lang/Math",   "asin",              "(D)D", RD_ARGS, Double},
  {ifunc_DASIN, "DASIN",    "Math.asin",          "java/lang/Math",   "asin",              "(D)D", DOUBLE_ARG, Double},

  /* Arccosine */
  {ifunc_ACOS, "ACOS",    "Math.acos",          "java/lang/Math",   "acos",              "(D)D", RD_ARGS, Double},
  {ifunc_DACOS, "DACOS",    "Math.acos",          "java/lang/Math",   "acos",              "(D)D", DOUBLE_ARG, Double},

  /* Arctangent */
  {ifunc_ATAN, "ATAN",    "Math.atan",          "java/lang/Math",   "atan",              "(D)D", RD_ARGS, Double},
  {ifunc_DATAN, "DATAN",    "Math.atan",          "java/lang/Math",   "atan",              "(D)D", DOUBLE_ARG, Double},
  {ifunc_ATAN2, "ATAN2",    "Math.atan2",          "java/lang/Math",   "atan2",              "(DD)D", RD_ARGS, Double},
  {ifunc_DATAN2, "DATAN2",    "Math.atan2",          "java/lang/Math",   "atan2",              "(DD)D", DOUBLE_ARG, Double},

  /* Hyperbolic Sine */
  {ifunc_SINH, "SINH",    "Util.sinh",          "org/netlib/util/Util",   "sinh",              "(D)D", RD_ARGS, Double},
  {ifunc_DSINH, "DSINH",    "Util.sinh",          "org/netlib/util/Util",   "sinh",              "(D)D", DOUBLE_ARG, Double},

  /* Hyperbolic Cosine */
  {ifunc_COSH, "COSH",    "Util.cosh",          "org/netlib/util/Util",   "cosh",              "(D)D", RD_ARGS, Double},
  {ifunc_DCOSH, "DCOSH",    "Util.cosh",          "org/netlib/util/Util",   "cosh",              "(D)D", DOUBLE_ARG, Double},

  /* Hyperbolic Tangent */
  {ifunc_TANH, "TANH",    "Util.tanh",          "org/netlib/util/Util",   "tanh",              "(D)D", RD_ARGS, Double},
  {ifunc_DTANH, "DTANH",    "Util.tanh",          "org/netlib/util/Util",   "tanh",              "(D)D", DOUBLE_ARG, Double},

  /* Lexically Greater than or Equal to */
  {ifunc_LGE, "LGE",    ".compareTo",          "java/lang/String",   "compareTo",              "(Ljava/lang/String;)I", CS_ARGS, Logical},

  /* Lexically Greater than */
  {ifunc_LGT, "LGT",    ".compareTo",          "java/lang/String",   "compareTo",              "(Ljava/lang/String;)I", CS_ARGS, Logical},

  /* Lexically Less than or Equal to */
  {ifunc_LLE, "LLE",    ".compareTo",          "java/lang/String",   "compareTo",              "(Ljava/lang/String;)I", CS_ARGS, Logical},

  /* Lexically Less than */
  {ifunc_LLT, "LLT",    ".compareTo",          "java/lang/String",   "compareTo",              "(Ljava/lang/String;)I", CS_ARGS, Logical},

  /* fortran pseudo intrinsic */
  {ifunc_ETIME, "ETIME", ".etime",    "org/netlib/util/Etime", "etime",    "([DI)D", IRDC_ARGS, Double}, 

  {ifunc_SECOND, "SECOND", "System.currentTimeMillis",    "java/lang/System", "currentTimeMillis",    "()J", NO_ARG, Double}, 

  /*  Ends a scanning loop.  See comment above. */
  {0, NULL , NULL, NULL, NULL, NULL, 0, 0}    
};

/*****************************************************************************
 * Fortran intrinsics have "generic" versions which can take several data    *
 * types.  we search this list before generating code so that we know        *
 * whether to set the return type based on the arguments.                    *
 *****************************************************************************/

char *generic_intrinsics[] =
{
   "INT", "REAL", "DBLE", "CMPLX", "AINT", "ANINT", "NINT", "ABS", "MOD",
   "SIGN", "DIM", "MAX", "MIN", "SQRT", "EXP", "LOG", "LOG10", "SIN",
   "COS", "TAN", "ASIN", "ACOS", "ATAN", "ATAN2", "SINH", "COSH", "TANH", 0
};

/*****************************************************************************
 * This is a table mapping the Fortran intrinsics onto the Jasmin            *
 * intrinsic calls.  The functions are the same as Java, but the calling     *
 * sequence is different from Java source.                                   *
 *****************************************************************************/

METHODTAB jasmin_intrinsic_toks[]=
{
  {ifunc_MAX, "MAX", 
      "invokestatic java/lang/Math/max(II)I"},
  {ifunc_MIN, "MIN", 
      "invokestatic java/lang/Math/min(II)I"},
  {0, NULL, 0}      /* Ends a scanning loop.  See comment above. */
};

/*****************************************************************************
 *  This is a list of Java reserved words.  If a variable in                 *
 * the Fortran source matches one of these words, it must be                 *
 * transformed before generating the Java source.                            *
 *                                                                           *
 *  This list comes from p. 181 of Java in a Nutshell (David                 *
 * Flanagan) so it should be fairly complete for Java versions               *
 * 1.0.x.  There will probably need to be some added to comply               *
 * with versions 1.1.x.                                                      *
 *****************************************************************************/

char *java_reserved_words[] = 
{
     "abstract" ,    "boolean" ,   "break" ,     "byte" ,   "byvalue" ,
         "cast" ,      "catch" ,    "char" ,    "class" ,     "const" ,  
      "default" ,         "do" ,  "double" ,     "else" ,   "extends" ,    
        "final" ,    "finally" ,   "float" ,      "for" ,    "future" ,   
         "goto" , "implements",       "if" ,   "import" ,     "inner" ,
          "int" ,  "interface" ,    "long" ,   "native" ,       "new" ,      
     "operator" ,      "outer" , "package" ,  "private" , "protected" ,    
         "rest" ,     "return" ,   "short" ,   "static" ,     "super" ,    
 "synchronized" ,       "this" ,   "throw" ,"transient" ,      "true" ,       
          "var" ,       "void" ,"volatile" ,    "while" ,      "null" ,
     "continue" ,      "false" ,    "case" ,  "generic" ,"instanceof" ,
       "public" ,     "switch" ,     "try" ,     0
};

/*****************************************************************************
 *  This is a list of words which will conflict with the Jasmin              *
 * assembler.  During goto translation, we generate a class file             *
 * which is then disassembled into Jasmin assembly code.  If                 *
 * any of these words are used as variable names, Jasmin will                *
 * complain.  I will add names to this list as I run across                  *
 * problems.   12/8/97 --Keith                                               *
 *                                                                           *
 *  When I finish writing the code to directly modify the                    *
 * bytecode, this stuff can be removed.  --Keith                             *
 *****************************************************************************/

char *jasmin_reserved_words[] =
{
     "ldc", "isub", "iinc", 0
};

/*****************************************************************************
 * This is a list of the BLAS routines.  When translating                    *
 * some code, we need to know whether to import the blas                     *
 * library or not.  so we can use this list to determine                     *
 * whether a call is to a BLAS routine or not.                               *
 *****************************************************************************/

char *blas_routines[] = 
{
   "dasum", "daxpy", "dcopy", "ddot",   "dgbmv", "dgemm",
   "dgemv", "dger",  "dnrm2", "drot",   "drotg", "dsbmv",
   "dscal", "dspmv", "dspr",  "dspr2",  "dswap", "dsymm",
   "dsymv", "dsyr",  "dsyr2", "dsyr2k", "dsyrk", "dtbmv",
   "dtbsv", "dtpmv", "dtpsv", "dtrmm",  "dtrmv", "dtrsm",
   "dtrsv", "idamax", 0
};

/*****************************************************************************
 * this is the default mapping of letters to data types.                     *
 *****************************************************************************/

enum returntype default_implicit_table[] =
{
  /* A */ Double,
  /* B */ Double,
  /* C */ Double,
  /* D */ Double,
  /* E */ Double,
  /* F */ Double,
  /* G */ Double,
  /* H */ Double,
  /* I */ Integer,
  /* J */ Integer,
  /* K */ Integer,
  /* L */ Integer,
  /* M */ Integer,
  /* N */ Integer,
  /* O */ Double,
  /* P */ Double,
  /* Q */ Double,
  /* R */ Double,
  /* S */ Double,
  /* T */ Double,
  /* U */ Double,
  /* V */ Double,
  /* W */ Double,
  /* X */ Double,
  /* Y */ Double,
  /* Z */ Double
};

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

char *field_descriptor[MAX_RETURNS+1][2] = {
  {JSTR, JSTR_ARR},
  {JSTR, JSTR_ARR},
  {"D", "[D"},
  {"D", "[D"},
  {"F", "[F"},
  {"I", "[I"},
  {"Z", "[Z"},
  {JOBJ, JOBJ_ARR}
};

char *wrapped_field_descriptor[MAX_RETURNS+1][2] = {
  {"Lorg/netlib/util/StringW;",
   "[Ljava/lang/String;"},
  {"Lorg/netlib/util/StringW;",
   "[Ljava/lang/String;"},
  {"Lorg/netlib/util/complexW;",
   "[Lorg/netlib/util/complexW;"},
  {"Lorg/netlib/util/doubleW;",
   "[D"},
  {"Lorg/netlib/util/floatW;",
   "[F"},
  {"Lorg/netlib/util/intW;",
   "[I"},
  {"Lorg/netlib/util/booleanW;",
   "[Z"},
  {"Ljava/lang/Object;",
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
  "(Ljava/lang/Object;)V",
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

/* opcodes to store local variables:         */
enum _opcode store_opcodes[MAX_RETURNS+1] =
{
  jvm_astore,
  jvm_astore,
  jvm_dstore,
  jvm_dstore,
  jvm_fstore,
  jvm_istore,
  jvm_istore,
  jvm_astore
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

/* shorthand opcodes for storing local variables:  */
enum _opcode short_store_opcodes[MAX_RETURNS+1][4] =
{
  {jvm_astore_0, jvm_astore_1, jvm_astore_2, jvm_astore_3},
  {jvm_astore_0, jvm_astore_1, jvm_astore_2, jvm_astore_3},
  {jvm_dstore_0, jvm_dstore_1, jvm_dstore_2, jvm_dstore_3},
  {jvm_dstore_0, jvm_dstore_1, jvm_dstore_2, jvm_dstore_3},
  {jvm_fstore_0, jvm_fstore_1, jvm_fstore_2, jvm_fstore_3},
  {jvm_istore_0, jvm_istore_1, jvm_istore_2, jvm_istore_3},
  {jvm_istore_0, jvm_istore_1, jvm_istore_2, jvm_istore_3},
  {jvm_astore_0, jvm_astore_1, jvm_astore_2, jvm_astore_3}
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

/* descriptors for EasyIn's read methods */
char *input_descriptors[MAX_RETURNS+1] = 
{
  "(I)Ljava/lang/String;",
  "(I)Ljava/lang/String;",
  "Unimplemented",
  "()D",
  "()F",
  "()I",
  "()Z"
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
  jvm_iflt,
  jvm_ifle,
  jvm_ifgt,
  jvm_ifge
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
};
