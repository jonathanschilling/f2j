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

int 
  lineno,                  /* current line number                            */
  statementno,             /* current statement number                       */
  func_stmt_num,           /* current statement number within this function  */
  ignored_formatting,      /* number of format statements ignored            */
  bad_format_count;        /* number of invalid format stmts encountered     */

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
  strictFp,                /* should we declare generated code as strictfp   */
  strictMath,              /* should we use Java's StrictMath library        */
  omitWrappers,            /* should we try to optimize use of wrappers      */
  genInterfaces,           /* should we generate simplified interfaces       */
  genJavadoc,              /* should we generate javadoc-compatible comments */
  noOffset,                /* should we generate offset args in interfaces   */
  f2j_arrays_static,       /* force all arrays to be declared static.        */
  save_all_override;       /* force all variables to be declared static.     */

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

Dlist 
  constants_table,         /* constants (for bytecode constant pool gen.)    */
  descriptor_table,        /* list of method descriptors from *.f2j files    */
  include_paths,           /* list of paths to search for included files     */
  file_stack;              /* file stack for handling include statements     */

INCLUDED_FILE
  *current_file_info;      /* lexer information about the current file       */

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
    {"ENDDO", ENDDO, 0},
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
    {"OPEN", OPEN, 0},
    {"IMPLICIT", IMPLICIT, 0},
    {"INTRINSIC", INTRINSIC, 0},
    {"PARAMETER", PARAMETER, 0},
    {"PROGRAM", PROGRAM, 0},
    {"READ", READ, 0},
    {"RETURN", RETURN, 0},
    {"REWIND", REWIND, 0},
    {"SAVE", SAVE, 0},
    {"STOP", STOP, 0},
    {"PAUSE", PAUSE, 0},
    {"SUBROUTINE", SUBROUTINE, 0},
    {"THEN", THEN, 0},
    {"WRITE", WRITE, 0},
    {"PRINT", PRINT, 0},
    {"ASSIGN", ASSIGN, 0},
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
    {"REAL*4", TYPE, Float},
    {"REAL", TYPE, Float},

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
 * Tokens found within an OPEN statement.  There are probably more that      *
 * should be here.                                                           *
 *****************************************************************************/

KWDTAB open_toks[] =
{
    {"IOSTAT", OPEN_IOSTAT, 0},
    {"ERR", OPEN_ERR, 0},
    {"FILE", OPEN_FILE, 0},
    {"STATUS", OPEN_STATUS, 0},
    {"ACCESS", OPEN_ACCESS, 0},
    {"FORM", OPEN_FORM, 0},
    {"UNIT", OPEN_UNIT, 0},
    {"RECL", OPEN_RECL, 0},
    {"BLANK", OPEN_BLANK, 0},
    { NULL, 0, 0}  /*  Ensures that the scanning loop ends if nothing is matched. */
};

/*****************************************************************************
 * Tokens found within an ASSIGN statement.                                  *
 *****************************************************************************/

KWDTAB assign_toks[] =
{
    {"TO", TO, 0},
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
  /* Type conversion intrinsics */
  {ifunc_INT,   "INT",   "(int)",     NULL, "Unused", NULL, "Unused", "Unused", IRDC_ARGS,  Integer},
  {ifunc_IFIX,  "IFIX",  "(int)",     NULL, "Unused", NULL, "Unused", "Unused", REAL_ARG,   Integer},
  {ifunc_IDINT, "IDINT", "(int)",     NULL, "Unused", NULL, "Unused", "Unused", DOUBLE_ARG, Integer},
  {ifunc_REAL,  "REAL",  "(float)",   NULL, "Unused", NULL, "Unused", "Unused", IRDC_ARGS,  Float},
  {ifunc_FLOAT, "FLOAT", "(float)",   NULL, "Unused", NULL, "Unused", "Unused", INT_ARG,    Float},
  {ifunc_SNGL,  "SNGL",  "(float)",   NULL, "Unused", NULL, "Unused", "Unused", DOUBLE_ARG, Float},
  {ifunc_DBLE,  "DBLE",  "(double)",  NULL, "Unused", NULL, "Unused", "Unused", IRDC_ARGS,  Double},
  {ifunc_CMPLX, "CMPLX", "(Complex)", NULL, "Unused", NULL, "Unused", "Unused", IRDC_ARGS,  Complex},
  {ifunc_ICHAR, "ICHAR", "(int)",     NULL, "Unused", NULL, "Unused", "Unused", CS_ARGS,    Integer},
  {ifunc_CHAR,  "CHAR",  "(char)",    NULL, "Unused", NULL, "Unused", "Unused", INT_ARG,    Character},

  /* Truncation */
  {ifunc_AINT, "AINT", "(int)", NULL, "Unused", NULL, "Unused", "Unused", RD_ARGS,    Float},
  {ifunc_DINT, "DINT", "(int)", NULL, "Unused", NULL, "Unused", "Unused", DOUBLE_ARG, Double},

  /* Nearest Whole Number - call NINT/IDNINT and then cast to Float/Double */
  {ifunc_ANINT, "ANINT", "Util.nint",   "StrictUtil.nint",   "org/netlib/util/Util", "org/netlib/util/StrictUtil", "nint",   "(F)I", RD_ARGS,    Float},
  {ifunc_DNINT, "DNINT", "Util.idnint", "StrictUtil.idnint", "org/netlib/util/Util", "org/netlib/util/StrictUtil", "idnint", "(D)I", DOUBLE_ARG, Double},

  /* Nearest Integer */
  {ifunc_NINT,   "NINT",   "Util.nint",   "StrictUtil.nint",   "org/netlib/util/Util", "org/netlib/util/StrictUtil", "nint",   "(F)I", RD_ARGS,    Integer},
  {ifunc_IDNINT, "IDNINT", "Util.idnint", "StrictUtil.idnint", "org/netlib/util/Util", "org/netlib/util/StrictUtil", "idnint", "(D)I", DOUBLE_ARG, Integer},

  /* Absolute Value */
  {ifunc_ABS,  "ABS",  "Math.abs", "StrictMath.abs", "java/lang/Math", "java/lang/StrictMath", "abs", "(F)F", IRDC_ARGS,   Double},
  {ifunc_IABS, "IABS", "Math.abs", "StrictMath.abs", "java/lang/Math", "java/lang/StrictMath", "abs", "(I)I", INT_ARG,     Integer},
  {ifunc_DABS, "DABS", "Math.abs", "StrictMath.abs", "java/lang/Math", "java/lang/StrictMath", "abs", "(D)D", DOUBLE_ARG,  Double},
  {ifunc_CABS, "CABS", "Math.abs", "StrictMath.abs", "java/lang/Math", "java/lang/StrictMath", "abs", "(F)F", COMPLEX_ARG, Float},

  /* Remaindering - directly supported in bytecode by irem, drem, etc */
  {ifunc_MOD,  "MOD",  "Unused", NULL, "Unused", NULL, "Unused", "Unused", IRD_ARGS,   Integer},
  {ifunc_AMOD, "AMOD", "Unused", NULL, "Unused", NULL, "Unused", "Unused", REAL_ARG,   Float},
  {ifunc_DMOD, "DMOD", "Unused", NULL, "Unused", NULL, "Unused", "Unused", DOUBLE_ARG, Double},

  /* Transfer of Sign */
  {ifunc_SIGN,  "SIGN",  "Util.sign",  "StrictUtil.sign",  "org/netlib/util/Util", "org/netlib/util/StrictUtil", "sign",  "(FF)F", IRD_ARGS,   Float},
  {ifunc_ISIGN, "ISIGN", "Util.isign", "StrictUtil.isign", "org/netlib/util/Util", "org/netlib/util/StrictUtil", "isign", "(II)I", INT_ARG,    Integer},
  {ifunc_DSIGN, "DSIGN", "Util.dsign", "StrictUtil.dsign", "org/netlib/util/Util", "org/netlib/util/StrictUtil", "dsign", "(DD)D", DOUBLE_ARG, Double},

  /* Positive Difference */
  {ifunc_DIM,  "DIM",  "Util.dim",  "StrictUtil.dim",  "org/netlib/util/Util", "org/netlib/util/StrictUtil", "dim",  "(FF)F", IRD_ARGS,   Float},
  {ifunc_IDIM, "IDIM", "Util.idim", "StrictUtil.idim", "org/netlib/util/Util", "org/netlib/util/StrictUtil", "idim", "(II)I", INT_ARG,    Integer},
  {ifunc_DDIM, "DDIM", "Util.ddim", "StrictUtil.ddim", "org/netlib/util/Util", "org/netlib/util/StrictUtil", "ddim", "(DD)D", DOUBLE_ARG, Double},

  /* Double Precision Product of two reals.  implement as (double)a1 * (double)a2  */
  {ifunc_DPROD, "DPROD", "Unused", NULL, "Unused", NULL, "Unused", "Unused", REAL_ARG, Double},

  /* Choosing Largest Value */
  {ifunc_MAX,   "MAX",   "Math.max", "StrictMath.max", "java/lang/Math", "java/lang/StrictMath", "max", "(DD)D", IRD_ARGS,   Double},
  {ifunc_MAX0,  "MAX0",  "Math.max", "StrictMath.max", "java/lang/Math", "java/lang/StrictMath", "max", "(II)I", INT_ARG,    Integer},
  {ifunc_AMAX1, "AMAX1", "Math.max", "StrictMath.max", "java/lang/Math", "java/lang/StrictMath", "max", "(FF)F", REAL_ARG,   Float},
  {ifunc_DMAX1, "DMAX1", "Math.max", "StrictMath.max", "java/lang/Math", "java/lang/StrictMath", "max", "(DD)D", DOUBLE_ARG, Double},
  {ifunc_AMAX0, "AMAX0", "Math.max", "StrictMath.max", "java/lang/Math", "java/lang/StrictMath", "max", "(FF)F", INT_ARG,    Float},
  {ifunc_MAX1,  "MAX1",  "Math.max", "StrictMath.max", "java/lang/Math", "java/lang/StrictMath", "max", "(FF)F", REAL_ARG,   Integer},

  /* Choosing Smallest Value */
  {ifunc_MIN,   "MIN",   "Math.min", "StrictMath.min", "java/lang/Math", "java/lang/StrictMath", "min", "(DD)D", IRD_ARGS,   Double},
  {ifunc_MIN0,  "MIN0",  "Math.min", "StrictMath.min", "java/lang/Math", "java/lang/StrictMath", "min", "(II)I", INT_ARG,    Integer},
  {ifunc_AMIN1, "AMIN1", "Math.min", "StrictMath.min", "java/lang/Math", "java/lang/StrictMath", "min", "(FF)F", REAL_ARG,   Float},
  {ifunc_DMIN1, "DMIN1", "Math.min", "StrictMath.min", "java/lang/Math", "java/lang/StrictMath", "min", "(DD)D", DOUBLE_ARG, Double},
  {ifunc_AMIN0, "AMIN0", "Math.min", "StrictMath.min", "java/lang/Math", "java/lang/StrictMath", "min", "(FF)F", INT_ARG,    Float},
  {ifunc_MIN1,  "MIN1",  "Math.min", "StrictMath.min", "java/lang/Math", "java/lang/StrictMath", "min", "(FF)F", REAL_ARG,   Integer},

  /* Length of Character Entity */
  {ifunc_LEN, "LEN", "Unused", NULL, "Unused", NULL, "Unused", "Unused", CS_ARGS, Integer},

  /* Location of Substring a2 in String a1 */
  {ifunc_INDEX, "INDEX", "(int)", NULL, "Unused", NULL, "Unused", "Unused", CS_ARGS, Integer},

  /* Imaginary Part of Complex Arg */
  {ifunc_AIMAG, "AIMAG", "(int)", NULL, "Unused", NULL, "Unused", "Unused", COMPLEX_ARG, Float},

  /* Conjuagate of Complex Argument */
  {ifunc_CONJG, "CONJG", "(int)", NULL, "Unused", NULL, "Unused", "Unused", COMPLEX_ARG, Complex},

  /* Sqare Root */
  {ifunc_SQRT,  "SQRT",   "Math.sqrt", "StrictMath.sqrt", "java/lang/Math", "java/lang/StrictMath", "sqrt", "(F)F", RDC_ARGS,    Double},
  {ifunc_DSQRT, "DSQRT",  "Math.sqrt", "StrictMath.sqrt", "java/lang/Math", "java/lang/StrictMath", "sqrt", "(D)D", DOUBLE_ARG,  Double},
  {ifunc_CSQRT, "CSQRT",  "Math.sqrt", "StrictMath.sqrt", "java/lang/Math", "java/lang/StrictMath", "sqrt", "(D)D", COMPLEX_ARG, Complex},

  /* Exponential */
  {ifunc_EXP,  "EXP",  "Math.exp", "StrictMath.exp", "java/lang/Math", "java/lang/StrictMath", "exp", "(D)D", RDC_ARGS,    Double},
  {ifunc_DEXP, "DEXP", "Math.exp", "StrictMath.exp", "java/lang/Math", "java/lang/StrictMath", "exp", "(D)D", DOUBLE_ARG,  Double},
  {ifunc_CEXP, "CEXP", "Math.exp", "StrictMath.exp", "java/lang/Math", "java/lang/StrictMath", "exp", "(D)D", COMPLEX_ARG, Complex},

  /* Natural Logarithm */
  {ifunc_LOG,  "LOG",  "Math.log", "StrictMath.log", "java/lang/Math", "java/lang/StrictMath", "log", "(D)D", RDC_ARGS,    Double},
  {ifunc_ALOG, "ALOG", "Math.log", "StrictMath.log", "java/lang/Math", "java/lang/StrictMath", "log", "(D)D", REAL_ARG,    Double},
  {ifunc_DLOG, "DLOG", "Math.log", "StrictMath.log", "java/lang/Math", "java/lang/StrictMath", "log", "(D)D", DOUBLE_ARG,  Double},
  {ifunc_CLOG, "CLOG", "Math.log", "StrictMath.log", "java/lang/Math", "java/lang/StrictMath", "log", "(D)D", COMPLEX_ARG, Complex},

  /* Common Logarithm - use java's log function then divide by 2.30258509 */
  {ifunc_LOG10,  "LOG10",  "Util.log10", "StrictUtil.log10", "org/netlib/util/Util", "org/netlib/util/StrictUtil", "log10", "(D)D", RD_ARGS,    Double},
  {ifunc_ALOG10, "ALOG10", "Util.log10", "StrictUtil.log10", "org/netlib/util/Util", "org/netlib/util/StrictUtil", "log10", "(D)D", REAL_ARG,   Double},
  {ifunc_DLOG10, "DLOG10", "Util.log10", "StrictUtil.log10", "org/netlib/util/Util", "org/netlib/util/StrictUtil", "log10", "(D)D", DOUBLE_ARG, Double},

  /* Sine */
  {ifunc_SIN,  "SIN",  "Math.sin", "StrictMath.sin", "java/lang/Math", "java/lang/StrictMath", "sin", "(D)D", RDC_ARGS,    Double},
  {ifunc_DSIN, "DSIN", "Math.sin", "StrictMath.sin", "java/lang/Math", "java/lang/StrictMath", "sin", "(D)D", DOUBLE_ARG,  Double},
  {ifunc_CSIN, "CSIN", "Math.sin", "StrictMath.sin", "java/lang/Math", "java/lang/StrictMath", "sin", "(D)D", COMPLEX_ARG, Complex},

  /* Cosine */
  {ifunc_COS,  "COS",  "Math.cos", "StrictMath.cos", "java/lang/Math", "java/lang/StrictMath", "cos", "(D)D", RDC_ARGS,    Double},
  {ifunc_DCOS, "DCOS", "Math.cos", "StrictMath.cos", "java/lang/Math", "java/lang/StrictMath", "cos", "(D)D", DOUBLE_ARG,  Double},
  {ifunc_CCOS, "CCOS", "Math.cos", "StrictMath.cos", "java/lang/Math", "java/lang/StrictMath", "cos", "(D)D", COMPLEX_ARG, Complex},

  /* Tangent */
  {ifunc_TAN,  "TAN",  "Math.tan", "StrictMath.tan", "java/lang/Math", "java/lang/StrictMath", "tan", "(D)D", RD_ARGS,    Double},
  {ifunc_DTAN, "DTAN", "Math.tan", "StrictMath.tan", "java/lang/Math", "java/lang/StrictMath", "tan", "(D)D", DOUBLE_ARG, Double},

  /* Arcsine */
  {ifunc_ASIN,  "ASIN",  "Math.asin", "StrictMath.asin", "java/lang/Math", "java/lang/StrictMath", "asin", "(D)D", RD_ARGS,    Double},
  {ifunc_DASIN, "DASIN", "Math.asin", "StrictMath.asin", "java/lang/Math", "java/lang/StrictMath", "asin", "(D)D", DOUBLE_ARG, Double},

  /* Arccosine */
  {ifunc_ACOS,  "ACOS",  "Math.acos", "StrictMath.acos", "java/lang/Math", "java/lang/StrictMath", "acos", "(D)D", RD_ARGS,    Double},
  {ifunc_DACOS, "DACOS", "Math.acos", "StrictMath.acos", "java/lang/Math", "java/lang/StrictMath", "acos", "(D)D", DOUBLE_ARG, Double},

  /* Arctangent */
  {ifunc_ATAN,   "ATAN",   "Math.atan",  "StrictMath.atan",  "java/lang/Math", "java/lang/StrictMath", "atan",  "(D)D",  RD_ARGS,    Double},
  {ifunc_DATAN,  "DATAN",  "Math.atan",  "StrictMath.atan",  "java/lang/Math", "java/lang/StrictMath", "atan",  "(D)D",  DOUBLE_ARG, Double},
  {ifunc_ATAN2,  "ATAN2",  "Math.atan2", "StrictMath.atan2", "java/lang/Math", "java/lang/StrictMath", "atan2", "(DD)D", RD_ARGS,    Double},
  {ifunc_DATAN2, "DATAN2", "Math.atan2", "StrictMath.atan2", "java/lang/Math", "java/lang/StrictMath", "atan2", "(DD)D", DOUBLE_ARG, Double},

  /* Hyperbolic Sine */
  {ifunc_SINH,  "SINH",  "Util.sinh", "StrictUtil.sinh", "org/netlib/util/Util", "org/netlib/util/StrictUtil", "sinh", "(D)D", RD_ARGS,    Double},
  {ifunc_DSINH, "DSINH", "Util.sinh", "StrictUtil.sinh", "org/netlib/util/Util", "org/netlib/util/StrictUtil", "sinh", "(D)D", DOUBLE_ARG, Double},

  /* Hyperbolic Cosine */
  {ifunc_COSH,  "COSH",  "Util.cosh", "StrictUtil.cosh", "org/netlib/util/Util", "org/netlib/util/StrictUtil", "cosh", "(D)D", RD_ARGS,    Double},
  {ifunc_DCOSH, "DCOSH", "Util.cosh", "StrictUtil.cosh", "org/netlib/util/Util", "org/netlib/util/StrictUtil", "cosh", "(D)D", DOUBLE_ARG, Double},

  /* Hyperbolic Tangent */
  {ifunc_TANH,  "TANH",  "Util.tanh", "StrictUtil.tanh", "org/netlib/util/Util", "org/netlib/util/StrictUtil", "tanh", "(D)D", RD_ARGS,    Double},
  {ifunc_DTANH, "DTANH", "Util.tanh", "StrictUtil.tanh", "org/netlib/util/Util", "org/netlib/util/StrictUtil", "tanh", "(D)D", DOUBLE_ARG, Double},

  /* Lexically Greater than or Equal to */
  {ifunc_LGE, "LGE", ".compareTo", NULL, "java/lang/String", NULL, "compareTo", "(Ljava/lang/String;)I", CS_ARGS, Logical},

  /* Lexically Greater than */
  {ifunc_LGT, "LGT", ".compareTo", NULL, "java/lang/String", NULL, "compareTo", "(Ljava/lang/String;)I", CS_ARGS, Logical},

  /* Lexically Less than or Equal to */
  {ifunc_LLE, "LLE", ".compareTo", NULL, "java/lang/String", NULL, "compareTo", "(Ljava/lang/String;)I", CS_ARGS, Logical},

  /* Lexically Less than */
  {ifunc_LLT, "LLT", ".compareTo", NULL, "java/lang/String", NULL, "compareTo", "(Ljava/lang/String;)I", CS_ARGS, Logical},

  /* fortran pseudo intrinsic */
  {ifunc_ETIME, "ETIME", ".etime", NULL, "org/netlib/util/Etime", NULL, "etime", "([FI)F", IRDC_ARGS, Float}, 

  {ifunc_SECOND, "SECOND", "System.currentTimeMillis",  NULL,  "java/lang/System", NULL, "currentTimeMillis", "()J", NO_ARG, Float}, 

  /*  Ends a scanning loop.  See comment above. */
  {0, NULL , NULL, NULL, NULL, NULL, NULL, NULL, 0, 0}    
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
  JVM_T_UNUSED, 
  JVM_T_UNUSED, 
  JVM_T_DOUBLE, 
  JVM_T_DOUBLE, 
  JVM_T_FLOAT, 
  JVM_T_INT, 
  JVM_T_BOOLEAN, 
  JVM_T_UNUSED
};

/* The jvm_data_types array maps from the f2j data types to the Java Virtual */
/* Machine data types.                                                       */

enum jvm_data_type jvm_data_types[MAX_RETURNS+1] = {
  jvm_Object,   /* String      */
  jvm_Object,   /* Character   */
  jvm_Object,   /* Complex     */
  jvm_Double,   /* Double      */
  jvm_Float,    /* Float       */
  jvm_Int,      /* Integer     */
  jvm_Byte,     /* Logical     */
  jvm_Object    /* Object      */
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
  jvm_aconst_null
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

/* initial values for above data types:  */
char *init_vals[MAX_RETURNS+1] =
{
  "\" \"",
  "\" \"",
  "0",
  "0.0d",
  "0.0f",
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
