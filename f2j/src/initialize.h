/*
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */

#ifndef _INITIALIZE_H
#define _INITIALIZE_H

/*****************************************************************************
 * initialize.h                                                              *
 *                                                                           *
 * Header file containing initialization of f2java's translation tables.     *
 * The following several tables have their last entry initialized            *
 * to `NULL'.  This allows each table to be traversed by a while()           *
 * loop: 'while (tab->entry)' loops until entry is NULL, then                *
 * gracefully exits.  Similarly, a for() loop can be used, for example:      *
 * 'for (tab;tab;tab++)' traverses tab until the NULL last entry is          *
 * reached. See the 'keyscan()' and 'methodscan()' procedures.               *
 *                                                                           *
 *****************************************************************************/


#include<stdio.h>
#include<string.h>
#include"f2j.h"
#include"f2jparse.tab.h" 

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
  {ifunc_LEN, "LEN",    "(int)",             "Unused",           "Unused",           "Unused", CS_ARGS, Integer},

  /* Location of Substring a2 in String a1 */
  {ifunc_INDEX, "INDEX",    "(int)",             "Unused",           "Unused",           "Unused", CS_ARGS, Integer},

  /* Imaginary Part of Complex Arg */
  {ifunc_AIMAG, "AIMAG",    "(int)",             "Unused",           "Unused",           "Unused", COMPLEX_ARG, Float},

  /* Conjuagate of Complex Argument */
  {ifunc_CONJG, "CONJG",    "(int)",             "Unused",           "Unused",           "Unused", COMPLEX_ARG, Complex},

  /* Sqare Root */
  {ifunc_SQRT, "SQRT",   "Math.sqrt",         "java/lang/Math",   "sqrt",             "(D)D", RDC_ARGS, Double},
  {ifunc_DSQRT, "DSQRT",  "Math.sqrt",         "java/lang/Math",   "sqrt",             "(D)D", DOUBLE_ARG, Double},
  {ifunc_CSQRT, "CSQRT",  "Math.sqrt",         "java/lang/Math",   "sqrt",             "(D)D", COMPLEX_ARG, Complex},

  /* Exponential */
  {ifunc_EXP, "EXP",    "Math.exp",          "java/lang/Math",   "exp",              "(D)D", RDC_ARGS, Double},
  {ifunc_DEXP, "DEXP",    "Math.exp",          "java/lang/Math",   "exp",              "(D)D", DOUBLE_ARG, Double},
  {ifunc_CEXP, "CEXP",    "Math.exp",          "java/lang/Math",   "exp",              "(D)D", COMPLEX_ARG, Complex},

  /* Natural Logarithm */
  {ifunc_LOG, "LOG",    "Math.log",          "java/lang/Math",   "log",              "(D)D", RDC_ARGS, Double},
  {ifunc_ALOG, "ALOG",    "Math.log",          "java/lang/Math",   "log",              "(D)D", REAL_ARG, Float},
  {ifunc_DLOG, "DLOG",    "Math.log",          "java/lang/Math",   "log",              "(D)D", DOUBLE_ARG, Double},
  {ifunc_CLOG, "CLOG",    "Math.log",          "java/lang/Math",   "log",              "(D)D", COMPLEX_ARG, Complex},

  /* Common Logarithm - use java's log function then divie by 2.30258509 */
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
  {ifunc_TAN, "TAN",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", RD_ARGS, Double},
  {ifunc_DTAN, "DTAN",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", DOUBLE_ARG, Double},

  /* Arcsine */
  {ifunc_ASIN, "ASIN",    "Math.sin",          "java/lang/Math",   "sin",              "(D)D", RD_ARGS, Double},
  {ifunc_DASIN, "DASIN",    "Math.sin",          "java/lang/Math",   "sin",              "(D)D", DOUBLE_ARG, Double},

  /* Arccosine */
  {ifunc_ACOS, "ACOS",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", RD_ARGS, Double},
  {ifunc_DACOS, "DACOS",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", DOUBLE_ARG, Double},

  /* Arctangent */
  {ifunc_ATAN, "ATAN",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", RD_ARGS, Double},
  {ifunc_DATAN, "DATAN",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", DOUBLE_ARG, Double},
  {ifunc_ATAN2, "ATAN2",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", RD_ARGS, Double},
  {ifunc_DATAN2, "DATAN2",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", DOUBLE_ARG, Double},

  /* Hyperbolic Sine */
  {ifunc_SINH, "SINH",    "Math.sin",          "java/lang/Math",   "sin",              "(D)D", RD_ARGS, Double},
  {ifunc_DSINH, "DSINH",    "Math.sin",          "java/lang/Math",   "sin",              "(D)D", DOUBLE_ARG, Double},

  /* Hyperbolic Cosine */
  {ifunc_COSH, "COSH",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", RD_ARGS, Double},
  {ifunc_DCOSH, "DCOSH",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", DOUBLE_ARG, Double},

  /* Hyperbolic Tangent */
  {ifunc_TANH, "TANH",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", RD_ARGS, Double},
  {ifunc_DTANH, "DTANH",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", DOUBLE_ARG, Double},

  /* Lexically Greater than or Equal to */
  {ifunc_LGE, "LGE",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", CS_ARGS, Logical},

  /* Lexically Greater than */
  {ifunc_LGT, "LGT",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", CS_ARGS, Logical},

  /* Lexically Less than or Equal to */
  {ifunc_LLE, "LLE",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", CS_ARGS, Logical},

  /* Lexically Less than */
  {ifunc_LLT, "LLT",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", CS_ARGS, Logical},

  /* LAPACK 'intrinsics' */
  {ifunc_LSAME, "LSAME",  ".equalsIgnoreCase", "java/lang/String", "equalsIgnoreCase", "(Ljava/lang/String;)Z", CS_ARGS, Logical}, 
  {ifunc_LSAMEN, "LSAMEN", ".regionMatches",    "java/lang/String", "regionMatches",    "(ZILjava/lang/String;II)Z", CS_ARGS, Logical}, 

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
  {ifunc_LSAME, "LSAME", 
      "invokevirtual java/lang/String/equalsIgnoreCase(Ljava/lang/String;)Z"}, 
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

#endif
