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
  {"INT",    "(int)",             "Unused",           "Unused",           "Unused", Integer},
  {"IFIX",   "(int)",             "Unused",           "Unused",           "Unused", Integer},
  {"IDINT",  "(int)",             "Unused",           "Unused",           "Unused", Integer},
  {"REAL",   "(double)",          "Unused",           "Unused",           "Unused", Float},
  {"FLOAT",  "(double)",          "Unused",           "Unused",           "Unused", Float},
  {"SNGL",   "(double)",          "Unused",           "Unused",           "Unused", Float},
  {"DBLE",   "(double)",          "Unused",           "Unused",           "Unused", Double},
  {"CMPLX",  "(Complex)",         "Unused",           "Unused",           "Unused", Complex},
  {"ICHAR",  "(int)",             "Unused",           "Unused",           "Unused", Integer},
  {"CHAR",   "(char)",            "Unused",           "Unused",           "Unused", Character},

  /* Truncation */
  {"AINT",   "(int)",             "Unused",           "Unused",           "Unused", Double},
  {"DINT",   "(int)",             "Unused",           "Unused",           "Unused", Double},

  /* Nearest Whole Number */
  {"ANINT",  "(int)",             "Unused",           "Unused",           "Unused", Double},
  {"DNINT",  "(int)",             "Unused",           "Unused",           "Unused", Double},

  /* Nearest Integer */
  {"NINT",   "(int)",             "Unused",           "Unused",           "Unused", Integer},
  {"IDNINT", "(int)",             "Unused",           "Unused",           "Unused", Integer},

  /* Absolute Value */
  {"ABS",    "Math.abs",          "java/lang/Math",   "abs",              "(D)D", Double},
  {"IABS",   "Math.abs",          "java/lang/Math",   "abs",              "(D)D", Integer},
  {"DABS",   "Math.abs",          "java/lang/Math",   "abs",              "(D)D", Double},
  {"CABS",   "Math.abs",          "java/lang/Math",   "abs",              "(D)D", Float},

  /* Remaindering */
  {"MOD",    "(int) Math.IEEEremainder", "java/lang/Math", "IEEEremainder", "(DD)D", Integer},
  {"AMOD",   "(int) Math.IEEEremainder", "java/lang/Math", "IEEEremainder", "(DD)D", Float},
  {"DMOD",   "(int) Math.IEEEremainder", "java/lang/Math", "IEEEremainder", "(DD)D", Double},

  /* Transfer of Sign */
  {"SIGN",   "(int)",             "Unused",           "Unused",           "Unused", Double},
  {"ISIGN",  "(int)",             "Unused",           "Unused",           "Unused", Integer},
  {"DSIGN",  "(int)",             "Unused",           "Unused",           "Unused", Double},

  /* Positive Difference */
  {"DIM",    "????Math.min",          "java/lang/Math",   "min",              "(II)I", Double},
  {"IDIM",   "????Math.min",          "java/lang/Math",   "min",              "(II)I", Integer},
  {"DDIM",   "????Math.min",          "java/lang/Math",   "min",              "(II)I", Double},

  /* Double Precision Product */
  {"DPROD",  "????Math.min",          "java/lang/Math",   "min",              "(II)I", Double},

  /* Choosing Largest Value */
  {"MAX",    "Math.max",          "java/lang/Math",   "max",              "(II)I", Double},
  {"MAX0",   "Math.max",          "java/lang/Math",   "max",              "(II)I", Integer},
  {"AMAX1",  "Math.max",          "java/lang/Math",   "max",              "(DD)D", Float},
  {"DMAX1",  "Math.max",          "java/lang/Math",   "max",              "(DD)D", Double},
  {"AMAX0",  "Math.max",          "java/lang/Math",   "max",              "(DD)D", Float},
  {"MAX1",   "Math.max",          "java/lang/Math",   "max",              "(DD)D", Integer},

  /* Choosing Smallest Value */
  {"MIN",    "Math.min",          "java/lang/Math",   "min",              "(II)I", Double},
  {"MIN0",    "Math.min",          "java/lang/Math",   "min",              "(II)I", Integer},
  {"MIN1",    "Math.min",          "java/lang/Math",   "min",              "(II)I", Integer},
  {"AMIN0",    "Math.min",          "java/lang/Math",   "min",              "(II)I", Float},
  {"AMIN1",    "Math.min",          "java/lang/Math",   "min",              "(II)I", Float},
  {"DMIN1",    "Math.min",          "java/lang/Math",   "min",              "(II)I", Double},

  /* Length of Character Entity */
  {"LEN",    "(int)",             "Unused",           "Unused",           "Unused", Integer},

  /* Location of Substring a2 in String a1 */
  {"INDEX",    "(int)",             "Unused",           "Unused",           "Unused", Integer},

  /* Imaginary Part of Complex Arg */
  {"AIMAG",    "(int)",             "Unused",           "Unused",           "Unused", Float},

  /* Conjuagate of Complex Argument */
  {"CONJG",    "(int)",             "Unused",           "Unused",           "Unused", Complex},

  /* Sqare Root */
  {"SQRT",   "Math.sqrt",         "java/lang/Math",   "sqrt",             "(D)D", Double},
  {"DSQRT",  "Math.sqrt",         "java/lang/Math",   "sqrt",             "(D)D", Double},
  {"CSQRT",  "Math.sqrt",         "java/lang/Math",   "sqrt",             "(D)D", Complex},

  /* Exponential */
  {"EXP",    "Math.exp",          "java/lang/Math",   "exp",              "(D)D", Double},
  {"DEXP",    "Math.exp",          "java/lang/Math",   "exp",              "(D)D", Double},
  {"CEXP",    "Math.exp",          "java/lang/Math",   "exp",              "(D)D", Complex},

  /* Natural Logarithm */
  {"LOG",    "Math.log",          "java/lang/Math",   "log",              "(D)D", Double},
  {"ALOG",    "Math.log",          "java/lang/Math",   "log",              "(D)D", Float},
  {"DLOG",    "Math.log",          "java/lang/Math",   "log",              "(D)D", Double},
  {"CLOG",    "Math.log",          "java/lang/Math",   "log",              "(D)D", Complex},

  /* Common Logarithm - use java's log function then divie by 2.30258509 */
  {"LOG10",  "Math.log",          "java/lang/Math",   "log",              "(D)D", Double},
  {"ALOG10",  "Math.log",          "java/lang/Math",   "log",              "(D)D", Float},
  {"DLOG10",  "Math.log",          "java/lang/Math",   "log",              "(D)D", Double},

  /* Sine */
  {"SIN",    "Math.sin",          "java/lang/Math",   "sin",              "(D)D", Double},
  {"DSIN",    "Math.sin",          "java/lang/Math",   "sin",              "(D)D", Double},
  {"CSIN",    "Math.sin",          "java/lang/Math",   "sin",              "(D)D", Complex},

  /* Cosine */
  {"COS",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", Double},
  {"DCOS",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", Double},
  {"CCOS",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", Complex},

  /* Tangent */
  {"TAN",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", Double},
  {"DTAN",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", Double},

  /* Arcsine */
  {"ASIN",    "Math.sin",          "java/lang/Math",   "sin",              "(D)D", Double},
  {"DASIN",    "Math.sin",          "java/lang/Math",   "sin",              "(D)D", Double},

  /* Arccosine */
  {"ACOS",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", Double},
  {"DACOS",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", Double},

  /* Arctangent */
  {"ATAN",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", Double},
  {"DATAN",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", Double},
  {"ATAN2",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", Double},
  {"DATAN2",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", Double},

  /* Hyperbolic Sine */
  {"SINH",    "Math.sin",          "java/lang/Math",   "sin",              "(D)D", Double},
  {"DSINH",    "Math.sin",          "java/lang/Math",   "sin",              "(D)D", Double},

  /* Hyperbolic Cosine */
  {"COSH",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", Double},
  {"DCOSH",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", Double},

  /* Hyperbolic Tangent */
  {"TANH",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", Double},
  {"DTANH",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", Double},

  /* Lexically Greater than or Equal to */
  {"LGE",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", Logical},

  /* Lexically Greater than */
  {"LGT",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", Logical},

  /* Lexically Less than or Equal to */
  {"LLE",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", Logical},

  /* Lexically Less than */
  {"LLT",    "Math.cos",          "java/lang/Math",   "cos",              "(D)D", Logical},

  /* LAPACK 'intrinsics' */
  {"LSAME",  ".equalsIgnoreCase", "java/lang/String", "equalsIgnoreCase", "(Ljava/lang/String;)Z", Logical}, 
  {"LSAMEN", ".regionMatches",    "java/lang/String", "regionMatches",    "(ZILjava/lang/String;II)Z", Logical}, 

  /*  Ends a scanning loop.  See comment above. */
  {NULL , NULL, NULL, NULL, NULL, 0}    
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
  {"LSAME", 
      "invokevirtual java/lang/String/equalsIgnoreCase(Ljava/lang/String;)Z"}, 
  {"MAX", 
      "invokestatic java/lang/Math/max(II)I"},
  {"MIN", 
      "invokestatic java/lang/Math/min(II)I"},
  { NULL, 0}      /* Ends a scanning loop.  See comment above. */
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
