/*  initialize.h  */

#include<stdio.h>
#include<string.h>
#include"f2j.h"
#include"f2jparse.tab.h" 

#define Lsame "invokevirtual java/lang/String/equalsIgnoreCase("\
              "Ljava/lang/String;)Z"
#define Mathmax "invokestatic java/lang/Math/max(II)I" /* Note integer only here. */
#define Mathmin "invokestatic java/lang/Math/min(II)I" /* Note integer only here. */




/* The following several tables have their last entry initialized
   to `NULL'.  This allows each table to be traversed by a while()
   loop: 'while (tab->entry)' loops until entry is NULL, then
   gracefully exits.  Similarly, a for() loop can be used, for example:
   'for (tab;tab;tab++)' traverses tab until the NULL last entry is
   reached. See the 'keyscan()' and 'methodscan()' procedures. */

/* Statement starting keywords. The only violation of this
   in fortran 77 is the keyword THEN following a closing
   parentheses (')'). */
KWDTAB tab_stmt[] =
{
    {"CALL", CALL, 0},
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
    {"RETURN", RETURN, 0},
    {"REWIND", REWIND, 0},
    {"SAVE", SAVE, 0},
    {"STOP", STOP, 0},
    {"SUBROUTINE", SUBROUTINE, 0},
    {"THEN", THEN, 0},
    {"WRITE", WRITE, 0},
    NULL  /* Ends a scanning loop.  See comment above. */
};

/* The type tokens MUST appear at the beginning of a
   statement, and must occur before any of the
   executable statements. */
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

    {"CHARACTER", TYPE, Character},
    NULL  /* Ends a scanning loop.  See comment above. */
};

/* Miscellaneous tokens.  None of these tokens may
 appear at the beginning fo a statement.  */
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
    {".NEQV.", EQV, 1},
    {".EQ.", RELOP, rel_eq},
    {".NE.", RELOP, rel_ne},
    {".LT.", RELOP, rel_lt},
    {".LE.", RELOP, rel_le},
    {".GT.", RELOP, rel_gt},
    {".GE.", RELOP, rel_ge},
    {".TRUE.", TrUE, 1},
    {".FALSE.", FaLSE, 0},
    {"FMT", FMT, 0},
    NULL  /*  Ensures that the scanning loop ends if nothing is matched. */
};

/* This table lists stuff that can be
   handled with java methods.  The pattern
   is {"fortran name", "java method"}.
   Some of the fortran names are intrinsic
   to fortran and java, others are intrinsic
   only to java and replace function or sub-
   routine calls in the lapack or blas source.
   This table may have to be extended to handle
   jasmin opcodes.  */

METHODTAB intrinsic_toks[]=
{
  {"LSAME", ".equalsIgnoreCase"}, 
  {"LSAMEN", ".regionMatches"}, 
  {"MAX", "Math.max"},
  {"MIN", "Math.min"},
  {"ABS", "Math.abs"},
  {"DABS", "Math.abs"},
  {"SQRT", "Math.sqrt"},
  {"DSQRT", "Math.sqrt"},
  {"MOD", "(int) Math.IEEEremainder"},
  {"DBLE", "(double)"},
  {NULL , NULL}    /*  Ends a scanning loop.  See comment above. */
};

METHODTAB java_toks[]=
{
  {"MAX", "Math.max"},
  {"MIN", "Math.min"},
  {"ABS", "Math.abs"},
  {"DABS", "Math.abs"},
  {"SQRT", "Math.sqrt"},
  {"MOD", "Math.IEEEremainder"},
  { NULL , NULL}    /*  Ends a scanning loop.  See comment above. */
};

/* There are some long strings associated with these, so 
   I am using the preprocessor to clean this code a bit.
   The strings are defined at the top of this file, but 
   probably need to be in a lex header file.  */

METHODTAB jasmin_intrinsic_toks[]=
{
  {"LSAME", Lsame}, 
  {"MAX", Mathmax},
  {"MIN", Mathmin},
  NULL      /* Ends a scanning loop.  See comment above. */
};

/* Table to indicate which primitives have to be passed by
   reference instead of value.  */
METHODTAB pass_by_refs[]=
{
  {"WORK", "work.val"},
  {"INFO", "info.val"},
  NULL      /* Ends a scanning loop.  See comment above. */
};

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
