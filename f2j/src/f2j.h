/*
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */

#ifndef _F2J_H
#define _F2J_H

/*****************************************************************************
 * f2j.h                                                                     *
 *                                                                           *
 * Header file for the Fortran-to-Java translator.                           *
 *                                                                           *
 *****************************************************************************/

#include<assert.h>
#include<stdlib.h>
#include"symtab.h"
#include"dlist.h"
#include"class.h"

typedef int BOOLEAN;

#define FALSE 0
#define TRUE  1

#define MIN(x,y) ((x)<(y)?(x):(y))       /* the minimum of two numbers       */

/*****************************************************************************
 * Define VCG as 1 if VCG output is desired (VCG == Visualization of         *
 *   Compiler Graphs)                                                        *
 *                                                                           *
 * Define DEFAULT_TARGET_LANG as 0 to generate Java code by default.         *
 * Define DEFAULT_TARGET_LANG as 1 to generate Jasmin code by default.       *
 * DEFAULT_TARGET_LANG can be overridden by the command-line options.        *
 *****************************************************************************/

#define VCG 0
#define DEFAULT_TARGET_LANG 0

/*****************************************************************************
 * Defines for optimization of the use of object wrappers:                   *
 *   NOT_VISITED - f2j has not started optimizing this routine               *
 *   VISITED     - f2j has started optimizing, but has not finished          *
 *   FINISHED    - optimization is complete for this routine                 *
 *****************************************************************************/

#define NOT_VISITED 0
#define VISITED     1
#define FINISHED    2

/*****************************************************************************
 *  If DEBUGGEM is defined as 1, yyparse produces voluminous, detailed       *
 *  output to stderr during parsing.                                         *
 *****************************************************************************/

#define DEBUGGEM 0

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
  *vcgfp;                  /* VCG output file pointer                        */

char 
  *inputfilename,          /* name of the input file                         */
  *package_name;           /* what to name the package, e.g. org.netlib.blas */

BOOLEAN 
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
  *global_common_table;    /* Global COMMON table                            */

Dlist constants_table;     /* constants (for bytecode constant pool gen.)    */

/* Enumeration of the different kinds of Specification statements */

enum spectype
{
  External, Intrinsic, Implicit, Parameter
};

/* Enumeration of the different return types */

enum returntype
{
  String, Character, Complex, Double, Float, Integer, Logical
};

/* Represents whether an expression is on the lhs or rhs. */

enum _expr_side
{
  left, right
};

/* Enumeration of all the different kinds of nodes in the AST */

enum _nodetype
{
  Source = 1,
  Progunit,
  Subroutine,
  Function,
  Program,
  Blockif,
  Comment,
  MainComment,
  Common,
  CommonList,
  DataStmt,
  DataList,
  Elseif,
  Else,
  Forloop,
  Format,
  Constant,
  Method,
  Identifier,
  Label,
  Logicalif,
  Arithmeticif,
  Typedec,
  Assignment,
  Expression,
  Equivalence,
  Return,
  Goto,
  Call,
  Statement,
  Relationalop,
  Logicalop,
  Binaryop,
  Power,
  Unaryop,
  Save,
  Specification,
  Substring,
  End,
  Write,
  Read,
  Stop,
  ComputedGoto,
  ArrayAccess,
  ArrayDec,
  ArrayIdxRange,
  EmptyArgList,
  IoExplist,
  DataImpliedLoop,
  IoImpliedLoop,
  Unimplemented
};

/*****************************************************************************
 * Structure for program units (program, function, subroutine).              *
 *****************************************************************************/

struct _source
{
  enum returntype returns;          /* The return type of this program unit  */

  struct ast_node 
    *name,                          /* node representing this unit's name    */
    *progtype,                      /* type of unit (e.g. PROGRAM, FUNCTION) */
    *typedecs,                      /* type declarations                     */
    *statements,                    /* executable statements                 */
    *args,                          /* argument list                         */
    *equivalences,                  /* list of equivalences                  */
    *prologComments,                /* comments preceding unit header        */
    *javadocComments;               /* comm. to be emitted in javadoc format */

  SYMTABLE 
    *type_table,                    /* general symbol table for this unit    */
    *external_table,                /* external funcs called from this unit  */
    *intrinsic_table,               /* intrinsic funcs called from this unit */
    *args_table,                    /* table of this unit's arguments        */
    *array_table,                   /* variables that are declared as arrays */
    *format_table,                  /* FORMAT statements                     */
    *data_table,                    /* variables declared in a DATA stmt     */
    *save_table,                    /* variables declared in a SAVE stmt     */
    *common_table,                  /* variables declared in a COMMON stmt   */
    *parameter_table,               /* variables declared as PARAMETERS      */
    *equivalence_table;             /* variables that are equivalenced       */

  Dlist constants_table;            /* constant_pool info for bytecode gen.  */

  BOOLEAN 
    needs_input,                    /* does this unit read any data          */
    needs_reflection,               /* does this unit call a passed-in func  */
    needs_blas;                     /* does this unit call any BLAS routines */
 
  int scalarOptStatus;              /* status of optimization on this unit   */

  struct ClassFile
    *class;                         /* class file for this program unit      */
};

/*****************************************************************************
 * Structure for expressions and assignment statements.                      *
 *****************************************************************************/

struct _assignment
{
  BOOLEAN parens;                   /* used only by expr nodes.  TRUE if the */
                                    /* expression is enclosed by parens      */

  int label;                        /* label for this expr (used w/Jasmin)   */

  char  
    minus,                          /* unary sign of this expression         */
    optype,                         /* kind of operation (e.g. +, -, *, etc) */
    *opcode;                        /* Jasmin opcode for this operation      */

  struct ast_node 
    *lhs,                           /* left-hand side of expr or assignment  */
    *rhs;                           /* right-hand side of expr or assignment */
};

/*****************************************************************************
 * This structure represents variable declarations.                          *
 *****************************************************************************/

struct _typeunit
{
  enum spectype specification;      /* what kind of declaration this is      */

  enum returntype returns;          /* the data type of this declaration     */

  struct ast_node *declist;         /* list of variables being declared      */
};

/*****************************************************************************
 * This structure represents DO loops.                                       *
 *****************************************************************************/

struct _forloop
{
  int 
    startlabel,                     /* label of beginning of loop (Jasmin)   */
    stoplabel;                      /* label of end of loop (Jasmin)         */

  struct ast_node 
    *counter,                       /* the loop variable                     */
    *Label,                         /* label of the CONTINUE for this loop   */
    *start,                         /* initial loop assignment (e.g. i = 0)  */
    *stop,                          /* stop when counter equals stop         */
    *incr,                          /* amount to increment each iteration    */
    *iter_expr,                     /* expression to calc # of iterations    */
    *incr_expr;                     /* expression to calc increment          */
};

/*****************************************************************************
 * This structure represents constants.                                      *
 *****************************************************************************/

struct _constant
{
  int 
    cp_index;                       /* constant pool index of this constant  */

  char 
    *opcode,                        /* e.g., iconst_1, bipush 121.23         */
    number[80];                     /* the constant                          */
};

/*****************************************************************************
 * This structure represents labels.                                         *
 *****************************************************************************/

struct _label
{
  int number;                       /* the label number                      */
  struct ast_node *stmt;            /* the statement after this label        */
};

/*****************************************************************************
 * This structure represents identifiers.  An identifier can be a scalar     *
 * variable, array variable, function name, or subroutine name.              *
 *****************************************************************************/

struct _ident
{
  int 
    dim,                            /* number of dimensions (for arrays)     */
    D[3],                           /* num elements in each dim (up to 3)    */
    len,                            /* size of ident (e.g. CHARACTER*8 = 8)  */
    position,                       /* ident's position in COMMON block      */
    localvnum;                      /* local variable number (for Jasmin)    */ 

  BOOLEAN
    passByRef,                      /* is this ident pass by reference       */ 
    needs_declaration;              /* does this ident need a declaration    */

  struct ast_node 
    *arraylist,                     /* expression representing array size    */
    *lead_expr;                     /* leading dimension expression          */

  char 
    *leaddim,                       /* leading dimension variable or const   */
    *opcode,	                    /* A string records the appropriate      * 
                                     * method to invoke on the stack when    * 
                                     * opcode is emitted.                    * 
                                     * e.g., opcode = strdup("iload_1");     */
    *commonBlockName,               /* name of COMMON block this ident is in */
    name[80],                       /* this ident's name                     */
    *merged_name,                   /* this ident's merged name (e.g. in     *
                                     * cases of equivalence or COMMON)       */
    *descriptor;                    /* constant pool descriptor of the ident */
};

/*****************************************************************************
 * This structure represents Logical IF statements and Block IF statements.  *
 * A logical if is a one-line IF statement with no ELSE or ELSE IF.          *
 * For example,                                                              * 
 *   IF(a.eq.b) x=12                                                         *
 *                                                                           * 
 * A Block if is an IF-THEN statement with optional ELSE and ELSE IF         *
 * blocks.  For example,                                                     *
 *   IF(a.eq.b) THEN                                                         *
 *     x=12                                                                  *
 *   ELSE                                                                    *
 *     x=0                                                                   *
 *   END IF                                                                  *
 *****************************************************************************/

struct _logicalif
{
  int 
    skip_label,                     /* go to this label if expr is FALSE     */
    fall_label,                     /* fall through label (expr is TRUE)     */
    break_label;                    /* for block if, the label after all IF, *
                                     * ELSEIF, and ELSE blocks.              */

  struct ast_node 
    *conds,                         /* the conditional expression to test    */
    *stmts,                         /* statements to execute if expr is TRUE */
    *elseifstmts,                   /* list of ELSE IF statements            */
    *elsestmts;                     /* stmts to exectue if no IF or ELSE IF  *
                                     * expression was TRUE                   */
};

/*****************************************************************************
 * This structure represents the Arithmetic IF.  The arithmetic IF consists  *
 * of an expression and three labels.  If the expression evaluates to a      *
 * negative value, control goes to the statement corresponding to the first  *
 * label.  If the expression is 0, jump to the second label.  If the         *
 * expression is positive, jump to the third label.                          *
 *****************************************************************************/

struct _arithmeticif
{
  struct ast_node *cond;            /* the conditional expression            */

  int 
    neg_label,                      /* branch to this label if expr < 0      */
    zero_label,                     /* branch to this label if expr == 0     */
    pos_label;                      /* branch to this label if expr > 0      */
};

/*****************************************************************************
 * This structure represents the GOTO statement.                             *
 *****************************************************************************/

struct _goto
{
  int label;                        /*  which label to branch to             */
};

/*****************************************************************************
 * This structure represents IO statements (READ and WRITE).                 *
 *****************************************************************************/

struct _io
{
  int 
    io_type,                        /* is this a READ or WRITE statement     */
    file_desc,                      /* file descriptor (not currently used)  */
    format_num,                     /* FORMAT desc for this statement        */
    end_num;                        /* where to branch on error              */

  struct ast_node 
    *fmt_list,                      /* inline FORMAT info (w/ WRITE)         */
    *arg_list;                      /* list of expressions to read or write  */
};

/*****************************************************************************
 * This structure represents DATA statements.                                *
 *****************************************************************************/

struct _data_stmt
{
  struct ast_node 
    *nlist,                         /* list of variable initializations      */
    *clist;                         /* list of values to initialize with     */
};

/*****************************************************************************
 * This structure represents COMMON blocks.                                  *
 *****************************************************************************/

struct _commonblock
{
  char *name;                       /* the name of the common block          */
  struct ast_node *nlist;           /* list of variables in this block       */
};

/*****************************************************************************
 * This structure represents the computed GOTO.  The computed GOTO consists  *
 * of a list of labels followed by an expression.  The expression is         *
 * evaluated and control flows to the Nth label in the list, where N is the  *
 * integer value of the expression.  For example,                            *
 *   X = 3                                                                   *
 *   GOTO (10, 20, 30, 40) X                                                 *
 *****************************************************************************/

struct _computed_goto
{
  struct ast_node 
    *name,                          /* expr that determines where to branch  */
    *intlist;                       /* list of labels (targets)              */
};

/*****************************************************************************
 *  The main data structure, a "tagged union". This represents a node        *
 * of the AST.                                                               *
 *****************************************************************************/

typedef struct ast_node
{
  int token;                        /* this node's token (from lexer)        */
  enum returntype vartype;          /* data type of this node                */
      
  struct ast_node 
    *nextstmt,                      /* statement or item following this one  */
    *prevstmt,                      /* statement or item preceding this one  */
    *parent;                        /* parent of this node                   */

  enum _expr_side expr_side;        /* which side this node is on            */

  enum _nodetype nodetype;          /* what kind of node this is             */

  /* 
   * For any given node, one of the following structures should apply,
   * depending on the node type.
   */

  union
  {
    struct _goto           go_to;             /* goto is a reserved word!    */
    struct _io             io_stmt;
    struct _label          label;
    struct _ident          ident;
    struct _source         source;
    struct _forloop        forloop;
    struct _typeunit       typeunit;
    struct _constant       constant;
    struct _commonblock    common;
    struct _data_stmt      data, equiv;
    struct _arithmeticif   arithmeticif;
    struct _computed_goto  computed_goto;
    struct _logicalif      logicalif, blockif;
    struct _assignment     assignment, expression;
  }
  astnode;
}
AST;


/*****************************************************************************
 * keyword lookup table.                                                     *
 *****************************************************************************/

typedef struct _kwdtab
{
  char *kwd;                        /* text of the keyword                   */
  int ktok;                         /* token code                            */
  int klex;                         /* lexical value                         */
}
KWDTAB;

/*****************************************************************************
 * Java intrinsic methods.                                                   *
 *****************************************************************************/

typedef struct method_tab
{
  char *fortran_name;               /* name of the Fortran intrinsic         */

  /* used for Java source generation: */
  char *java_method;                /* name of the corresponding Java func   */

  /* used for bytecode generation: */
  char *class_name;                 /* fully qualified Java class name       */
  char *method_name;                /* fully qualified Java class name       */
  char *descriptor;                 /* corresponding Java func descriptor    */
}
METHODTAB;

/*****************************************************************************
 * Enumeration of the relational operators.                                  *
 *****************************************************************************/

enum relops
{
  rel_eq = 1,                       /* equals                                */
  rel_ne,                           /* not equal                             */
  rel_lt,                           /* less than                             */
  rel_le,                           /* less than or equal                    */
  rel_gt,                           /* greater than                          */
  rel_ge                            /* greater than or equal                 */
};

/*****************************************************************************
 * This structure represents a 'substitution'.  This associates an integer   *
 * value with a variable name.                                               *
 *****************************************************************************/

typedef struct {
  char *name;                       /* variable name                         */
  int val;                          /* value                                 */
} SUBSTITUTION;


/*****************************************************************************
 * Function prototypes to keep the compiler from complaining.                *
 *****************************************************************************/

void 
  jasminheader (FILE *, char *),
  javaheader (FILE *, char *),
  return_emit (),
  logicalop_assign(AST *),
  relationalop_assign(AST *),
  logicalop_emit(AST *),
  elseif_emit(AST *),
  elseif_assign(AST *),
  else_emit(AST *),
  else_assign(AST *),
  store_array_var(AST *),
  initialize(),
  uppercase(char *),
  while_emit(AST *),
  alloc_error(size_t),
  * f2jalloc(size_t),
  * f2jcalloc(size_t, size_t),
  * f2jrealloc(void *, size_t);

AST *format_item_emit(AST *, AST**);

#endif
