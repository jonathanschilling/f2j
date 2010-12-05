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
#include"bytecode.h"
#include"opcodes.h"

#define FALSE 0
#define TRUE  1

#define F2J_CLASS_ACC (JVM_ACC_PUBLIC | JVM_ACC_FINAL | JVM_ACC_SUPER)
#define F2J_NORMAL_ACC (JVM_ACC_PUBLIC | JVM_ACC_STATIC)
#define F2J_STRICT_ACC (JVM_ACC_STRICT | F2J_NORMAL_ACC)
#define F2J_ADAPTER_ACC (JVM_ACC_PRIVATE | JVM_ACC_STATIC)
#define F2J_INIT_ACC (JVM_ACC_PUBLIC)

#define MAX(a,b) (((a)>(b))?(a):(b))    /* the maximum of two numbers       */
#define MIN(x,y) (((x)<(y))?(x):(y))    /* the minimum of two numbers       */

/*****************************************************************************
 * Define VCG as 1 if VCG output is desired (VCG == Visualization of         *
 *   Compiler Graphs)                                                        *
 *****************************************************************************/

#define VCG 0

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
* Definitions for intrinsic variable names. At certain pts in the parser, we *
* do not know whether this intrinsic name represents an intrinsic call,      *
* function call, array name, or a regular variable.                          *
*****************************************************************************/

#define INTRIN_NOT_NAMED 0
#define INTRIN_NAMED_VARIABLE 1
#define INTRIN_NAMED_ARRAY 2
#define INTRIN_NAMED_ARRAY_OR_FUNC_CALL 3

/*****************************************************************************
* Various definitions related to opening files.                              *
*****************************************************************************/

#define FILE_STATUS_OLD 0
#define FILE_STATUS_NEW 1
#define FILE_STATUS_SCRATCH 2
#define FILE_STATUS_UNKNOWN 3

#define FILE_ACCESS_SEQ 0
#define FILE_ACCESS_DIRECT 1

#define FILE_FORMATTED 0
#define FILE_UNFORMATTED 1

#define FILE_BLANK_NULL 0
#define FILE_BLANK_ZERO 1

/*****************************************************************************
 * Definitions for an expandable string structure.  STR_INIT is the initial  *
 * size of the string, while STR_CHUNK is the number of bytes by which we    *
 * increment the string when it is too small.                                *
 *****************************************************************************/

#define STR_INIT  50
#define STR_CHUNK 20

#define MAX_CONST_LEN 256

/*****************************************************************************
 * BIGBUFF is the maximum size in characters of an input line (including)    *
 * continuations.  Had a segfault on a very long continued line              *
 * in a lapack routine.  This is a hack, should                              *
 * reallaoc when buffer overflows instead.                                   *
 *                                                                           *
 * YYTEXTLEN is the maximum size in characters of the token string.          *
 *                                                                           *
 * COMMENT_BUFLEN is the number of lines of comments to buffer when we are   *
 * in the middle of combining continued lines.                               *
 *                                                                           *
 * NT_NUM is the size of a small buffer for holding saved tokens that we     *
 * want to pass on subsequent calls to the lexer.                            *
 *****************************************************************************/

#define BIGBUFF    2000
#define YYTEXTLEN  2000
#define COMMENT_BUFLEN 8192
#define NT_NUM        2

struct _str {
  unsigned int size;
  char *val;
};

/*****************************************************************************
 * this structure holds information about an array access, including the     *
 * full name of the array, local variable number, etc.                       *
 *****************************************************************************/

struct var_info {
  char *name;        /* name of variable incl common prefix if appropriate   */
  char *desc;        /* field descriptor of variable                         */
  char *class;       /* class name of variable                               */
  int localvar;      /* local variable num of this variable, if appropriate  */
  BOOL is_arg;       /* is this variable an arg to the current prog unit?    */
};

/*****************************************************************************
 * This struct retains information about included files that are on the      *
 * stack (so we can keep track of which line number we were on when we       *
 * started the included file.                                                *
 *****************************************************************************/

typedef struct _include_file_info
{
  char *name;
  int line_num;
  FILE *fp;
}
INCLUDED_FILE;

/*****************************************************************************
 * This struct defines an entry in the implicit table, which holds info      *
 * about any IMPLICIT statements and the mapping between first letter and    *
 * data type.                                                                *
 *****************************************************************************/

typedef struct _itab_entry {
  enum returntype type;
  int len;
  int declared;
} ITAB_ENTRY;

/*****************************************************************************
 * F2J_PATH_VAR defines the environment variable used to specify the search  *
 * path for .f2j method/descriptor files.                                    *
 *****************************************************************************/

#define F2J_PATH_VAR "F2J_SEARCH_PATH"

/*****************************************************************************
 * bitfields representing the valid arguments to intrinsics.   the generic   *
 * intrinsics may take many different valid types, so we OR them together in *
 * some cases.                                                               *
 *****************************************************************************/

#define STRING_ARG   64
#define CHAR_ARG     32
#define COMPLEX_ARG  16
#define DOUBLE_ARG    8
#define REAL_ARG      4
#define INT_ARG       2
#define LOGICAL_ARG   1
#define NO_ARG        0

#define IRDC_ARGS (INT_ARG | REAL_ARG | DOUBLE_ARG | COMPLEX_ARG)
#define IRD_ARGS (INT_ARG | REAL_ARG | DOUBLE_ARG)
#define IR_ARGS (INT_ARG | REAL_ARG)
#define RD_ARGS (REAL_ARG | DOUBLE_ARG)
#define RDC_ARGS (REAL_ARG | DOUBLE_ARG | COMPLEX_ARG)
#define CS_ARGS (STRING_ARG | CHAR_ARG)

/*****************************************************************************
 * MAX_ARRAY_DIM is the maximum number of dimensions allowed in an array.    *
 *****************************************************************************/

#define MAX_ARRAY_DIM 7

/*****************************************************************************
 * MAIN_DESCRIPTOR is the descriptor required for a main() method in Java.   *
 *****************************************************************************/

#define MAIN_DESCRIPTOR "([Ljava/lang/String;)V"

/*****************************************************************************
 *  If DEBUGGEM is defined as 1, yyparse produces voluminous, detailed       *
 *  output to stderr during parsing.                                         *
 *****************************************************************************/

#define DEBUGGEM 0

/* Enumeration of the different kinds of Specification statements */

enum spectype
{
  External, Intrinsic, Implicit, Parameter
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
  Dimension,
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
  Pause,
  ComputedGoto,
  AssignedGoto,
  ArrayAccess,
  ArrayDec,
  ArrayIdxRange,
  EmptyArgList,
  IoExplist,
  DataImpliedLoop,
  IoImpliedLoop,
  StmtLabelAssign,
  Unimplemented,
  UnitExp,
  FormatOrUnknownSpec,
  OpenFileSpec,
  ReclExp,
  RecExp,
  StatusExp,
  AccessExp,
  FormExp,
  BlankExp,
  ErrExp,
  EndExp,
  IostatExp,
  Open,
  Close,
  Rewind,
  Backspace,
  Endfile,
  CharExp
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

  Dlist
    stmt_assign_list,               /* labels used in ASSIGN TO statements   */
    constants_table;                /* constant_pool info for bytecode gen.  */

  BOOL 
    explicit_decl,                  /* was function type explicitly decl'd   */
    needs_input,                    /* does this unit read any data          */
    needs_output,                   /* does this unit write any data         */
    needs_files,                    /* does this unit open any files         */
    needs_reflection,               /* does this unit call a passed-in func  */
    needs_iostat;                   /* does this unit need a temp iostat var */
 
  int
    scalarOptStatus,                /* status of optimization on this unit   */
    save_all;                       /* is there a SAVE stmt without var list */

  JVM_CLASS
    *class;                         /* class file for this program unit      */

  char * descriptor;                /* method descriptor for this prog unit  */
};

/*****************************************************************************
 * Structure for expressions and assignment statements.                      *
 *****************************************************************************/

struct _assignment
{
  BOOL parens;                      /* used only by expr nodes.  TRUE if the */
                                    /* expression is enclosed by parens      */

  char  
    minus,                          /* unary sign of this expression         */
    optype;                         /* kind of operation (e.g. +, -, *, etc) */

  struct ast_node 
    *lhs,                           /* left-hand side of expr or assignment  */
    *rhs;                           /* right-hand side of expr or assignment */
};

/*****************************************************************************
 * Structure for the OPEN statment.                                          *
 *****************************************************************************/

struct _open
{
  struct ast_node
    *unit_expr,                     /* expression for the unit number        */
    *file_expr,                     /* expr for name of the file to open     */
    *iostat,                        /* identifier for i/o status specifier   */
    *status,                        /* file status: old/new/scratch/unknown  */
    *access,                        /* file access mode: sequential/direct   */
    *form,                          /* file style: formatted/unformatted     */
    *blank,                         /* blank mode: null/zero                 */
    *recl;                          /* expression for record length          */

  int
    err;                            /* statement number for error specifier  */
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
  unsigned int 
    localvar;                       /* local var holding iteration count     */

  struct ast_node 
    *counter,                       /* the loop variable                     */
    *Label,                         /* label of the CONTINUE for this loop   */
    *start,                         /* initial loop assignment (e.g. i = 0)  */
    *stop,                          /* stop when counter equals stop         */
    *incr,                          /* amount to increment each iteration    */
    *iter_expr,                     /* expression to calc # of iterations    */
    *incr_expr;                     /* expression to calc increment          */

  JVM_CODE_GRAPH_NODE
    *goto_node;                     /* graph node of initial loop goto op    */
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
    *number;                        /* the constant                          */
};

/*****************************************************************************
 * This structure represents labels.                                         *
 *****************************************************************************/

struct _label
{
  int 
    number;                         /* the label number                      */

  JVM_CODE_GRAPH_NODE
    *instr;                         /* bytecode instruction with this label  */

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
    position,                       /* ident's position in COMMON block      */
    len,                            /* size of ident (e.g. CHARACTER*8 = 8)  */
    array_len,                      /* num elements in array (if not implied)*/
    localvnum,                      /* local variable number (for bytecode)  */
    which_implicit;                 /* default 0, array 1, var 2, lfunc 3, intrin 4 */ 

  BOOL
    passByRef,                      /* is this ident pass by reference       */ 
    needs_declaration,              /* does this ident need a declaration    */
    explicit;                       /* true is explicitly declared           */

  struct ast_node 
    *startDim[MAX_ARRAY_DIM],       /* start expression for each dimension   */
                                    /* also used as start exp idx for substr */
    *endDim[MAX_ARRAY_DIM],         /* ending expression for each dimension  */
                                    /* also used as end exp idx for substr   */
    *arraylist;                     /* expression representing array size    */

  char 
    *leaddim,                       /* leading dimension variable or const   */
    *opcode,	                    /* A string records the appropriate      * 
                                     * method to invoke on the stack when    * 
                                     * opcode is emitted.                    * 
                                     * e.g., opcode = strdup("iload_1");     */
    *commonBlockName,               /* name of COMMON block this ident is in */
    name[MAX_CONST_LEN],            /* this ident's name                     */
    *merged_name,                   /* this ident's merged name (e.g. in     *
                                     * cases of equivalence or COMMON)       */
    *descriptor,                    /* constant pool descriptor of the ident */
    *buffered_comments;             /* comments found within continued lines */
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
    endif_label;                    /* label of ENDIF stmt if present        */

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
    format_num,                     /* FORMAT desc for this statement        */
    err,                            /* where to branch on error              */
    end_num;                        /* where to branch on EOF                */

  struct ast_node 
    *unit_desc,                     /* unit descriptor                       */
    *iostat,                        /* identifier for i/o status specifier   */
    *rec,                           /* expression for record number          */
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
  char *descriptor;                 /* the common block descriptor           */
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
    struct _open           open, close, reb;
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

enum _intrinsics {
  ifunc_INT, ifunc_IFIX, ifunc_IDINT, ifunc_REAL, ifunc_FLOAT, ifunc_SNGL,
  ifunc_DBLE, ifunc_CMPLX, ifunc_ICHAR, ifunc_CHAR, ifunc_AINT, ifunc_DINT,
  ifunc_ANINT, ifunc_DNINT, ifunc_NINT, ifunc_IDNINT, ifunc_ABS, ifunc_IABS,
  ifunc_DABS, ifunc_CABS, ifunc_MOD, ifunc_AMOD, ifunc_DMOD, ifunc_SIGN,
  ifunc_ISIGN, ifunc_DSIGN, ifunc_DIM, ifunc_IDIM, ifunc_DDIM, ifunc_DPROD,
  ifunc_MAX, ifunc_MAX0, ifunc_AMAX1, ifunc_DMAX1, ifunc_AMAX0, ifunc_MAX1,
  ifunc_MIN, ifunc_MIN0, ifunc_AMIN1, ifunc_DMIN1, ifunc_AMIN0, ifunc_MIN1,
  ifunc_LEN, ifunc_INDEX, ifunc_AIMAG, ifunc_CONJG, ifunc_SQRT, ifunc_DSQRT,
  ifunc_CSQRT, ifunc_EXP, ifunc_DEXP, ifunc_CEXP, ifunc_LOG, ifunc_ALOG,
  ifunc_DLOG, ifunc_CLOG, ifunc_LOG10, ifunc_ALOG10, ifunc_DLOG10, ifunc_SIN,
  ifunc_DSIN, ifunc_CSIN, ifunc_COS, ifunc_DCOS, ifunc_CCOS, ifunc_TAN,
  ifunc_DTAN, ifunc_ASIN, ifunc_DASIN, ifunc_ACOS, ifunc_DACOS, ifunc_ATAN,
  ifunc_DATAN, ifunc_ATAN2, ifunc_DATAN2, ifunc_SINH, ifunc_DSINH, ifunc_COSH,
  ifunc_DCOSH, ifunc_TANH, ifunc_DTANH, ifunc_LGE, ifunc_LGT, ifunc_LLE,
  ifunc_LLT, ifunc_ETIME, ifunc_SECOND
};

typedef struct method_tab
{
  enum _intrinsics intrinsic;      /* id of this intrinsic                  */
  char *fortran_name;              /* name of the Fortran intrinsic         */

  /* for Java source generation: */
  char *java_method;                /* name of the corresponding Java func   */
  char *strict_java_method;         /* strict version (e.g. StrictMath.abs)  */

  /* for bytecode generation: */
  char *class_name;                 /* fully qualified Java class name       */
  char *strict_class_name;          /* strict version of the class name      */ 
  char *method_name;                /* method name                           */
  char *descriptor;                 /* corresponding Java func descriptor    */

  char args;                        /* bitfield of valid args to intrinsic   */
  enum returntype ret;              /* return type of this intrinsic         */
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
  unsigned int val;                 /* value                                 */
} SUBSTITUTION;


/*****************************************************************************
 * Function prototypes to keep the compiler from complaining.                *
 *****************************************************************************/

void 
  javaheader (FILE *, char *),
  initialize(void),
  uppercase(char *),
  print_vcg_header(FILE *, char *),
  print_vcg_trailer(FILE *),
  print_vcg_node(FILE *, int, char *),
  print_vcg_nearedge(FILE *, int, int),
  print_vcg_edge(FILE *, int, int),
  print_vcg_typenode(FILE *, int, char *);

Dlist
  build_method_table(char *);

char
  * get_method_descriptor(AST *, SYMTABLE *, SYMTABLE *, SYMTABLE *),
  * print_nodetype ( AST * );

struct _str
  * strAppend(struct _str *, char *);

int
  isPassByRef(char *, SYMTABLE *, SYMTABLE *, SYMTABLE *);

BOOL
  isPassByRef_desc(char *);

double
  mypow(double, double);

AST *clone_ident(AST *);

#endif
