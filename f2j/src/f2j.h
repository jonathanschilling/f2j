#include<assert.h>
#include"symtab.h"

/*  Fortran is context sensitive.  These 
   boolean values are an attempt to deal 
   with that sensitivity.
 */

typedef int BOOLEAN;
#define TRUE 1
#define FALSE 0
#define VCG 0   /* define VCG to get graph output */
#define BLAS 0
#define LAPACK 1
#define DEFAULT_TARGET_LANG 0 /* 0 for JAVA, 1 for JAS */

/*  If 1, yyparse produces voluminous, detailed
    output to stderr during parsing.  */
#define DEBUGGEM 0

BOOLEAN typedecs;
int lineno;
int statementno;
int func_stmt_num;
FILE *ifp;
FILE *jasminfp;
/* FILE *javafp; */
FILE *vcgfp;
int JAS;

/* Dlist tokenstack; */
SYMTABLE *type_table;
SYMTABLE *external_table;
SYMTABLE *intrinsic_table;
SYMTABLE *args_table;
SYMTABLE *array_table; 
SYMTABLE *format_table; 
SYMTABLE *data_table; 
SYMTABLE *save_table; 
SYMTABLE *common_table; 
SYMTABLE *parameter_table; 
SYMTABLE *function_table; 
SYMTABLE *java_keyword_table; 
SYMTABLE *jasmin_keyword_table; 
SYMTABLE *common_block_table;

int locals;
int stacksize;
enum spectype
  {
      External,
      Intrinsic,
      Implicit,
      Parameter
  };


enum returntype
  {
      String,
      Character,
      Complex,
      Double,
      Float,
      Integer,
      Logical
  };

/*  The main data structure, a "tagged union". */

typedef struct ast_node
  {

      int token;
      enum returntype vartype;
      
      struct ast_node *nextstmt;
      struct ast_node *prevstmt;
      struct ast_node *parent;

      enum _expr_side
      {
	left,
	right
      }expr_side;

      enum _nodetype
	{
	    Source = 1,
	    Progunit,
	    Subroutine,
	    Function,
            Program,
	    Blockif,
            Common,
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
	    Typedec,
	    Assignment,
	    Expression,
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
            Stop,
            ComputedGoto,
            ArrayAccess,
            ArrayDec,
            EmptyArgList,
	    Unimplemented
	}
      nodetype;

      union
	{
	    struct _source
	      {
		  enum returntype returns;
		  struct ast_node *name;
		  struct ast_node *progtype;
		  struct ast_node *typedecs;
		  struct ast_node *statements;
		  struct s_table *nametable;
		  struct ast_node *args;
                  SYMTABLE *type_table;
                  SYMTABLE *external_table;
                  SYMTABLE *intrinsic_table;
                  SYMTABLE *args_table;
                  SYMTABLE *array_table; 
                  SYMTABLE *format_table; 
                  SYMTABLE *data_table; 
                  SYMTABLE *save_table; 
                  SYMTABLE *common_table; 
                  SYMTABLE *parameter_table; 
                  struct ast_node *dataStmtList;
	      }
	    source;

	    struct _assignment
	      {
		  BOOLEAN parens;
                  int label;
		  char minus;
		  char optype;
		  char *opcode;
		  struct ast_node *lhs;
		  struct ast_node *rhs;
	      }
	    assignment, expression;

	    struct _typeunit
	      {
		  int tokentype;
		  enum spectype specification;
		  enum returntype returns;
		  struct ast_node *declist;
	      }
	    typeunit;

	    struct _forloop
	      {
		  /* char indexname[30]; */
                  int startlabel, stoplabel;
		  struct ast_node *counter;
		  struct ast_node *Label, *Continue;
		  struct ast_node *start, *stop, *stmts, *incr;
	      }
	    forloop;

	    struct _constant
	      {
		  int type;
		  char *opcode;	/* e.g., iconst_1, bipush 121.23  */
		  char number[30];
                  int sign;     /* sign used for data statements when we dont
                                   want to allow full expressions, but we
                                   need to allow negative constants.  if
                                   sign == 1, the constant is negative.
                                                    9/30/97  --Keith */
	      }
	    constant;

	    struct _label
	      {
		  int number;
		  struct ast_node *stmt;
/* To construct a flow graph, need an array of "called_from"
   pointers, that point back to the appropriate goto statements. */
	      }
	    label;

	    struct _ident
	      {
		  struct ast_node *arraylist;
		  char  * leaddim;
                  struct ast_node *lead_expr;
                  int dim;
                  int D[3];
		  char *opcode;	/* e.g., opcode = strdup("iload_1"); */
		  /*  A string records the appropriate method
		     to invoke on the stack when opcode is
		     emitted.  */
		  char *invokemethod;
                  char *commonBlockName;
		  int localvnum;
		  char name[80];
                  char *merged_name;
                  int needs_declaration;
                  int len;
	      }
	    ident;

	    struct _logicalif
	      {
		  int skip_label, fall_label, break_label;
		  struct ast_node *conds, *stmts;
		  struct ast_node *elseifstmts, *elsestmts;
	      }
	    logicalif, blockif;

	    struct _goto
	      {
		  int label;
		  /*  Use for the `label' node.  */
		  struct ast_node *callingstmt;
		  /*  Use for the `goto' node.  */
		  struct ast_node *labelstmt;
/*  Using the name "targetnode" might be less confusing. */
	      }
	    go_to;		/*, label; *//* goto is a reserved word! */

            struct _io
              {
                int io_type, file_desc, format_num;
                struct ast_node *fmt_list;
                struct ast_node *arg_list;
              }
            io_stmt;

	    struct _data_stmt
	      {
		  struct ast_node *nlist;
		  struct ast_node *clist;
	      }
            data;

	    struct _commonblock
	      {
		  char *name;
		  struct ast_node *nlist;
	      }
            common;

	    struct _computed_goto
	      {
		  struct ast_node *name;
		  struct ast_node *intlist;
	      }
            computed_goto;
	}
      astnode;


  }
AST;


/* A struct to keep track of compiler and translation
   options.  */
typedef struct _options 
{
  BOOLEAN lapack;
  BOOLEAN blas;
  BOOLEAN arrays1D;
  BOOLEAN arrays2D;
} OPTIONS;

/* I don't think I use this struct anymore.  */
typedef struct _list_node
  {
      int token;
      char name[30];
  }
List_node;


/* The Fortran lexical analyzer is very context dependent, here we list the
   various contexts that it knows about.
 */

enum contexts
  {
      none = 0,
      type			/* Need to type the following tokens.  */
  }
context;

/* keyword lookup table */
typedef struct _kwdtab
  {
      char *kwd;		/* text of the keyword */
      int ktok;			/* token code */
      int klex;			/* lexical value */
  }
KWDTAB;

/* Java intrinsic methods.. */
typedef struct method_tab
  {
      char *fortran_name;
      char *java_method;  /*  Some of the jasmin names are long. */
  }
METHODTAB;

/* 
typedef struct method_tab
  {
      char fortran_name[30];
      char java_method[150];
  }
METHODTAB;
*/

/* relops */
enum relops
  {
      rel_eq = 1,
      rel_ne,
      rel_lt,
      rel_le,
      rel_gt,
      rel_ge
  };

typedef struct {
  char *name;
  int val;
} SUBSTITUTION;

/*  Prototypes to keep the compiler from complaining.  */

void jasminheader (FILE *, char *);
void javaheader (FILE *, char *);
void return_emit (AST *);
void logicalop_assign(AST *);
void relationalop_assign(AST *);
void logicalop_emit(AST *);
void elseif_emit(AST *);
void elseif_assign(AST *);
void else_emit(AST *);
void else_assign(AST *);
void store_array_var(AST *);
void initialize();
void uppercase(char *);
void while_emit(AST *);
AST *format_item_emit(AST *, AST**);
