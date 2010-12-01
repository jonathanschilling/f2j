/*
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */

%{

/*****************************************************************************
 * f2jparse                                                                  *
 *                                                                           *
 * This is a yacc parser for a subset of Fortran 77.  It builds an AST       *
 * which is used by codegen() to generate Java code.                         *
 *                                                                           *
 *****************************************************************************/

#include<stdio.h>
#include<stdlib.h>
#include<ctype.h>
#include<string.h>
#include"f2j.h"
#include"f2j_externs.h"
#include"f2jmem.h"

/*****************************************************************************
 * Define YYDEBUG as 1 to get debugging output from yacc.                    *
 *****************************************************************************/

#define YYDEBUG 0

/*****************************************************************************
 * Global variables.                                                         *
 *****************************************************************************/

int 
  debug = FALSE,                  /* set to TRUE for debugging output        */
  emittem = 1,                    /* set to 1 to emit Java, 0 to just parse  */
  len = 1,                        /* keeps track of the size of a data type  */
  temptok,                        /* temporary token for an inline expr      */
  save_all,                       /* is there a SAVE stmt without a var list */
  cur_do_label;                   /* current 'do..end do' loop label         */
  
AST 
  * unit_args = NULL,             /* pointer to args for this program unit   */
  * equivList = NULL;             /* list to keep track of equivalences      */

Dlist 
  assign_labels,                  /* labels used in ASSIGN TO statements     */
  subroutine_names,               /* holds the names of subroutines          */
  do_labels;                      /* generated labels for 'do..end do' loops */

enum returntype
  typedec_context = Object;       /* what kind of type dec we are parsing    */

/*****************************************************************************
 * Function prototypes:                                                      *
 *****************************************************************************/

METHODTAB
  * methodscan (METHODTAB *, char *);

int 
  yylex(void),
  intrinsic_or_implicit(char *),
  in_dlist_stmt_label(Dlist, AST *),
  in_dlist(Dlist, char *);

double
  eval_const_expr(AST *);

char 
  * lowercase(char * ),
  * first_char_is_minus(char *),
  * unary_negate_string(char *),
  * tok2str(int );

void
  yyerror(char *),
  start_vcg(AST *),
  emit(AST *),
  jas_emit(AST *),
  init_tables(void),
  addEquiv(AST *),
  assign(AST *),
  typecheck(AST *),
  optScalar(AST *),
  type_insert (SYMTABLE * , AST * , enum returntype , char *),
  type_hash(AST *),
  merge_common_blocks(AST *),
  arg_table_load(AST *),
  exp_to_double (char *, char *),
  assign_function_return_type(AST *, AST *),
  insert_name(SYMTABLE *, AST *, enum returntype),
  store_array_var(AST *),
  initialize_implicit_table(ITAB_ENTRY *),
  get_info_from_cilist(AST *, AST *),
  get_info_from_cllist(AST *, AST *),
  get_info_from_olist(AST *, AST *),
  printbits(char *, void *, int),
  print_sym_table_names(SYMTABLE *);

AST 
  * dl_astnode_examine(Dlist l),
  * addnode(void),
  * switchem(AST *),
  * gen_incr_expr(AST *, AST *),
  * gen_iter_expr(AST *, AST *, AST *),
  * initialize_name(char *),
  * process_typestmt(enum returntype, AST *),
  * process_array_declaration(AST *, AST *),
  * process_subroutine_call(AST *, AST *);

SYMTABLE 
  * new_symtable (int );

extern METHODTAB intrinsic_toks[];

ITAB_ENTRY implicit_table[26];

%}

%union {
   struct ast_node *ptnode;
   int tok;
   enum returntype type;
   char lexeme[YYTEXTLEN];
}

/* generic tokens */

%token PLUS MINUS OP CP STAR POW DIV CAT CM EQ COLON NL
%token NOT AND OR
%token RELOP EQV NEQV
%token <lexeme>  NAME DOUBLE INTEGER E_EXPONENTIAL D_EXPONENTIAL
%token CONST_EXP TrUE FaLSE ICON RCON LCON CCON
%token FLOAT CHARACTER LOGICAL COMPLEX NONE

/* a zillion keywords */

%token IF THEN ELSE ELSEIF ENDIF DO GOTO ASSIGN TO CONTINUE STOP
%token RDWR END ENDDO STRING CHAR  PAUSE
%token OPEN CLOSE BACKSPACE REWIND ENDFILE FORMAT
%token PROGRAM FUNCTION SUBROUTINE ENTRY CALL RETURN
%token <type> ARITH_TYPE CHAR_TYPE 
%token DIMENSION INCLUDE NO_PROGRAM DUMMY
%token COMMON EQUIVALENCE EXTERNAL PARAMETER INTRINSIC IMPLICIT
%token SAVE DATA COMMENT BLOCK_COMMENT READ WRITE PRINT EDIT_DESC REPEAT

%token IOSPEC_IOSTAT IOSPEC_ERR IOSPEC_FILE IOSPEC_STATUS IOSPEC_ACCESS 
%token IOSPEC_FORM IOSPEC_UNIT IOSPEC_RECL IOSPEC_REC IOSPEC_BLANK
%token IOSPEC_END IOSPEC_EMPTY IOSPEC_FMT

/* these are here to silence conflicts related to parsing comments */

%nonassoc RELOP 
%nonassoc LOWER_THAN_COMMENT
%nonassoc COMMENT

/*  All of my additions or changes to Levine's code. These 
 * non-terminals are in alphabetic order because I have had to 
 * change the grammar quite a bit.  It is tiring trying to root
 * out the location of a non-terminal, much easier to find when
 * in alphabetic order. 
 */

%type <ptnode> Arraydeclaration Arrayname Arraynamelist Assignment
%type <ptnode> Arrayindexlist Arithmeticif ArraydecList AssignedGoto
%type <ptnode> Blockif Boolean Close Comment
%type <ptnode> Call Constant Continue EndDo
%type <ptnode> Data DataList DataConstantExpr DataConstant DataItem 
%type <ptnode> /* DataElement */ Do_incr Doloop 
%type <ptnode> DataLhs DataConstantList Dimension LoopBounds
%type <ptnode> Do_vals Double Float
%type <ptnode> EquivalenceStmt EquivalenceList EquivalenceItem
%type <ptnode> Else Elseif Elseifs EndIf End Exp Explist Exponential External
%type <ptnode> Function Functionargs F2java
%type <ptnode> Fprogram Ffunction Fsubroutine
%type <ptnode> Goto Common CommonList CommonSpec ComputedGoto
%type <ptnode> IfBlock Implicit Integer Intlist Intrinsic
%type <ptnode> ImplicitSpecItem ImplicitLetterList ImplicitLetter
%type <ptnode> Label Lhs Logicalif
%type <ptnode> Name UndeclaredName Namelist UndeclaredNamelist
%type <ptnode> LhsList Open
%type <ptnode> Parameter  Pdec Pdecs Program PrintIoList
%type <ptnode> Read IoExp IoExplist Return  Rewind
%type <ptnode> Save Specstmt Specstmts SpecStmtList Statements 
%type <ptnode> Statement StmtLabelAssign Subroutinecall
%type <ptnode> Sourcecodes  Sourcecode Star
%type <ptnode> String  Subroutine Stop SubstringOp Pause
%type <ptnode> Typestmt ArithTypevar ArithTypevarlist
%type <ptnode> CharTypevar CharTypevarlist
%type <type>   ArithTypes ArithSimpleType CharTypes CharSimpleType
%type <type>   AnySimpleType AnyTypes
%type <ptnode> Write FormatOrUnknownSpec
%type <ptnode> Format FormatExplist FormatExp FormatSeparator
%type <ptnode> RepeatableItem UnRepeatableItem RepeatSpec 
%type <ptnode> log_disjunct log_term log_factor log_primary
%type <ptnode> arith_expr term factor char_expr primary
%type <ptnode> IostatExp CharExp ReclExp OlistItem Olist OpenFileSpec
%type <ptnode> ErrExp StatusExp AccessExp FormExp BlankExp UnitExp
%type <ptnode> RecExp EndExp CllistItem Cllist CilistItem Cilist

%%

F2java:   Sourcecodes
          {
            AST *temp, *prev, *commentList = NULL;

            if(debug)
              printf("F2java -> Sourcecodes\n");
	    $$ = switchem($1);

#if VCG
            if(emittem) start_vcg($$);
#endif
            prev = NULL;
            for(temp=$$;temp!=NULL;temp=temp->nextstmt)
            {
              if(emittem) {

                if(temp->nodetype == Comment)
                {
                  if((prev == NULL) ||
                     ((prev != NULL) && (prev->nodetype != Comment)))
                    commentList = temp;
                }
                else
                {
                  /* commentList may be NULL here so we must check
                   * for that in codegen.
                   */
                  temp->astnode.source.prologComments = commentList;

                  typecheck(temp);

                  if(omitWrappers)
                    optScalar(temp);

                  emit(temp);

                  commentList = NULL;
                }
              }
              prev = temp;
            }
          }
;

Sourcecodes:   Sourcecode 
               {
                 AST *temp;

                 if(debug)
                   printf("Sourcecodes -> Sourcecode\n"); 
                 $$=$1;

                 /* insert the name of the program unit into the
                  * global function table.  this will allow optScalar()
                  * to easily get a pointer to a function. 
                  */

                 if(omitWrappers && ($1->nodetype != Comment)) {
                   temp = $1->astnode.source.progtype->astnode.source.name;
                   
                   type_insert(global_func_table, $1, 0, temp->astnode.ident.name);
                 }
               }
             | Sourcecodes Sourcecode 
               {
                 AST *temp;

                 if(debug)
                   printf("Sourcecodes -> Sourcecodes Sourcecode\n");
                 $2->prevstmt = $1; 
                 $$=$2;

                 /* insert the name of the program unit into the
                  * global function table.  this will allow optScalar()
                  * to easily get a pointer to a function. 
                  */

                 if(omitWrappers && ($2->nodetype != Comment)) {
                   temp = $2->astnode.source.progtype->astnode.source.name;

                   type_insert(global_func_table, $2, 0, temp->astnode.ident.name);
                 }
               }
;

Sourcecode :    Fprogram
                { 
                  if(debug)
                    printf("Sourcecode -> Fprogram\n"); 
                  $$=$1; 
                }
              | Fsubroutine
                { 
                  if(debug)
                    printf("Sourcecode -> Fsubroutine\n"); 
                  $$=$1;
                }
              | Ffunction
                {
                  if(debug)
                    printf("Sourcecode -> Ffunction\n"); 
                  $$=$1;
                }
              | Comment
                { 
                  if(debug)
                    printf("Sourcecode -> Comment\n"); 
                  $$=$1;
                }
;

Fprogram:   Program Specstmts Statements End 
              {
                if(debug)
                  printf("Fprogram -> Program  Specstmts  Statements End\n");

                add_implicit_to_tree($2);

                $$ = addnode();

                /* store the tables built during parsing into the
                 * AST node for access during code generation.
                 */

                $$->astnode.source.type_table = type_table;
                $$->astnode.source.external_table = external_table;
                $$->astnode.source.intrinsic_table = intrinsic_table;
                $$->astnode.source.args_table = args_table;
                $$->astnode.source.array_table = array_table; 
                $$->astnode.source.format_table = format_table; 
                $$->astnode.source.data_table = data_table; 
                $$->astnode.source.save_table = save_table; 
                $$->astnode.source.common_table = common_table; 
                $$->astnode.source.parameter_table = parameter_table; 
                $$->astnode.source.constants_table = constants_table;
                $$->astnode.source.equivalences = equivList; 
                $$->astnode.source.stmt_assign_list = assign_labels; 

                $$->astnode.source.javadocComments = NULL; 
                $$->astnode.source.save_all = save_all; 

                /* initialize some values in this node */

                $$->astnode.source.needs_input = FALSE;
                $$->astnode.source.needs_output = FALSE;
                $$->astnode.source.needs_files = FALSE;
                $$->astnode.source.needs_iostat = FALSE;
                $$->astnode.source.needs_reflection = FALSE;

                if(omitWrappers)
                  $$->astnode.source.scalarOptStatus = NOT_VISITED;

	        $1->parent = $$; /* 9-4-97 - Keith */
	        if($2) $2->parent = $$; /* 9-4-97 - Keith */
	        $3->parent = $$; /* 9-4-97 - Keith */
	        $4->parent = $$; /* 9-4-97 - Keith */
                $$->nodetype = Progunit;
                $$->astnode.source.progtype = $1;
                $$->astnode.source.typedecs = $2;
                $4->prevstmt = $3;
                $$->astnode.source.statements = switchem($4);

                /* a PROGRAM has no args, so set the symbol table
                   to NULL */
                args_table = NULL;  

                $1->astnode.source.descriptor = MAIN_DESCRIPTOR;
              }
;


Fsubroutine: Subroutine Specstmts Statements End 
              {
                HASHNODE *ht;
                AST *temp;

                if(debug)
                  printf("Fsubroutine -> Subroutine Specstmts Statements End\n");

                add_implicit_to_tree($2);
                
                $$ = addnode();
	        $1->parent = $$; 
	        if($2) $2->parent = $$;
	        $3->parent = $$;
	        $4->parent = $$;
                $$->nodetype = Progunit;
                $$->astnode.source.progtype = $1;

                /* store the tables built during parsing into the
                 * AST node for access during code generation.
                 */

                $$->astnode.source.type_table = type_table;
                $$->astnode.source.external_table = external_table;
                $$->astnode.source.intrinsic_table = intrinsic_table;
                $$->astnode.source.args_table = args_table;
                $$->astnode.source.array_table = array_table; 
                $$->astnode.source.format_table = format_table; 
                $$->astnode.source.data_table = data_table; 
                $$->astnode.source.save_table = save_table; 
                $$->astnode.source.common_table = common_table; 
                $$->astnode.source.parameter_table = parameter_table; 
                $$->astnode.source.constants_table = constants_table;
                $$->astnode.source.equivalences = equivList; 
                $$->astnode.source.stmt_assign_list = assign_labels; 

                $$->astnode.source.javadocComments = NULL; 
                $$->astnode.source.save_all = save_all; 

                /* initialize some values in this node */

                $$->astnode.source.needs_input = FALSE;
                $$->astnode.source.needs_output = FALSE;
                $$->astnode.source.needs_files = FALSE;
                $$->astnode.source.needs_iostat = FALSE;
                $$->astnode.source.needs_reflection = FALSE;

                if(omitWrappers)
                  $$->astnode.source.scalarOptStatus = NOT_VISITED;

                $$->astnode.source.typedecs = $2;
                $4->prevstmt = $3;
                $$->astnode.source.statements = switchem($4);

                /* foreach arg to this program unit, store the array 
                 * size, if applicable, from the hash table into the
                 * node itself.
                 */
              
                for(temp=$1->astnode.source.args;temp!=NULL;temp=temp->nextstmt)
                {
                  if((ht=type_lookup(type_table,temp->astnode.ident.name)) != NULL)
                  {
                    temp->vartype=ht->variable->vartype;
                    temp->astnode.ident.arraylist=ht->variable->astnode.ident.arraylist;
                  }
                  if((ht=type_lookup(args_table, temp->astnode.ident.name)) != NULL){
                      ht->variable->vartype=temp->vartype;
                  }
                }
                
                type_insert(function_table, $1, 0,
                   $1->astnode.source.name->astnode.ident.name);
              }
;

Ffunction:   Function Specstmts Statements  End
              {
                HASHNODE *ht;
                AST *temp;

                if(debug)
                  printf("Ffunction ->   Function Specstmts Statements  End\n");
             
                if(!$1->astnode.source.explicit_decl)
                  assign_function_return_type($1, $2);

                add_implicit_to_tree($2);

                $$ = addnode();

                /* store the tables built during parsing into the
                 * AST node for access during code generation.
                 */

                $$->astnode.source.type_table = type_table;
                $$->astnode.source.external_table = external_table;
                $$->astnode.source.intrinsic_table = intrinsic_table;
                $$->astnode.source.args_table = args_table;
                $$->astnode.source.array_table = array_table; 
                $$->astnode.source.format_table = format_table; 
                $$->astnode.source.data_table = data_table; 
                $$->astnode.source.save_table = save_table; 
                $$->astnode.source.common_table = common_table; 
                $$->astnode.source.parameter_table = parameter_table; 
                $$->astnode.source.constants_table = constants_table;
                $$->astnode.source.equivalences = equivList; 
                $$->astnode.source.stmt_assign_list = assign_labels; 

                $$->astnode.source.javadocComments = NULL; 
                $$->astnode.source.save_all = save_all; 

                /* initialize some values in this node */

                $$->astnode.source.needs_input = FALSE;
                $$->astnode.source.needs_output = FALSE;
                $$->astnode.source.needs_files = FALSE;
                $$->astnode.source.needs_iostat = FALSE;
                $$->astnode.source.needs_reflection = FALSE;
                if(omitWrappers)
                  $$->astnode.source.scalarOptStatus = NOT_VISITED;

	        $1->parent = $$; /* 9-4-97 - Keith */
	        if($2) $2->parent = $$; /* 9-4-97 - Keith */
	        $3->parent = $$; /* 9-4-97 - Keith */
	        $4->parent = $$; /* 9-4-97 - Keith */
                $$->nodetype = Progunit;
                $$->astnode.source.progtype = $1;
                $$->astnode.source.typedecs = $2;
		$4->prevstmt = $3;
                $$->astnode.source.statements = switchem($4);

                /* foreach arg to this program unit, store the array 
                 * size, if applicable, from the hash table into the
                 * node itself.
                 */

                for(temp=$1->astnode.source.args;temp!=NULL;temp=temp->nextstmt)
                {
                  if((ht=type_lookup(type_table,temp->astnode.ident.name)) != NULL)
                  {
                    temp->vartype=ht->variable->vartype;
                    temp->astnode.ident.arraylist=ht->variable->astnode.ident.arraylist;
                  }
                  if((ht=type_lookup(args_table, temp->astnode.ident.name)) != NULL){
                      ht->variable->vartype=temp->vartype;
                  }
                }
                      
                type_insert(function_table, $1, 0,
                  $1->astnode.source.name->astnode.ident.name);
              }

;

Program:      PROGRAM UndeclaredName NL
              {
                 if(debug)
                   printf("Program ->  PROGRAM UndeclaredName\n");
                 
                 unit_args = NULL;

                 $$ = addnode();
	         $2->parent = $$; /* 9-4-97 - Keith */
		 lowercase($2->astnode.ident.name);
		 $$->astnode.source.name = $2;
                 $$->nodetype = Program;
                 $$->token = PROGRAM;
                 $$->astnode.source.args = NULL;

                 init_tables();
                
                 fprintf(stderr," MAIN %s:\n",$2->astnode.ident.name);
              }
        |     NO_PROGRAM
              {
                AST *name_node;

                if(debug)
                  printf("Program ->  NO_PROGRAM\n");

                unit_args = NULL;

                name_node = addnode();
                name_node->token = NAME;
                name_node->nodetype = Identifier;

                name_node->astnode.ident.needs_declaration = FALSE;

                if(omitWrappers)
                  name_node->astnode.ident.passByRef = FALSE;

                strcpy(name_node->astnode.ident.name, "f2jmain");

                $$ = addnode();
                name_node->parent = $$;
                $$->astnode.source.name = name_node;
                $$->nodetype = Program;
                $$->token = PROGRAM;
                $$->astnode.source.args = NULL;

                init_tables();

                fprintf(stderr," MAIN %s:\n",name_node->astnode.ident.name);
              }
;

Subroutine: SUBROUTINE UndeclaredName Functionargs NL
              {
                 if(debug)
                   printf("Subroutine ->  SUBROUTINE UndeclaredName Functionargs NL\n");

                 unit_args = $3;

                 $$ = addnode();
                 $2->parent = $$; /* 9-4-97 - Keith */
                 if($3 != NULL)
                   $3->parent = $$; /* 9-4-97 - Keith */

                 $$->astnode.source.name = $2; 
                 $$->nodetype = Subroutine;
                 $$->token = SUBROUTINE;
                 $$->astnode.source.args = switchem($3);
                
                 fprintf(stderr,"\t%s:\n",$2->astnode.ident.name);
              }
          | SUBROUTINE UndeclaredName NL
              {
                 if(debug)
                   printf("Subroutine ->  SUBROUTINE UndeclaredName NL\n");

                 unit_args = NULL;

                 init_tables();
                 $$ = addnode();
                 $2->parent = $$; /* 9-4-97 - Keith */

                 $$->astnode.source.name = $2; 
                 $$->nodetype = Subroutine;
                 $$->token = SUBROUTINE;
                 $$->astnode.source.args = NULL;

                 fprintf(stderr,"\t%s:\n",$2->astnode.ident.name);
              }
;

Function:  AnySimpleType FUNCTION UndeclaredName Functionargs NL 
           {
             if(debug)
               printf("Function ->  AnySimpleType FUNCTION UndeclaredName Functionargs NL\n");

             unit_args = $4;

             $$ = addnode();

  	     $3->parent = $$;  /* 9-4-97 - Keith */
             if($4 != NULL)
               $4->parent = $$;  /* 9-4-97 - Keith */
             $$->astnode.source.name = $3;
             $$->nodetype = Function;
             $$->token = FUNCTION;
             $$->astnode.source.returns = $1;
             $$->astnode.source.explicit_decl = TRUE;
             $$->vartype = $1;
             $3->vartype = $1;
             $$->astnode.source.args = switchem($4);

             /* since the function name is the implicit return value
              * and it can be treated as a variable, we insert it into
              * the hash table for lookup later.
              */

             $3->astnode.ident.localvnum = -1;
             insert_name(type_table, $3, $1);
           
             fprintf(stderr,"\t%s:\n",$3->astnode.ident.name);
          }
        | FUNCTION UndeclaredName Functionargs NL
          {
             enum returntype ret;

             unit_args = $3;

             $$ = addnode();

             $2->parent = $$;  
             if($3 != NULL)
               $3->parent = $$;  
             $$->astnode.source.name = $2;
             $$->nodetype = Function;
             $$->token = FUNCTION;
             ret = implicit_table[tolower($2->astnode.ident.name[0]) - 'a'].type;
             $$->astnode.source.returns = ret;
             $$->astnode.source.explicit_decl = FALSE;
             $$->vartype = ret;
             $2->vartype = ret;
             $$->astnode.source.args = switchem($3);
        
             $2->astnode.ident.localvnum = -1;
             insert_name(type_table, $2, ret);
            
             fprintf(stderr,"\t%s:\n",$2->astnode.ident.name);
          }
; 

Specstmts: SpecStmtList    %prec LOWER_THAN_COMMENT
           {
             AST *tmparg;

             if(debug)
               printf("Specstmts -> SpecStmtList\n");

             $1 = switchem($1);
             type_hash($1); 
             $$=$1;

             for(tmparg = unit_args; tmparg; tmparg=tmparg->nextstmt) {
               HASHNODE *ht;

               ht = type_lookup(type_table, tmparg->astnode.ident.name);

               if(ht) {
                 if(!ht->variable->astnode.ident.explicit)
                   ht->variable->vartype = 
                     implicit_table[tolower(tmparg->astnode.ident.name[0]) - 'a'].type;
               }
               else
                 fprintf(stderr, "warning: didn't find %s in symbol table\n", 
                   tmparg->astnode.ident.name);
             }
           }
         | /* NULL */  %prec LOWER_THAN_COMMENT
           {
             if(debug)
               printf("Specstmts -> [NULL]\n");

             $$ = NULL;
           }
;

SpecStmtList: Specstmt
           {
             $$=$1;
           }
         | SpecStmtList  Specstmt
           { 
             $2->prevstmt = $1; 
             $$ = $2; 
           }
;

Specstmt:  Dimension
           {
	     $$ = $1;
	   }
         | EquivalenceStmt
	   {
	     $$ = $1;
	   }
         | Common
	   {
	     $$ = $1;
	   }
         | Save      
           {
             $$=$1;
           }
         | Intrinsic
           {
             $$=$1;
           }
         | Typestmt
           {
             $$=$1;
           }
         | External
           {
             $$=$1;
           }
         | Parameter
           {
             $$=$1;
           }
         | Implicit
           {
             $$=$1;
           }
         | Data NL
           {
             $$=$1;
           }
         | Comment
	   {
             $$ = $1;
	   }
;

Dimension: DIMENSION ArraydecList NL
           {
             $$ = addnode();
             $2->parent = $$;
             $2 = switchem($2);
             $$->nodetype = Dimension;

             $$->astnode.typeunit.declist = $2;
           }
;

ArraydecList: ArraydecList CM Arraydeclaration
              {
                $3->prevstmt = $1;
                $$ = $3;
                $$->nodetype = Dimension;
              }
            | Arraydeclaration
              {
                $$ = $1;
                $$->nodetype = Dimension;
              }
;

/*  the EQUIVALENCE productions are taken from Robert Moniot's 
 *  ftnchek grammar.
 */

EquivalenceStmt: EQUIVALENCE EquivalenceList NL
                 {
                   $$ = addnode();
                   $$->nodetype = Equivalence;
                   $$->prevstmt = NULL;
                   $$->nextstmt = NULL;
                   $$->astnode.equiv.nlist = switchem($2);
                 }
;

EquivalenceList: OP EquivalenceItem CP
                 {
                   AST *tmp;

                   $$ = addnode();
                   $$->nodetype = Equivalence;
                   $$->prevstmt = NULL;
                   $$->nextstmt = NULL;
                   $$->astnode.equiv.clist = switchem($2);

                   for(tmp=$2;tmp!=NULL;tmp=tmp->prevstmt)
                     tmp->parent = $$;

                   addEquiv($$->astnode.equiv.clist);
                 }
               | EquivalenceList CM OP EquivalenceItem CP
                 {
                   AST *tmp;

                   $$ = addnode();
                   $$->nodetype = Equivalence;
                   $$->astnode.equiv.clist = switchem($4);
                   $$->prevstmt = $1;
                   $$->nextstmt = NULL;

                   for(tmp=$4;tmp!=NULL;tmp=tmp->prevstmt)
                     tmp->parent = $$;

                   addEquiv($$->astnode.equiv.clist);
                 }
;

EquivalenceItem: Lhs
                 {
                   $$ = $1;
                 }
               | EquivalenceItem CM Lhs
                 {
                   $3->prevstmt = $1;
                   $$ = $3;
                 }
;

Common: COMMON CommonList NL
        {
          $$ = addnode();
          $$->nodetype = CommonList;
          $$->astnode.common.name = NULL;

          $$->astnode.common.nlist = switchem($2);
          merge_common_blocks($$->astnode.common.nlist);
        }
;

CommonList: CommonSpec
            {
              $$ = $1;
            }
         |  CommonList CommonSpec
            {
              $2->prevstmt = $1;
              $$ = $2;
            }
;

CommonSpec: DIV UndeclaredName DIV ArithTypevarlist
           {
              AST *temp;
              int pos;

              if(debug){
                 printf("CommonSpec -> DIV UndeclaredName DIV Namelist\n");
              }

              $$ = addnode();
              $$->nodetype = Common;
              $$->astnode.common.name = strdup($2->astnode.ident.name);
              $$->astnode.common.nlist = switchem($4);

              pos = 0;

              /* foreach variable in the COMMON block... */
              for(temp=$$->astnode.common.nlist;temp!=NULL;temp=temp->nextstmt)
              {
                temp->astnode.ident.commonBlockName = 
                  strdup($2->astnode.ident.name);

                if(omitWrappers)
                  temp->astnode.ident.position = pos++;

                /* insert this name into the common table */
                if(debug)
                  printf("@insert %s (block = %s) into common table\n",
                    temp->astnode.ident.name, $2->astnode.ident.name);

                type_insert(common_table, temp, Float, temp->astnode.ident.name);
              }

              type_insert(global_common_table, $$, Float, $$->astnode.common.name);
              free_ast_node($2);
           }
         | CAT ArithTypevarlist     /* CAT is // */
           {
              AST *temp;

              /* This is an unnamed common block */
              if(debug){
                printf("CommonSpec -> CAT Namelist\n");
              }

              $$ = addnode();
              $$->nodetype = Common;
              $$->astnode.common.name = strdup("Blank");
              $$->astnode.common.nlist = switchem($2);

              /* foreach variable in the COMMON block... */
              for(temp=$2;temp!=NULL;temp=temp->prevstmt) {
                temp->astnode.ident.commonBlockName = "Blank";

                /* insert this name into the common table */

                if(debug)
                  printf("@@insert %s (block = unnamed) into common table\n",
                    temp->astnode.ident.name);

                type_insert(common_table, temp, Float, temp->astnode.ident.name);
              }

              type_insert(global_common_table, $$, Float, $$->astnode.common.name);
           }
;

/* SAVE is ignored by the code generator.
 * ..not anymore 12/10/01 kgs 
 */

Save: SAVE NL
       {
         /*
          * I think in this case every variable is supposed to
          * be saved, but we already emit every variable as
          * static.  do nothing here.  --Keith
          */

         $$ = addnode();
         $$->nodetype = Save;
         save_all = TRUE;
       }
    | SAVE DIV Namelist DIV NL
           {
             AST *temp;
             
             if(debug){
                printf("Save -> SAVE DIV Namelist DIV NL\n");
             }
             $$ = addnode();
             $3->parent = $$; /* 9-4-97 - Keith */
             $$->nodetype = Save;

             for(temp=$3;temp!=NULL;temp=temp->prevstmt) {
               if(debug)
                 printf("@@insert %s into save table\n",
                    temp->astnode.ident.name);

               type_insert(save_table, temp, Float, temp->astnode.ident.name);
             }
	   }
    | SAVE Namelist NL
           {
             AST *temp;
             if(debug){
                printf("Save -> SAVE Namelist NL\n");
             }

             $$ = addnode();
             $2->parent = $$; /* 9-4-97 - Keith */
             $$->nodetype = Save;

             for(temp=$2;temp!=NULL;temp=temp->prevstmt) {
               if(debug)
                 printf("@@insert %s into save table\n",
                    temp->astnode.ident.name);

               type_insert(save_table, temp, Float, temp->astnode.ident.name);
             }
	   }
;

Implicit:   IMPLICIT ImplicitSpecList NL
            {
	      $$=addnode();
	      $$->nodetype = Specification;
	      $$->token = IMPLICIT;
	    }
         |  IMPLICIT NONE NL
            {
	      $$=addnode();
	      $$->nodetype = Specification;
	      $$->token = IMPLICIT;
              fprintf(stderr,"Warning: IMPLICIT NONE ignored.\n");
	    }
;

ImplicitSpecList: ImplicitSpecItem
                  {
                    /* I don't think anything needs to be done here */
                  }
                | ImplicitSpecList CM ImplicitSpecItem
                  {
                    /* or here either. */
                  }
;

ImplicitSpecItem:  AnyTypes OP ImplicitLetterList CP
                   {
                     AST *temp;

                     for(temp=$3;temp!=NULL;temp=temp->prevstmt) {
                       char *start_range, *end_range;
                       char start_char, end_char;
                       int i;

                       start_range = temp->astnode.expression.lhs->astnode.ident.name;
                       end_range = temp->astnode.expression.rhs->astnode.ident.name;

                       start_char = tolower(start_range[0]);
                       end_char = tolower(end_range[0]);

                       if((strlen(start_range) > 1) || (strlen(end_range) > 1)) {
                         yyerror("IMPLICIT spec must contain single character.");
                         exit(EXIT_FAILURE);
                       }

                       if(end_char < start_char) {
                         yyerror("IMPLICIT range in backwards order.");
                         exit(EXIT_FAILURE);
                       }

                       for(i=start_char - 'a'; i <= end_char - 'a'; i++) {
                         if(implicit_table[i].declared) {
                           yyerror("Duplicate letter specified in IMPLICIT statement.");
                           exit(EXIT_FAILURE);
                         }

                         implicit_table[i].type = $1;
                         implicit_table[i].declared = TRUE;
                         implicit_table[i].len = len;  /* global set in Types production */
                       }
                     }
                   }
;

ImplicitLetterList: ImplicitLetter
                    {
                      $$ = $1;
                    }
                  | ImplicitLetterList CM ImplicitLetter
                    {
                      $3->prevstmt = $1;
                      $$ = $3;
                    }
;

ImplicitLetter: UndeclaredName
                {
                  $$ = addnode();
                  $$->nodetype = Expression;
                  $$->astnode.expression.lhs = $1;
                  $$->astnode.expression.rhs = $1;
                }
              | UndeclaredName MINUS UndeclaredName
                {
                  $$ = addnode();
                  $$->nodetype = Expression;
                  $$->astnode.expression.lhs = $1;
                  $$->astnode.expression.rhs = $3;
                }
;

Data:       DATA DataList
            {
              /* $$ = $2; */
              $$ = addnode();
              $$->nodetype = DataList;
              $$->astnode.label.stmt = $2;
            } 
;

DataList:   DataItem
            {
              $$ = $1;
            }
        |   DataList CM DataItem
            {
              $3->prevstmt = $1;
              $$ = $3;
            }
;

DataItem:   LhsList DIV DataConstantList DIV
            {
              AST *temp;

              $$ = addnode();
              $$->astnode.data.nlist = switchem($1);
              $$->astnode.data.clist = switchem($3);

              $$->nodetype = DataStmt;
              $$->prevstmt = NULL;
              $$->nextstmt = NULL;

              for(temp=$1;temp!=NULL;temp=temp->prevstmt) {
                if(debug)
                  printf("@@insert %s into data table\n",
                     temp->astnode.ident.name);
                
                temp->parent = $$;

                if(temp->nodetype == DataImpliedLoop)
                  type_insert(data_table, temp, Float,
                     temp->astnode.forloop.Label->astnode.ident.name);
                else
                  type_insert(data_table, temp, Float, temp->astnode.ident.name);
              }
            }
;

DataConstantList:  DataConstantExpr
                   {
                     $$ = $1;
                   }
                |  DataConstantList CM DataConstantExpr
                   {
                     $3->prevstmt = $1;
                     $$ = $3;
                   }
;

DataConstantExpr: DataConstant 
                  {
                    $$ = $1;
                  }
                | DataConstant STAR DataConstant
                  {
                    $$ = $1;
                    $$=addnode();
                    $$->nodetype = Binaryop;
                    $$->token = STAR;
                    $1->expr_side = left;
                    $3->expr_side = right;
                    $1->parent = $$;
                    $3->parent = $$;
                    $$->astnode.expression.lhs = $1;
                    $$->astnode.expression.rhs = $3;
                    $$->astnode.expression.optype = '*';
                  }
;

DataConstant:  Constant
               {
                 $$ = $1;
               }
            |  UndeclaredName
               {
                 HASHNODE *hash_temp;
                 if((parameter_table != NULL) &&
                    ((hash_temp = type_lookup(parameter_table, yylval.lexeme)) != NULL))
                 {
                    $$ = addnode();
                    $$->nodetype = Constant;
                    $$->vartype = hash_temp->variable->vartype;
                    $$->token = hash_temp->variable->token;
                    $$->astnode.constant.number = strdup(hash_temp->variable->astnode.constant.number);
                 }
                 else{
                    printf("Error: '%s' is not a constant\n",yylval.lexeme);
                    exit(EXIT_FAILURE);
                 }
               }   
            |  MINUS Constant   
               {
                 char *neg_string;

                 neg_string = unary_negate_string($2->astnode.constant.number);

                 if(!neg_string) {
                   fprintf(stderr, "Error generating negated string (DataConstant)\n");
                   exit(EXIT_FAILURE);
                 }

                 free($2->astnode.constant.number);
                 $2->astnode.constant.number = neg_string;

                 $$ = $2;
               }
;

LhsList:  DataLhs
          {
            $$ = $1;
          }
        | LhsList CM DataLhs
          {
            $3->prevstmt = $1;
            $$ = $3;
          }
;

DataLhs:  Lhs
          {
            $$ = $1;
          }
        | OP Lhs CM UndeclaredName EQ LoopBounds CP
          {
            $6->astnode.forloop.counter = $4;
            $6->astnode.forloop.Label = $2;
            $$ = $6;
            $2->parent = $$;
            $4->parent = $$;
          }
;

LoopBounds:  Integer CM Integer
             {
               $$ = addnode();
               $1->parent = $$;
               $3->parent = $$;
               $$->nodetype = DataImpliedLoop;
               $$->astnode.forloop.start = $1;
               $$->astnode.forloop.stop = $3;
               $$->astnode.forloop.incr = NULL;
             }
           | Integer CM Integer CM Integer
             {
               $$ = addnode();
               $1->parent = $$;
               $3->parent = $$;
               $5->parent = $$;
               $$->nodetype = DataImpliedLoop;
               $$->astnode.forloop.start = $1;
               $$->astnode.forloop.stop = $3;
               $$->astnode.forloop.incr = $5;
             }
;

/*  Here is where the fun begins.  */

/*  No newline token here.  Newlines have to be dealt with at 
 *  a lower level.
 */

Statements:    Statement  
               {  
                 $$ = $1; 
               }
             | Statements  Statement 
               { 
                 $2->prevstmt = $1; 
                 $$ = $2; 
               }
;

Statement:    Assignment  NL /* NL has to be here because of parameter dec. */
              {
                $$ = $1;
                $$->nodetype = Assignment;   
              }
            | Call
              {
                $$ = $1;
                $$->nodetype = Call;
              }
            | StmtLabelAssign
              {
                $$ = $1;
                $$->nodetype = StmtLabelAssign;
              }
            | Logicalif
              {
                $$ = $1;
                $$->nodetype = Logicalif;
              }
            | Arithmeticif
              {
                $$ = $1;
                $$->nodetype = Arithmeticif;
              }
            | Blockif
              {
                $$ = $1;
                $$->nodetype = Blockif;
              }
            | Doloop
              {
                $$ = $1;
                $$->nodetype = Forloop;
              }
            | Return
              {
                $$ = $1;
                $$->nodetype = Return;
              }
            | AssignedGoto
              {
                $$ = $1;
                $$->nodetype = AssignedGoto;
              }
            | ComputedGoto
              {
                $$ = $1;
                $$->nodetype = ComputedGoto;
              }
            | Goto
              {
                $$ = $1;
                $$->nodetype = Goto;
              }
            | Label
              {
                $$ = $1;
                $$->nodetype = Label;
              }
            | EndDo
              {
                $$ = $1;
                $$->nodetype = Label;
              }
            | Continue
              {
                $$ = $1;
                $$->nodetype = Label;
              }
            | Write
              {
                $$ = $1;
                $$->nodetype = Write;
              }
            | Read
              {
                $$ = $1;
                $$->nodetype = Read;
              }
            | Stop
              {
                $$ = $1;
                $$->nodetype = Stop;
              }
            | Pause
              {
                $$ = $1;
                $$->nodetype = Pause;
              }
            | Open
              {
                $$ = $1;
              }
            | Close
              {
                $$ = $1;
              }
            | Comment
              {
                $$ = $1;
                $$->nodetype = Comment;
              }
            | Rewind
              {
                $$ = $1;
                $$->nodetype = Unimplemented;
              }
            | DUMMY
              {
                $$ = addnode();
                $$->token = COMMENT;
                $$->nodetype = Comment;
                $$->astnode.ident.len = 0;
                strcpy($$->astnode.ident.name, "dummy stmt\n");
              }
;           

Comment: COMMENT NL
         {
           if(debug)
             printf("Comment -> COMMENT NL\n");

           $$ = addnode();
           $$->token = COMMENT;
           $$->nodetype = Comment;
           $$->astnode.ident.len = 0;

           /* check global var from lexer */
           if(comment_buffer[0]) {
             int idx, len;

             idx = len = 0;

             for(idx = 0; comment_buffer[idx]; idx++)
               len += strlen(comment_buffer[idx]);

             /* if there is only one line of comments buffered, then just copy
              * it to ident.name (if it's small enough to fit).  that will allow
              * the comment to be emitted on one line with the '//' comment marker
              * rather than three lines with the block style comment markers.
              */
             if((idx == 1) && (len < MAX_CONST_LEN)) {
               strcpy($$->astnode.ident.name, comment_buffer[0]);
             }
             else {
               if(len > 0) {
                 $$->astnode.ident.buffered_comments = (char *) f2jalloc(len+1);
                 $$->astnode.ident.buffered_comments[0] = '\0';

                 for(idx = 0; comment_buffer[idx]; idx++)
                   strcat($$->astnode.ident.buffered_comments, comment_buffer[idx]);
               }
             }

             for(idx = 0; comment_buffer[idx]; idx++) {
               f2jfree(comment_buffer[idx], strlen(comment_buffer[idx])+1);
               comment_buffer[idx] = 0;
             }
           }
           else
             strcpy($$->astnode.ident.name, yylval.lexeme);
         }
;

Open: OPEN OP Olist CP NL
      {
        $$ = addnode();
        $3 = switchem($3);

        get_info_from_olist($$, $3);

        /* everything is optional except for the Unit number, so check
         * whether it was specified.
         */
        if(!$$->astnode.open.unit_expr) {
          yyerror("ERROR: OPEN statement has no unit specifier");
          exit(EXIT_FAILURE);
        }
      }
;

Cilist: Cilist CM CilistItem
       {
         $3->prevstmt = $1;
         $$ = $3;
       }
     | CilistItem
       {
         $$ = $1;
       }
;

CilistItem: UnitExp
           {
             $$ = $1;
           }
         | ErrExp
           {
             $$ = $1;
           }
         | RecExp
           {
             $$ = $1;
           }
         | IostatExp
           {
             $$ = $1;
           }
         | FormatOrUnknownSpec
           {
             $$ = $1;
           }
         | EndExp
           {
             $$ = $1;
           }
;

Olist: Olist CM OlistItem
       {
         $3->prevstmt = $1;
         $$ = $3;
       }
     | OlistItem
       {
         $$ = $1;
       }
;

OlistItem: UnitExp
           {
             $$ = $1;
           }
         | Exp
           {
             /* this is for a unit specifier without the "UNIT=" */
             $$ = addnode();
             $$->token = IOSPEC_UNIT;
             $$->nodetype = UnitExp;
             $$->astnode.expression.rhs = $1;
           }
         | IostatExp
           {
             $$ = $1;
           }
         | ErrExp 
           {
             $$ = $1;
           }
         | OpenFileSpec
           {
             $$ = $1;
           }
         | StatusExp
           {
             $$ = $1;
           }
         | AccessExp 
           {
             $$ = $1;
           }
         | FormExp
           {
             $$ = $1;
           }
         | ReclExp
           {
             $$ = $1;
           }
         | BlankExp
           {
             $$ = $1;
           }
;

Cllist: Cllist CM CllistItem
       {
         $3->prevstmt = $1;
         $$ = $3;
       }
     | CllistItem
       {
         $$ = $1;
       }
;

CllistItem: UnitExp
           {
             $$ = $1;
           }
         | Exp
           {
             /* this is for a unit specifier without the "UNIT=" */
             $$ = addnode();
             $$->token = IOSPEC_UNIT;
             $$->nodetype = UnitExp;
             $$->astnode.expression.rhs = $1;
           }
         | IostatExp
           {
             $$ = $1;
           }
         | ErrExp
           {
             $$ = $1;
           }
         | StatusExp
           {
             $$ = $1;
           }
;

UnitExp: IOSPEC_UNIT Exp
         {
           $$ = addnode();
           $$->token = IOSPEC_UNIT;
           $$->nodetype = UnitExp;
           $$->astnode.expression.rhs = $2;
         }
       | IOSPEC_UNIT STAR
         {
           $$ = addnode();
           $$->token = IOSPEC_UNIT;
           $$->nodetype = UnitExp;
           $$->astnode.expression.rhs = addnode();
           $$->astnode.expression.rhs->token = STAR;
           $$->astnode.expression.rhs->nodetype = Constant;
           $$->astnode.expression.rhs->astnode.constant.number =
               strdup("*");
         }
;

OpenFileSpec: IOSPEC_FILE CharExp
          {
            $$ = addnode();
            $$->token = IOSPEC_FILE;
            $$->nodetype = OpenFileSpec;
            $$->astnode.expression.rhs = $2;
          }
;

ReclExp: IOSPEC_RECL Exp
         {
           $$ = addnode();
           $$->token = IOSPEC_RECL;
           $$->nodetype = ReclExp;
           $$->astnode.expression.rhs = $2;
         }
;

RecExp: IOSPEC_REC Exp
         {
           $$ = addnode();
           $$->token = IOSPEC_REC;
           $$->nodetype = RecExp;
           $$->astnode.expression.rhs = $2;
         }
;

StatusExp: IOSPEC_STATUS CharExp
           {
             $$ = addnode();
             $$->token = IOSPEC_STATUS;
             $$->nodetype = StatusExp;
             $$->astnode.expression.rhs = $2;
           }
;

AccessExp: IOSPEC_ACCESS CharExp
           {
             $$ = addnode();
             $$->token = IOSPEC_ACCESS;
             $$->nodetype = AccessExp;
             $$->astnode.expression.rhs = $2;
           }
;

FormExp: IOSPEC_FORM CharExp
         {
           $$ = addnode();
           $$->token = IOSPEC_FORM;
           $$->nodetype = FormExp;
           $$->astnode.expression.rhs = $2;
         }
;

BlankExp: IOSPEC_BLANK CharExp
         {
           $$ = addnode();
           $$->token = IOSPEC_BLANK;
           $$->nodetype = BlankExp;
           $$->astnode.expression.rhs = $2;
         }
;

ErrExp: IOSPEC_ERR Integer
         {
           $$ = addnode();
           $$->token = IOSPEC_ERR;
           $$->nodetype = ErrExp;
           $$->astnode.expression.rhs = $2;
         }
;

EndExp: IOSPEC_END Integer
         {
           $$ = addnode();
           $$->token = IOSPEC_END;
           $$->nodetype = EndExp;
           $$->astnode.expression.rhs = $2;
         }
;

CharExp: UndeclaredName
         {
           $$ = $1;
         }
       | String
         {
           $$ = $1;
         }
;

IostatExp: IOSPEC_IOSTAT Lhs
           {
             $$ = addnode();
             $$->token = IOSPEC_IOSTAT;
             $$->nodetype = IostatExp;
             $$->astnode.expression.rhs = $2;
           }
;

Close: CLOSE OP Cllist CP NL
       {
         $$ = addnode();
         $$->nodetype = Close;

         $3 = switchem($3);

         get_info_from_cllist($$, $3);

         /* everything is optional except for the Unit number, so check
          * whether it was specified.
          */
         if(!$$->astnode.close.unit_expr) {
           yyerror("ERROR: CLOSE statement has no unit specifier");
           exit(EXIT_FAILURE);
         }
       }
;

Rewind: REWIND UndeclaredName NL
        {
          fprintf(stderr,"Warning: REWIND not implemented.\n");
          $$ = $2;
        }
;

End: END  NL 
     {
       $$ = addnode();
       $$->token = END;
       $$->nodetype = End;
     }
   |
     Integer END NL
     {
       AST *end_temp;

       end_temp = addnode();
       end_temp->token = END;
       end_temp->nodetype = End;

       $$ = addnode();
       end_temp->parent = $$;
       $$->nodetype = Label;
       $$->astnode.label.number = atoi($1->astnode.constant.number);
       $$->astnode.label.stmt = end_temp;
       free_ast_node($1);
     }
;

/* 
 * We have to load up a symbol table here with the names of all the
 * variables that are passed in as arguments to our function or
 * subroutine.  Also need to pass `namelist' off to a procedure
 * to load a local variable table for opcode generation.   
 *
 * i inlined the call to init_tables() because when parsing the
 * argument list, if some arg matched a name previously defined as
 * a PARAMETER in some other program unit, then arg_table_load()
 * would catch that and assume that the Name represented a paramter
 * and reinitialize the node as if it were a constant.  kgs 7/26/00
 */

Functionargs:   OP {init_tables();} Namelist CP   
                {
                  if(debug){
                     printf("Functionargs -> OP Namelist CP\n");
                  }
                  $3 = switchem($3);
                  arg_table_load($3);
                  $$ = $3;
                }
              | OP CP
                {
                  if(debug){
                     printf("Functionargs -> OP Namelist CP\n");
                  }
                  init_tables();
                  $$ = NULL;
                }
;


Namelist:   Name  
            {
              if(debug){
                printf("Namelist -> Name\n");
              }
              $$=$1;
            }
          | Namelist CM Name 
            {
              if(debug){
                printf("Namelist -> Namelist CM Name\n");
              }
              $3->prevstmt = $1; 
              $$ = $3;
            }
;

/* 
 *  Somewhere in the actions associated with this production,
 * I need to ship off the type and variable list to get hashed.
 * Also need to pass `typevarlist' off to a procedure
 * to load a local variable table for opcode generation.
 */

Typestmt:  ArithTypes ArithTypevarlist NL
           {
             $$ = process_typestmt($1, $2);
           }
        |  CharTypes CharTypevarlist NL
           {
             $$ = process_typestmt($1, $2);
           }
;

ArithTypes:  ArithSimpleType 
             {
               $$ = $1;
               len = 1;
             }
          |  ArithSimpleType Star Integer
             {
               $$ = $1;
               len = atoi($3->astnode.constant.number);
               free_ast_node($2);
               free_ast_node($3);
             }
;

ArithSimpleType:  ARITH_TYPE
                  { 
                    $$ = yylval.type;
                    typedec_context = $$;
                  }
;

CharTypes:  CharSimpleType
            {
              $$ = $1;
              len = 1;
            }
         |  CharSimpleType Star Integer
            {
              $$ = $1;
              len = atoi($3->astnode.constant.number);
              free_ast_node($2);
              free_ast_node($3);
            }
         |  CharSimpleType Star OP Star CP
            {
              $$ = $1;
              len = -1;
              free_ast_node($2);
              free_ast_node($4);
            }
;

CharSimpleType:  CHAR_TYPE
                 {
                   $$ = yylval.type;
                   typedec_context = $$;
                 }
;

AnySimpleType: ArithSimpleType
               {
                 $$ = $1;
               }
             | CharSimpleType
               {
                 $$ = $1;
               }
;

AnyTypes: ArithTypes
          {
            $$ = $1;
          }
        | CharTypes
          {
            $$ = $1;
          }
;

/* Here I'm going to do the same thing I did with Explist.  That is,
 * each element in the list of typevars will have a parent link to a 
 * single node indicating that the context of the array is a
 * declaration.  --Keith 
 */

ArithTypevarlist: ArithTypevar
                  {
                    $1->parent = addnode();
                    $1->parent->nodetype = Typedec;

                    $$ = $1;
                  }
               |  ArithTypevarlist CM  ArithTypevar
                  {
                    $3->prevstmt = $1;
                    $3->parent = $1->parent;
                    $$ = $3;
                  }
;

ArithTypevar:   Name 
                {
                  $$ = $1;
                  $$->astnode.ident.len = -1;
                }
              | Name Star Integer
                {
                  $$ = $1;
                  $$->astnode.ident.len = atoi($3->astnode.constant.number);
                }
              | Arraydeclaration 
                {
                  $$ = $1;
                  $$->astnode.ident.len = -1;
                }
;

CharTypevarlist: CharTypevar
                 {
                   $1->parent = addnode();
                   $1->parent->nodetype = Typedec;

                   $$ = $1;
                 }
              |  CharTypevarlist CM  CharTypevar
                 {
                   $3->prevstmt = $1;
                   $3->parent = $1->parent;
                   $$ = $3;
                 }
;

CharTypevar:   Name
               {
                 $$ = $1;
                 $$->astnode.ident.len = -1;
               }
             | Name Star Integer
               {
                 $$ = $1;
                 $$->astnode.ident.len = atoi($3->astnode.constant.number);
               }
             | Name Star OP Star CP
               {
                 $$ = $1;
                 $$->astnode.ident.len = -1;
               }
             | Arraydeclaration
               {
                 $$ = $1;
                 $$->astnode.ident.len = -1;
               }
;

/*  Deleted the Type REAL hack...  Need to take care of that in the 
 *  lexer.  This CHAR and STRING stuff is in the wrong place and
 *  needs to get axed.  Putting the TYPE back in ...
 *        ^^^^^^^^^^^ it is commented out for now 9-12-97, Keith
 *                 moved to 'Constant' production 9-17-97, Keith
 */

/*
 *  Might have to explicitly set the arraydeclist pointer to
 *  NULL in this action.  `Name' gets pointed to by the node
 *  that carries the array information.
 */

Name:    NAME  
         {
           HASHNODE *hashtemp;

           lowercase(yylval.lexeme);

           if(type_lookup(java_keyword_table,yylval.lexeme))
             yylval.lexeme[0] = toupper(yylval.lexeme[0]);


           /* check if the name we're looking at is defined as a parameter.
            * if so, instead of inserting an Identifier node here, we're just
            * going to insert the Constant node that corresponds to
            * the parameter.  normally the only time we'd worry about
            * such a substitution would be when the ident was the lhs
            * of some expression, but that should not happen with parameters.
            *
            * otherwise, if not a parameter, get a new AST node initialized
            * with this name.
            *
            * added check for null parameter table because this Name could
            * be reduced before we initialize the tables.  that would mean
            * that this name is the function name, so we dont want this to
            * be a parameter anyway.  kgs 11/7/00
            * 
            */
          

           if((parameter_table != NULL) &&
              ((hashtemp = type_lookup(parameter_table,yylval.lexeme)) != NULL))
           {
             /* had a problem here just setting $$ = hashtemp->variable
              * when there's an arraydec with two of the same PARAMETERS
              * in the arraynamelist, e.g. A(NMAX,NMAX).   so, instead we
              * just copy the relevant fields from the constant node.
              */
             if(debug)
               printf("not calling init name, param %s\n", yylval.lexeme);
             $$ = addnode();
             $$->nodetype = hashtemp->variable->nodetype;
             $$->vartype = hashtemp->variable->vartype;
             $$->token = hashtemp->variable->token;
             $$->astnode.constant.number = 
                strdup(hashtemp->variable->astnode.constant.number);
           }
           else{
             if(debug)
               printf("Name -> NAME\n");
             $$ = initialize_name(yylval.lexeme);
           }
         }
;

/* 
 * UndeclaredName is similar to Name except that it is used in
 * contexts where the name is not actually going to be a declared
 * variable.  Thus in Name, we can insert implicitly defined variables
 * into the hash table, but here in UndeclaredName we do not.
 */

UndeclaredName: NAME
                {
                  lowercase(yylval.lexeme);

                  $$=addnode();
                  $$->token = NAME;
                  $$->nodetype = Identifier;

                  $$->astnode.ident.needs_declaration = FALSE;

                  if(omitWrappers)
                    $$->astnode.ident.passByRef = FALSE;

                  if(type_lookup(java_keyword_table,yylval.lexeme))
                    yylval.lexeme[0] = toupper(yylval.lexeme[0]);

                  strcpy($$->astnode.ident.name, yylval.lexeme);
                }
;

UndeclaredNamelist:   UndeclaredName
            {
              $$=$1;
            }
          | UndeclaredNamelist CM UndeclaredName
            {
              $3->prevstmt = $1;
              $$ = $3;
            }
;

String:  STRING
         {
           $$=addnode();
           $$->token = STRING;
           $$->nodetype = Constant;
           $$->astnode.constant.number = strdup(yylval.lexeme);

           $$->vartype = String;
           if(debug)
             printf("**The string value is %s\n",$$->astnode.constant.number);
         }
       | CHAR
         {
           $$=addnode();
           $$->token = STRING;
           $$->nodetype = Constant;
           $$->astnode.constant.number = strdup(yylval.lexeme);

           $$->vartype = String;
           if(debug)
             printf("**The char value is %s\n",$$->astnode.constant.number);
         }
;

Arraydeclaration: Name OP Arraynamelist CP 
                  {
                    $$ = process_array_declaration($1, $3);
                  }
;

Arraynamelist:    Arrayname 
                  {
                    AST *temp;

                    temp = addnode();
                    temp->nodetype = ArrayDec;
                    $1->parent = temp;
                    if($1->nodetype == ArrayIdxRange) {
                      $1->astnode.expression.lhs->parent = temp;
                      $1->astnode.expression.rhs->parent = temp;
                    }

                    $$=$1;
                  }
                | Arraynamelist CM Arrayname 
                  {
                    $3->prevstmt = $1; 
                    $3->parent = $1->parent;
                    if($3->nodetype == ArrayIdxRange) {
                      $3->astnode.expression.lhs->parent = $1->parent;
                      $3->astnode.expression.rhs->parent = $1->parent;
                    }
                    $$ = $3;
                  }
;

Arrayname: Exp 
           {
             $$ = $1; 
           }
         | Star 
           {
             $$=$1;
           }
         | Exp COLON Exp 
           {
             $$ = addnode();
             $$->nodetype = ArrayIdxRange;
             $$->astnode.expression.lhs = $1;
             $$->astnode.expression.rhs = $3;
           }
;

/*  We reduce STAR here, make changes in the Binaryops
 *  reductions for that.  This handles the fortran array
 *  declaration, e.g., array(*).  
 */

Star:  STAR 
       {
         $$=addnode();
         $$->token = STAR;
         $$->nodetype = Identifier;
        *$$->astnode.ident.name = '*';
       }
;

StmtLabelAssign: ASSIGN Integer TO Name NL
                 {
                   $$ = addnode();
                   $2->parent = $$;
                   $4->parent = $$;
                   $$->nodetype = StmtLabelAssign;
                   $$->astnode.assignment.lhs = $4;
                   $$->astnode.assignment.rhs = $2;

                   /* add this label to the list of assigned labels */

                   if(in_dlist_stmt_label(assign_labels, $2) == 0) {
                     if(debug)
                       printf("inserting label num %s in assign_labels list\n",
                         $2->astnode.constant.number);
                     dl_insert_b(assign_labels, $2);
                   }
                 }
;

/*  At some point, I will need to typecheck the `Name' on the left
 *  hand side of this rule in case it has an array form.  If it looks like
 *  an array, but it isn't in the array table, that's an error. 
 */

Assignment:  Lhs  EQ Exp /* NL (Assignment is also used in the parameter
                          *  declaration, where it is not followed by a NL.
                          */
             { 
                $$ = addnode();
                $1->parent = $$; /* 9-4-97 - Keith */
                $3->parent = $$; /* 9-4-97 - Keith */
                $$->nodetype = Assignment;
                $$->astnode.assignment.lhs = $1;
                $$->astnode.assignment.rhs = $3;
             }
;

Lhs:     Name
         {
           $$=$1;
           $$->nextstmt = NULL;
           $$->prevstmt = NULL;
         }
      |  Name OP Arrayindexlist CP
         {
           AST *temp;

           /*   Use the following declaration in case we 
            *   need to switch index order. 
            *
            *   HASHNODE * hashtemp;  
            */

           $$ = addnode();
           $1->parent = $$; /* 9-4-97 - Keith */
           $$->nodetype = Identifier;
           $$->prevstmt = NULL;
           $$->nextstmt = NULL;

           free_ast_node($3->parent);
           for(temp = $3; temp != NULL; temp = temp->prevstmt)
             temp->parent = $$;

           strcpy($$->astnode.ident.name, $1->astnode.ident.name);

           /*  This is in case we want to switch index order later.
            *
            *  hashtemp = type_lookup(array_table, $1->astnode.ident.name);
            *  if(hashtemp)
            *    $$->astnode.ident.arraylist = $3;
            *  else
            *    $$->astnode.ident.arraylist = switchem($3);
            */

           /* We don't switch index order.  */

           $$->astnode.ident.arraylist = switchem($3);
           free_ast_node($1);
         }
      |  SubstringOp
         {
           $$ = $1;
         }
;

Arrayindexlist:   Exp 
                  { 
                    $1->parent = addnode();
                    $1->parent->nodetype = Identifier;

                    $$ = $1;
                  }
                | Arrayindexlist CM Exp
                  {
                    $3->prevstmt = $1;
                    $3->parent = $1->parent;
		    $$ = $3;
		  }
;

/*  New do loop productions.  Entails rewriting in codegen.c
 *  to emit java source code.  
 */

Doloop:   Do_incr Do_vals
          {
            $$ = $2;
            $$->nodetype = Forloop;
            $$->astnode.forloop.Label = $1;
          }
;


Do_incr:  DO Integer 
          { 
            $$ = $2;
          } 

        | DO Integer CM 
          { 
            $$ = $2;
          }
        | DO 
          {
            char *loop_label;

            loop_label = (char *)malloc(32);
            if(!loop_label) {
              fprintf(stderr,"Malloc error\n");
              exit(EXIT_FAILURE);
            }
            sprintf(loop_label,"%d", cur_do_label);
            cur_do_label++;

            $$ = addnode();
            $$->token = INTEGER;
            $$->nodetype = Constant;
            $$->astnode.constant.number = strdup(loop_label);
            $$->vartype = Integer;

            dl_insert_b(do_labels, strdup($$->astnode.constant.number));

            free(loop_label);
          }
;


Do_vals:  Assignment CM Exp   NL
          {
            AST *counter;

            $$ = addnode();
	    $1->parent = $$; /* 9-4-97 - Keith */
	    $3->parent = $$; /* 9-4-97 - Keith */
            counter = $$->astnode.forloop.counter = $1->astnode.assignment.lhs;
            $$->astnode.forloop.start = $1;
            $$->astnode.forloop.stop = $3;
            $$->astnode.forloop.incr = NULL;
            $$->astnode.forloop.iter_expr = gen_iter_expr($1->astnode.assignment.rhs,$3,NULL);
            $$->astnode.forloop.incr_expr = gen_incr_expr(counter,NULL);
          }
       | Assignment CM Exp CM Exp   NL
         {
           AST *counter;

           $$ = addnode();
	   $1->parent = $$; /* 9-4-97 - Keith */
	   $3->parent = $$; /* 9-4-97 - Keith */
	   $5->parent = $$; /* 9-4-97 - Keith */
           counter = $$->astnode.forloop.counter = $1->astnode.assignment.lhs;
           $$->nodetype = Forloop;
           $$->astnode.forloop.start = $1;
           $$->astnode.forloop.stop = $3;
           $$->astnode.forloop.incr = $5;
           $$->astnode.forloop.iter_expr = gen_iter_expr($1->astnode.assignment.rhs,$3,$5);
           $$->astnode.forloop.incr_expr = gen_incr_expr(counter,$5);
         }
;

/* 
 * changed the Label production to allow any statement to have
 * a line number.   -- keith
 */
Label: Integer Statement
       {
         $$ = addnode();
         $1->parent = $$;
         $2->parent = $$;
         $$->nodetype = Label;
         $$->astnode.label.number = atoi($1->astnode.constant.number);
         $$->astnode.label.stmt = $2;
         free_ast_node($1);
       }
     | Integer Format NL 
       {
         /* HASHNODE *newnode; */
         char *tmpLabel;

         tmpLabel = (char *) f2jalloc(10); /* plenty of space for a f77 label num */

         /* newnode = (HASHNODE *) f2jalloc(sizeof(HASHNODE)); */

         $$ = addnode();
         $1->parent = $$;
         $2->parent = $$;
         $$->nodetype = Format;
         $$->astnode.label.number = atoi($1->astnode.constant.number);
         $$->astnode.label.stmt = $2;
         $2->astnode.label.number = $$->astnode.label.number;
         if(debug)
           printf("@@ inserting format line num %d\n",$$->astnode.label.number);

         sprintf(tmpLabel,"%d",$2->astnode.label.number);

         type_insert(format_table,$2,0,tmpLabel);
         free_ast_node($1);
       }
;

/*  The following productions for FORMAT parsing are derived
 *  from Robert K. Moniot's grammar (see ftnchek-2.9.4) 
 */

Format: FORMAT OP FormatExplist CP
       {
         $$ = addnode();
         $$->nodetype = Format;
         $$->astnode.label.stmt = switchem($3);
       }
;

FormatExplist:   FormatExp
           {
             AST *temp;

             temp = addnode();
             temp->nodetype = Format;
             $1->parent = temp;

             $$ = $1;
           }
         | FormatExplist FormatExp
           {
             $1->nextstmt = $2;
             $2->prevstmt = $1;
             $2->parent = $1->parent;
             if(($2->token == REPEAT) && ($1->token == INTEGER)) {
               $2->astnode.label.number = atoi($1->astnode.constant.number);

               if(debug)
                 printf("## setting number = %s\n", $1->astnode.constant.number);
             }
             if(debug) {
               if($2->token == REPEAT)
                 printf("## $2 is repeat token, $1 = %s ##\n",tok2str($1->token));
               if($1->token == REPEAT)
                 printf("## $1 is repeat token, $2 = %s ##\n",tok2str($2->token));
             }
             $$ = $2;
           }
;

FormatExp:  
       RepeatableItem
       {
         $$ = $1;
       }
     | UnRepeatableItem 
       {
         $$ = $1;
       }
     | FormatSeparator
       {
         $$ = $1;
       }
;

RepeatableItem:  EDIT_DESC  /* A, F, I, D, G, E, L, X */
       {
         $$ = addnode();
         $$->token = EDIT_DESC;
         strcpy($$->astnode.ident.name, yylval.lexeme);
       }
     | UndeclaredName
       {
         $$ = $1;
       }
     | UndeclaredName '.' Constant
       {
         /* ignore the constant part for now */
         free_ast_node($3);

         $$ = $1;
       }
     | OP FormatExplist CP
       {
         $$ = addnode();
         $$->token = REPEAT;
         $$->astnode.label.stmt = switchem($2);
         if(debug)
           printf("## setting number = 1\n");
         $$->astnode.label.number = 1;
       }
;

UnRepeatableItem:  String
       {
         $$ = $1;
       }
     | RepeatSpec
       {
         $$ = $1;
       }
;

FormatSeparator:
       CM
       {
         $$ = addnode();
         $$->token = CM;
       }
     | DIV
       {
         $$ = addnode();
         $$->token = DIV;
       }
     | CAT   /* CAT is two DIVs "//" */
       {
         $$ = addnode();
         $$->token = CAT;
       }
     | COLON
       {
         $$ = addnode();
         $$->token = COLON;
       }
;

RepeatSpec:  Integer
       {
         $$ = $1;
       }
     | PLUS Integer
       {
         $$ = $2;
       }
/*
  this will stay commented out until I know the
meaning of a negative repeat specification.

     | MINUS Integer
       {
         $$ = $1;
       }
*/
;

Continue:  Integer CONTINUE NL
       {
         $$ = addnode();
	 $1->parent = $$; /* 9-4-97 - Keith */
	 $$->nodetype = Label;
	 $$->astnode.label.number = atoi($1->astnode.constant.number);
	 $$->astnode.label.stmt = NULL;
         free_ast_node($1);
       }
;

EndDo:  ENDDO NL
        {
          char *loop_label;

          $$ = addnode();
          $$->nodetype = Label;

          loop_label = (char *)dl_pop(do_labels);

          $$->astnode.label.number = atoi(loop_label);
          $$->astnode.label.stmt = NULL;
        }
;

Write: WRITE OP Cilist CP IoExplist NL
       {
         AST *temp;

         $$ = addnode();
         $$->nodetype = Write;

         $$->astnode.io_stmt.arg_list = switchem($5);
         $$->astnode.io_stmt.io_type = Write;

         $3 = switchem($3);
         get_info_from_cilist($$, $3);

         if($$->astnode.io_stmt.end_num >= 0) {
           yyerror("ERROR: END specifier not allowed in WRITE statment");
           exit(EXIT_FAILURE);
         }

         if($$->astnode.io_stmt.arg_list && $$->astnode.io_stmt.arg_list->parent)
           free_ast_node($$->astnode.io_stmt.arg_list->parent);

         for(temp=$$->astnode.io_stmt.arg_list;temp!=NULL;temp=temp->nextstmt)
           temp->parent = $$;
       }
     | PRINT Integer PrintIoList NL
       {
         AST *temp;

         $$ = addnode();
         $$->astnode.io_stmt.io_type = Write;
         $$->astnode.io_stmt.fmt_list = NULL;

         $$->astnode.io_stmt.format_num = atoi($2->astnode.constant.number);
         $$->astnode.io_stmt.arg_list = switchem($3);

         for(temp=$$->astnode.io_stmt.arg_list;temp!=NULL;temp=temp->nextstmt)
           temp->parent->nodetype = Write;
         free_ast_node($2);
       }
     | PRINT STAR PrintIoList NL
       {
         AST *temp;

         $$ = addnode();
         $$->astnode.io_stmt.io_type = Write;
         $$->astnode.io_stmt.fmt_list = NULL;

         $$->astnode.io_stmt.format_num = -1;
         $$->astnode.io_stmt.arg_list = switchem($3);
           
         for(temp=$$->astnode.io_stmt.arg_list;temp!=NULL;temp=temp->nextstmt)
           temp->parent->nodetype = Write;
       }
     | PRINT String PrintIoList NL
       {
         AST *temp;

         $$ = addnode();
         $$->astnode.io_stmt.io_type = Write;
         $$->astnode.io_stmt.fmt_list = $2;

         $$->astnode.io_stmt.format_num = -1;
         $$->astnode.io_stmt.arg_list = switchem($3);

         for(temp=$$->astnode.io_stmt.arg_list;temp!=NULL;temp=temp->nextstmt)
           temp->parent->nodetype = Write;
       }
;

PrintIoList: CM IoExplist
             {
               $$ = $2;
             }
           | /* empty */
             {
               $$ = NULL;
             }
;

FormatOrUnknownSpec:
       IOSPEC_FMT Integer
        {
          $$ = addnode();
          $$->token = IOSPEC_FMT;
          $$->nodetype = FormatOrUnknownSpec;
          $$->astnode.expression.rhs = $2;
        }
     | Exp
        {
          $$ = addnode();
          $$->token = IOSPEC_EMPTY;
          $$->nodetype = FormatOrUnknownSpec;
          $$->astnode.expression.rhs = $1;
        }
     | IOSPEC_FMT STAR
        {
          $$ = addnode();
          $$->token = IOSPEC_FMT;
          $$->nodetype = FormatOrUnknownSpec;
          $$->astnode.expression.rhs = addnode();
          $$->astnode.expression.rhs->token = STAR;
          $$->astnode.expression.rhs->nodetype = Constant;
          $$->astnode.expression.rhs->astnode.constant.number =
               strdup("*");
        }
     | STAR
        {
          $$ = addnode();
          $$->token = IOSPEC_EMPTY;
          $$->nodetype = FormatOrUnknownSpec;
          $$->astnode.expression.rhs = addnode();
          $$->astnode.expression.rhs->token = STAR;
          $$->astnode.expression.rhs->nodetype = Constant;
          $$->astnode.expression.rhs->astnode.constant.number =
               strdup("*");
        }
     | IOSPEC_FMT String
        {
          $$ = addnode();
          $$->token = IOSPEC_FMT;
          $$->nodetype = FormatOrUnknownSpec;
          $$->astnode.expression.rhs = $2;
        }
     | IOSPEC_FMT UndeclaredName
        {
          fprintf(stderr,"Warning - ignoring FMT = %s\n",
             $2->astnode.ident.name);
          $$ = addnode();
          $$->token = IOSPEC_FMT;
          $$->nodetype = FormatOrUnknownSpec;
          $$->astnode.expression.rhs = $2;
        }
;

Read: READ OP Cilist CP IoExplist NL
      {
         AST *temp;

         $$ = addnode();
         $$->nodetype = Read;

         $$->astnode.io_stmt.arg_list = switchem($5);
         $$->astnode.io_stmt.io_type = Read;

         $3 = switchem($3);
         get_info_from_cilist($$, $3);

         if($$->astnode.io_stmt.arg_list && $$->astnode.io_stmt.arg_list->parent)
           free_ast_node($$->astnode.io_stmt.arg_list->parent);

         for(temp=$$->astnode.io_stmt.arg_list;temp!=NULL;temp=temp->nextstmt)
           temp->parent = $$;
      }
;

IoExplist: IoExp
           {
             $1->parent = addnode();
             $1->parent->nodetype = IoExplist;

             $$ = $1;
           }
         | IoExplist CM IoExp
           {
             $3->prevstmt = $1;
             $3->parent = $1->parent;
             $$ = $3;
           }
         | /* empty - should this be allowed for READ? */
           {
             $$ = NULL;
           }
;

IoExp: Exp
       {
         $$ = $1;
       }
     | OP Explist CM Name EQ Exp CM Exp CP /* implied do loop */
       {
         AST *temp;

         $$ = addnode();
         $$->nodetype = IoImpliedLoop;
         $$->astnode.forloop.start = $6;
         $$->astnode.forloop.stop = $8;
         $$->astnode.forloop.incr = NULL;
         $$->astnode.forloop.counter = $4;
         $$->astnode.forloop.Label = switchem($2);
         $$->astnode.forloop.iter_expr = gen_iter_expr($6,$8,NULL);
         $$->astnode.forloop.incr_expr = gen_incr_expr($4,NULL);

         $2->parent = $$;
         for(temp = $2; temp != NULL; temp = temp->nextstmt)
           temp->parent = $$;
         $4->parent = $$;
         $6->parent = $$;
         $8->parent = $$;
       }
     | OP Explist CM Name EQ Exp CM Exp CM Exp CP /* implied do loop */
       {
         AST *temp;

         $$ = addnode();
         $$->nodetype = IoImpliedLoop;
         $$->astnode.forloop.start = $6;
         $$->astnode.forloop.stop = $8;
         $$->astnode.forloop.incr = $10;
         $$->astnode.forloop.counter = $4;
         $$->astnode.forloop.Label = switchem($2);
         $$->astnode.forloop.iter_expr = gen_iter_expr($6,$8,$10);
         $$->astnode.forloop.incr_expr = gen_incr_expr($4,$10);

         $2->parent = $$;
         for(temp = $2; temp != NULL; temp = temp->nextstmt)
           temp->parent = $$;
         $4->parent = $$;
         $6->parent = $$;
         $8->parent = $$;
         $10->parent = $$;
       }
;

/*  Got a problem when a Blockif opens with a Blockif.  The
 *  first statement of the second Blockif doesn't get into the
 *  tree.  Might be able to use do loop for example to fix this. 
 *
 *  --apparently the problem mentioned in the comment above has
 *    been fixed now.
 */

Blockif:   IF OP Exp CP THEN NL IfBlock Elseifs Else EndIf NL
           {
             $$ = addnode();
             $3->parent = $$;
             if($7 != NULL)
               $7->parent = $$; /* 9-4-97 - Keith */
             if($8 != NULL) 
               $8->parent = $$; /* 9-4-97 - Keith */
             if($9 != NULL)
               $9->parent = $$; /* 9-4-97 - Keith */
             $$->nodetype = Blockif;
             $$->astnode.blockif.conds = $3;
             $7 = switchem($7);
             $$->astnode.blockif.stmts = $7;

             /*  If there are any `else if' statements,
              *  switchem. Otherwise, NULL pointer checked
              *  in code generating functions. 
              */
             $8 = switchem($8); 
             $$->astnode.blockif.elseifstmts = $8; /* Might be NULL. */
             $$->astnode.blockif.elsestmts = $9;   /* Might be NULL. */

             $$->astnode.blockif.endif_label = $10->astnode.blockif.endif_label;
           }
;

IfBlock:  /* Empty. */ {$$=0;} /* if block may be null */
        | Statements
          {
             $$ = $1;
          }
;

Elseifs:  /* Empty. */ {$$=0;} /* No `else if' statements, NULL pointer. */
        |  Elseif 
           {
              $$ = $1;
           }
        | Elseifs Elseif 
          {
             $2->prevstmt = $1;
	     $$ = $2;
          } 
;


Elseif: ELSEIF OP Exp CP THEN NL Statements 
        {
          $$=addnode();
	  $3->parent = $$;  
	  $7->parent = $$; /* 9-4-97 - Keith */
	  $$->nodetype = Elseif;
	  $$->astnode.blockif.conds = $3;
	  $$->astnode.blockif.stmts = switchem($7);
        }
;


Else:  /* Empty. */  {$$=0;}  /* No `else' statements, NULL pointer. */
        | ELSE NL  Statements
          {
            $$=addnode();
            $3->parent = $$; /* 9-4-97 - Keith */
            $$->nodetype = Else;
            $$->astnode.blockif.stmts = switchem($3);
          }
        | ELSE NL
          {
            $$ = 0;
          }
;

EndIf: ENDIF 
       {
         if(debug) printf("EndIf\n");
         $$ = addnode();
         $$->nodetype = Blockif;

         if(strlen(yylval.lexeme) > 0)
           $$->astnode.blockif.endif_label = atoi(yylval.lexeme);
         else
           $$->astnode.blockif.endif_label = -1;
       }
;

Logicalif: IF OP Exp CP Statement
           {
             $$ = addnode();
             $3->parent = $$;
             $5->parent = $$; /* 9-4-97 - Keith */
             $$->astnode.logicalif.conds = $3;
             $$->astnode.logicalif.stmts = $5;
           }           
;

Arithmeticif: IF OP Exp CP Integer CM Integer CM Integer NL
              {
                $$ = addnode();
                $$->nodetype = Arithmeticif;
                $3->parent = $$;
                $5->parent = $$;
                $7->parent = $$;
                $9->parent = $$;

                $$->astnode.arithmeticif.cond = $3;
                $$->astnode.arithmeticif.neg_label  = atoi($5->astnode.constant.number);
                $$->astnode.arithmeticif.zero_label = atoi($7->astnode.constant.number);
                $$->astnode.arithmeticif.pos_label  = atoi($9->astnode.constant.number);
                free_ast_node($5);
                free_ast_node($7);
                free_ast_node($9);
              }
;

/* 
 * This _may_ have to be extended to deal with 
 * jasmin opcode.  Variables of type array need 
 * to have their arguments emitted in reverse order 
 * so that java can increment in row instead of column
 * order.  So we look each name up in the array table, 
 * it is in there we leave the argument list reversed, 
 * otherwise, it is a subroutine or function (method) 
 * call and we reverse the arguments.
 *
 * I don't think the above comment makes sense anymore.
 * --kgs 7/2007
 */

Subroutinecall:   Name OP Explist CP
                  {
                    $$ = process_subroutine_call($1, $3);
                  }
;

SubstringOp: Name OP Exp COLON Exp CP
             {
               if(debug)
                 printf("SubString! format = c(e1:e2)\n");
               $$ = addnode();
               $1->parent = $$;
               $3->parent = $$;
               $5->parent = $$;
               strcpy($$->astnode.ident.name, $1->astnode.ident.name);
               $$->nodetype = Substring;
               $$->token = NAME;
               $$->astnode.ident.startDim[0] = $3;
               $$->astnode.ident.endDim[0] = $5;
               free_ast_node($1);
             }
           | Name OP COLON Exp CP
             {
               if(debug)
                 printf("SubString! format = c(:e2)\n");
               $$ = addnode();
               $1->parent = $$;
               $4->parent = $$;
               strcpy($$->astnode.ident.name, $1->astnode.ident.name);
               $$->nodetype = Substring;
               $$->token = NAME;
               $$->astnode.ident.startDim[0] = NULL;
               $$->astnode.ident.endDim[0] = $4;
               free_ast_node($1);
             }
           | Name OP Exp COLON CP
             {
               if(debug)
                 printf("SubString! format = c(e1:)\n");
               $$ = addnode();
               $1->parent = $$;
               $3->parent = $$;
               strcpy($$->astnode.ident.name, $1->astnode.ident.name);
               $$->nodetype = Substring;
               $$->token = NAME;
               $$->astnode.ident.startDim[0] = $3;
               $$->astnode.ident.endDim[0] = NULL;
               free_ast_node($1);
             }
           | Name OP COLON CP
             {
               if(debug)
                 printf("SubString! format = c(:)\n");
               $$ = addnode();
               $1->parent = $$;
               strcpy($$->astnode.ident.name, $1->astnode.ident.name);
               $$->nodetype = Substring;
               $$->token = NAME;
               $$->astnode.ident.startDim[0] = NULL;
               $$->astnode.ident.endDim[0] = NULL;
               free_ast_node($1);
             }
;


/* 
 * What I'm going to try to do here is have each element
 * of the list linked back to a single node through its
 * parent pointer.  This will allow the code generator
 * to check the array context (whether it is being used
 * as part of an external call or part of a call to an
 * intrinsic function or some other use). --Keith 
 */

Explist:   Exp
           {
             AST *temp;

             temp = addnode();
             temp->nodetype = Call;
             $1->parent = temp;

             $$ = $1;
           }
         | Explist CM Exp
           {
             $3->prevstmt = $1;
             $3->parent = $1->parent;
             $$ = $3;
           }
         | /* empty */
           {
             $$ = NULL;
           }
;

/*  This is not exactly right.  There will need to 
 *  be a struct to handle this.
 */
Call:     CALL Subroutinecall  NL
          {
            /* we don't want subroutines in the type_table
             * make a dlist to stuff the names in and check
             * them in initialize_name.
             */
             
             if(in_dlist(subroutine_names, $2->astnode.ident.name)==0){
                if(debug){
                   printf("inserting %s in dlist and del from type\n",
                         $2->astnode.ident.name);
                }
                dl_insert_b(subroutine_names, strdup($2->astnode.ident.name));
                hash_delete(type_table, $2->astnode.ident.name);
             }
             if(debug){
               printf("call: %s\n", $2->astnode.ident.name);
             }

             $$ = $2;
	     $$->nodetype = Call;
          }
       |  CALL UndeclaredName NL
          {
            $$ = addnode();
            $2->parent = $$;
            $$->nodetype = Identifier;
            strcpy($$->astnode.ident.name, $2->astnode.ident.name);
            $$->astnode.ident.arraylist = addnode();
            $$->astnode.ident.arraylist->nodetype = EmptyArgList;
            free_ast_node($2);
          }
;

/* again we borrowed from Moniot's grammar....from the Exp production down to
 * the primary production is from his ftnchek grammar.    --keith  2/17/98.
 */

Exp: log_disjunct
     {
       $$ = $1;
     }
   | Exp EQV log_disjunct
     {
       $$=addnode();
       $1->expr_side = left;
       $3->expr_side = right;
       $1->parent = $$;
       $3->parent = $$;
       $$->token = EQV;
       $$->nodetype = Logicalop;
       $$->astnode.expression.lhs = $1;
       $$->astnode.expression.rhs = $3;
     }
   | Exp NEQV log_disjunct
     {
       $$=addnode();
       $1->expr_side = left;
       $3->expr_side = right;
       $1->parent = $$;
       $3->parent = $$;
       $$->token = NEQV;
       $$->nodetype = Logicalop;
       $$->astnode.expression.lhs = $1;
       $$->astnode.expression.rhs = $3;
     }
;

log_disjunct: log_term
              {
                $$ = $1;
              }
            | log_disjunct OR log_term
              {
                $$=addnode();
		$1->expr_side = left;
		$3->expr_side = right;
		$1->parent = $$;
		$3->parent = $$;
		$$->token = OR;
		$$->nodetype = Logicalop;
		$$->astnode.expression.lhs = $1;
		$$->astnode.expression.rhs = $3;
              }
;
 
log_term: log_factor
          {
            $$ = $1;
          }
        | log_term AND log_factor
          {
            $$=addnode();
            $1->expr_side = left;
            $3->expr_side = right;
            $1->parent = $$;
            $3->parent = $$;
            $$->token = AND;
            $$->nodetype = Logicalop;
            $$->astnode.expression.lhs = $1;
            $$->astnode.expression.rhs = $3;
          }
;

log_factor: log_primary
            {
              $$ = $1;
            }
          | NOT log_primary
            {
              $$=addnode();
              $2->parent = $$;  /* 9-4-97 - Keith */
              $$->token = NOT;
              $$->nodetype = Logicalop;
              $$->astnode.expression.lhs = 0;
              $$->astnode.expression.rhs = $2;
            }
;
 
log_primary: arith_expr
             {
               $$ = $1;
             }
           | log_primary RELOP {temptok = yylval.tok;} log_primary
             {
               $$=addnode();
               $1->expr_side = left;
               $4->expr_side = right;
               $1->parent = $$;
               $4->parent = $$;
               $$->nodetype = Relationalop;
               $$->token = temptok;
               $$->astnode.expression.lhs = $1;
               $$->astnode.expression.rhs = $4;
             }
;

arith_expr: term
            {
              $$ = $1;
            }
          | MINUS term
            {
              if($2->nodetype == Constant) {
                char *neg_string;

                neg_string = unary_negate_string($2->astnode.constant.number);

                if(!neg_string) {
                  fprintf(stderr, "Error generating negated string (arith_expr)\n");
                  exit(EXIT_FAILURE);
                }

                free($2->astnode.constant.number);
                $2->astnode.constant.number = neg_string;

                $$ = $2;
              }
              else {
                $$ = addnode();
                $2->parent = $$;
                $$->astnode.expression.rhs = $2;
                $$->astnode.expression.lhs = 0;
                $$->astnode.expression.minus = '-';   
                $$->nodetype = Unaryop;
                $$->vartype = $2->vartype;
              }
            }
          | PLUS term
            {
              if($2->nodetype == Constant) {
                $$ = $2;
              }
              else {
                $$ = addnode();
                $2->parent = $$;
                $$->astnode.expression.rhs = $2;
                $$->astnode.expression.lhs = 0;
                $$->astnode.expression.minus = '+';
                $$->nodetype = Unaryop;
		  $$->vartype = $2->vartype;
              }
            }
          | arith_expr PLUS term
            {
              $$=addnode();
              $1->expr_side = left;
              $3->expr_side = right;
              $$->token = PLUS;
              $1->parent = $$;
              $3->parent = $$;
              $$->astnode.expression.lhs = $1;
              $$->astnode.expression.rhs = $3;
              $$->vartype = MIN($1->vartype, $3->vartype);
              $$->nodetype = Binaryop;
              $$->astnode.expression.optype = '+';
            }
          | arith_expr MINUS term
            {
              $$=addnode();
              $$->token = MINUS;
              $1->expr_side = left;
              $3->expr_side = right;
              $1->parent = $$;
              $3->parent = $$;
              $$->astnode.expression.lhs = $1;
              $$->astnode.expression.rhs = $3;
              $$->vartype = MIN($1->vartype, $3->vartype);
              $$->nodetype = Binaryop;
              $$->astnode.expression.optype = '-';
            }
;
 
term: factor
      {
        $$ = $1;
      }
    | term DIV factor
      {
        $$=addnode();
        $1->expr_side = left;
        $3->expr_side = right;
        $$->token = DIV;
        $1->parent = $$;
        $3->parent = $$;
        $$->astnode.expression.lhs = $1;
        $$->astnode.expression.rhs = $3;
	 $$->vartype = MIN($1->vartype, $3->vartype);
        $$->nodetype = Binaryop;
        $$->astnode.expression.optype = '/';
      }
    | term STAR factor
      {
        $$=addnode();

        $$->token = STAR;
        $1->expr_side = left;
        $3->expr_side = right;
        $1->parent = $$;
        $3->parent = $$;
        $$->astnode.expression.lhs = $1;
        $$->astnode.expression.rhs = $3;
	 $$->vartype = MIN($1->vartype, $3->vartype);
        $$->nodetype = Binaryop;
        $$->astnode.expression.optype = '*';
      }
;

factor: char_expr
        {
          $$ = $1;
        }
      | char_expr POW factor
        {
          $$=addnode();
          $1->parent = $$;
          $3->parent = $$;
 	  $$->nodetype = Power;
	  $$->astnode.expression.lhs = $1;
	  $$->astnode.expression.rhs = $3;
          $$->vartype = MIN($1->vartype, $3->vartype);
        }
;

char_expr: primary
           {
             $$ = $1;
           }
         | char_expr CAT primary
           {
             $$=addnode();
             $$->token = CAT;
             $1->expr_side = left;
             $3->expr_side = right;
             $1->parent = $$;
             $3->parent = $$;
             $$->astnode.expression.lhs = $1;
             $$->astnode.expression.rhs = $3;
             $$->vartype = MIN($1->vartype, $3->vartype);
             $$->nodetype = Binaryop;
             $$->astnode.expression.optype = '+';
           }
;

primary:  Name {$$=$1;}
          |  Constant
             {
	       $$ = $1;
	     }
   /*       |  Complex {$$=$1;} */
          |  Subroutinecall {$$=$1;}    
          |  SubstringOp {$$=$1;}    
          |  OP Exp CP  
             {
               $$ = addnode();
               $2->parent = $$;   /* 9-4-97 - Keith */
               $$->nodetype = Expression;
               $$->astnode.expression.parens = TRUE;
               $$->astnode.expression.rhs = $2;
               $$->astnode.expression.lhs = NULL;
               $$->vartype = $2->vartype;
             }
;

/*
Complex: OP Constant CM Constant CP {$$=addnode();}
;
*/

/* `TRUE' and `FALSE' have already been typedefed
 * as BOOLEANs.  
 */
Boolean:  TrUE
             {
               $$ = addnode();
               $$->token = TrUE;
               $$->nodetype = Constant;
               $$->astnode.constant.number = strdup("true");
               $$->vartype = Logical;
             }
         | FaLSE
             {
               $$ = addnode();
               $$->token = FaLSE;
               $$->nodetype = Constant;
               $$->astnode.constant.number = strdup("false");
               $$->vartype = Logical;
             }

;

Constant:   
         Integer  
         {
           $$ = $1; 
         }
       | Float
         { 
           $$ = $1; 
         }
       | Double
         { 
           $$ = $1; 
         }
       | Exponential
         { 
           $$ = $1; 
         }
       | Boolean
         { 
           $$ = $1; 
         }
       | String   /* 9-16-97, keith */
         { 
           $$ = $1; 
         }
; 

Integer :     INTEGER 
             {
               if(debug)printf("Integer\n");
               $$ = addnode();
               $$->token = INTEGER;
               $$->nodetype = Constant;
               $$->astnode.constant.number = strdup(yylval.lexeme);
               $$->vartype = Integer;
             }
;

Double:       DOUBLE
             {
               $$ = addnode();
	       $$->token = DOUBLE;
               $$->nodetype = Constant;
               $$->astnode.constant.number = strdup(yylval.lexeme);
               $$->vartype = Double;
             }
;

Float:       FLOAT
             {
               $$ = addnode();
               $$->token = FLOAT;
               $$->nodetype = Constant;
               $$->astnode.constant.number = 
                   (char *)malloc(strlen(yylval.lexeme) + 2);
               strcpy($$->astnode.constant.number, yylval.lexeme);
               strcat($$->astnode.constant.number, "f");
               $$->vartype = Float;
             }
;

               
/*
 * Call exp_to_double() to change the 'D' to 'e' for emitting
 * exponentials in Java source.
 */

Exponential:   E_EXPONENTIAL
             {
               char tempname[60];

               $$ = addnode();
	       $$->token = E_EXPONENTIAL;
               $$->nodetype = Constant;
	       exp_to_double(yylval.lexeme, tempname);
               $$->astnode.constant.number = 
                   (char *)malloc(strlen(tempname) + 2);
               strcpy($$->astnode.constant.number, tempname);
               strcat($$->astnode.constant.number, "f");
               $$->vartype = Float;
             }
           |   D_EXPONENTIAL
             {
               char tempname[60];

               $$ = addnode();
	       $$->token = D_EXPONENTIAL;
               $$->nodetype = Constant;
	       exp_to_double(yylval.lexeme, tempname);
               $$->astnode.constant.number = strdup(tempname);
               $$->vartype = Double;
             }
;

/*  All the easy productions that work go here.  */

Return:      RETURN NL
             {
                $$= addnode();
             }
;

Pause:  PAUSE NL
        {
          $$ = addnode();
          $$->nodetype = Pause;
          $$->astnode.constant.number = strdup("");
        }
      | PAUSE String NL
        {
           $$ = $2;
           $$->nodetype = Pause;
        }
;

Stop:   STOP NL
        {
          $$ = addnode();
          $$->nodetype = Stop;
          $$->astnode.constant.number = strdup("");
        }
      | STOP String NL
        {
           $$ = $2;
           $$->nodetype = Stop;
        }
      | STOP Integer NL
        {
           $$ = $2;
           $$->nodetype = Stop;
        }
;

Goto:   GOTO Integer  NL
        {
          $$ = addnode();
          $2->parent = $$;   /* 9-4-97 - Keith */
          $$->nodetype = Goto;
	  if(debug)
            printf("goto label: %d\n", atoi(yylval.lexeme)); 
          $$->astnode.go_to.label = atoi(yylval.lexeme);
          free_ast_node($2);
        }
;

ComputedGoto:   GOTO OP Intlist CP Exp NL
                {
                  $$ = addnode();
                  $3->parent = $$;   /* 9-4-97 - Keith */
                  $5->parent = $$;   /* 9-4-97 - Keith */
                  $$->nodetype = ComputedGoto;
                  $$->astnode.computed_goto.name = $5;
                  $$->astnode.computed_goto.intlist = switchem($3);
        	  if(debug)
        	    printf("Computed go to,\n");
                }    
              | GOTO OP Intlist CP CM Exp NL
                {
                  $$ = addnode();
                  $3->parent = $$;   /* 9-4-97 - Keith */
                  $6->parent = $$;   /* 9-4-97 - Keith */
                  $$->nodetype = ComputedGoto;
                  $$->astnode.computed_goto.name = $6;
                  $$->astnode.computed_goto.intlist = switchem($3);
        	  if(debug)
        	    printf("Computed go to,\n");
                }    
;

AssignedGoto:   GOTO Name OP Intlist CP NL
                {
                  $$ = addnode();
                  $2->parent = $$;
                  $4->parent = $$;
                  $$->nodetype = AssignedGoto;
                  $$->astnode.computed_goto.name = $2;
                  $$->astnode.computed_goto.intlist = switchem($4);
        	  if(debug)
        	    printf("Assigned go to,\n");
                }    
              | GOTO Name CM OP Intlist CP NL
                {
                  $$ = addnode();
                  $2->parent = $$;
                  $5->parent = $$;
                  $$->nodetype = AssignedGoto;
                  $$->astnode.computed_goto.name = $2;
                  $$->astnode.computed_goto.intlist = switchem($5);
        	  if(debug)
        	    printf("Assigned go to,\n");
                }    
              | GOTO Name NL
                {
                  $$ = addnode();
                  $2->parent = $$;
                  $$->nodetype = AssignedGoto;
                  $$->astnode.computed_goto.name = $2;
                  $$->astnode.computed_goto.intlist = NULL;
        	  if(debug)
        	    printf("Assigned go to (no intlist)\n");
                }    
;

Intlist:   Integer
            {
              $$ = $1;
            }
      | Intlist CM Integer
            {
              $3->prevstmt = $1;
              $$ = $3;
            }
;

Parameter:   PARAMETER OP Pdecs CP NL 
             {
	       $$ = addnode();
               $3->parent = $$;   /* 9-4-97 - Keith */
	       $$->nodetype = Specification;
	       $$->astnode.typeunit.specification = Parameter;
               $$->astnode.typeunit.declist = switchem($3); 
             }
;

Pdecs:    Pdec 
          { 
            $$=$1;
          }
        | Pdecs CM Pdec 
          {
            $3->prevstmt = $1; 
            $$=$3;
          }
;

Pdec:     Assignment
          {
            void add_decimal_point(char *);
            double constant_eval;
            HASHNODE *ht;
            char *cur_id;
            AST *temp;

            if(debug)
              printf("Parameter...\n");

            $$ = $1;
            $$->nodetype = Assignment;

            constant_eval = eval_const_expr($$->astnode.assignment.rhs);

            if(debug) {
              printf("### constant_eval is %.40g\n", constant_eval);
              printf("### constant_eval is %.40e\n", constant_eval);
            }
            
            temp = addnode();
            temp->nodetype = Constant;

            ht = type_lookup(type_table, $$->astnode.assignment.lhs->astnode.ident.name);

            if(ht)
              temp->vartype = ht->variable->vartype;
            else
              temp->vartype = $$->astnode.assignment.rhs->vartype;
            
            switch(temp->vartype) {
              case String:
              case Character:
                temp->token = STRING;
                temp->astnode.constant.number =
                  strdup($$->astnode.assignment.rhs->astnode.constant.number);
                break;
              case Complex:
                fprintf(stderr,"Pdec: Complex not yet supported.\n");
                break;
              case Logical:
                temp->token = $$->astnode.assignment.rhs->token;
                temp->astnode.constant.number =
                   strdup(temp->token == TrUE ? "true" : "false");
                break;
              case Float:
                temp->token = FLOAT;

                temp->astnode.constant.number = (char *)malloc(MAX_CONST_LEN);
                sprintf(temp->astnode.constant.number,"%.40g",constant_eval);
                add_decimal_point(temp->astnode.constant.number);
                strcat(temp->astnode.constant.number, "f");
                
                break;
              case Double:
                temp->token = DOUBLE;

                temp->astnode.constant.number = (char *)malloc(MAX_CONST_LEN);
                sprintf(temp->astnode.constant.number,"%.40g",constant_eval);
                add_decimal_point(temp->astnode.constant.number);
                
                break;
              case Integer:
                temp->token = INTEGER;
                temp->astnode.constant.number = (char *)malloc(MAX_CONST_LEN);
                sprintf(temp->astnode.constant.number,"%d",(int)constant_eval);
                break;
              default:
                fprintf(stderr,"Pdec: bad vartype!\n");
            }

            free_ast_node($$->astnode.assignment.rhs);
            $$->astnode.assignment.rhs = temp;
                                                      
            if(debug)
              printf("### the constant is '%s'\n",
                temp->astnode.constant.number);

            cur_id = strdup($$->astnode.assignment.lhs->astnode.ident.name);

            if(type_lookup(java_keyword_table,cur_id))
              cur_id[0] = toupper(cur_id[0]);

            if(debug)
               printf("insert param_table %s\n", $$->astnode.assignment.lhs->astnode.ident.name);
            hash_delete(type_table, $$->astnode.assignment.lhs->astnode.ident.name);
            type_insert(parameter_table, temp, 0, cur_id);
            free_ast_node($$->astnode.assignment.lhs);
          }
;

External:  EXTERNAL UndeclaredNamelist NL
           {
             $$=addnode(); 
             $2->parent = $$;  /* 9-3-97 - Keith */
             $$->nodetype = Specification;
             $$->token = EXTERNAL;
             $$->astnode.typeunit.declist = switchem($2);
             $$->astnode.typeunit.specification = External;
           }
;

Intrinsic: INTRINSIC UndeclaredNamelist NL
           {
             $$=addnode(); 
             $2->parent = $$;  /* 9-3-97 - Keith */
             $$->nodetype = Specification;
	     $$->token = INTRINSIC;
             $$->astnode.typeunit.declist = switchem($2);
             $$->astnode.typeunit.specification = Intrinsic;
           }
;


%%


/*****************************************************************************
 *                                                                           *
 * yyerror                                                                   *
 *                                                                           *
 * The standard yacc error routine.                                          *
 *                                                                           *
 *****************************************************************************/

void 
yyerror(char *s)
{
  extern Dlist file_stack;
  INCLUDED_FILE *pfile;
  Dlist tmp;

  if(current_file_info)
    printf("%s:%d: %s\n", current_file_info->name, lineno, s);
  else
    printf("line %d: %s\n", lineno, s);

  dl_traverse_b(tmp, file_stack) {
    pfile = (INCLUDED_FILE *)dl_val(tmp);

    printf("\tincluded from: %s:%d\n", pfile->name, pfile->line_num);
  }
}

/*****************************************************************************
 *                                                                           *
 * add_decimal_point                                                         *
 *                                                                           *
 * this is just a hack to compensate for the fact that there's no printf     *
 * specifier that does exactly what we want.  assume the given string        *
 * represents a floating point number.  if there's no decimal point in the   *
 * string, then append ".0" to it.  However, if there's an 'e' in the string *
 * then javac will interpret it as floating point.  The only real problem    *
 * that occurs is when the constant is too big to fit as an integer, but has *
 * no decimal point, so javac flags it as an error (int constant too big).   *
 *                                                                           *
 *****************************************************************************/

void
add_decimal_point(char *str)
{
  BOOL found_dec = FALSE;
  char *p = str;

  while( *p != '\0' ) {
    if( *p == '.' ) {
      found_dec = TRUE;
      break;
    }

    if( *p == 'e' )
      return;
    
    p++;
  }

  if(!found_dec)
    strcat(str, ".0");
}

/*****************************************************************************
 *                                                                           *
 * addnode                                                                   *
 *                                                                           *
 * To keep things simple, there is only one type of parse tree node.         *
 *                                                                           *
 *****************************************************************************/

AST * 
addnode() 
{
  return (AST*)f2jcalloc(1,sizeof(AST));
}


/*****************************************************************************
 *                                                                           *
 * switchem                                                                  *
 *                                                                           *
 * Need to turn the linked list around,                                      *
 * so that it can traverse forward instead of in reverse.                    *
 * What I do here is create a doubly linked list.                            *
 * Note that there is no `sentinel' or `head' node                           *
 * in this list.  It is acyclic and terminates in                            *
 * NULL pointers.                                                            *
 *                                                                           *
 *****************************************************************************/

AST * 
switchem(AST * root) 
{
  if(root == NULL)
    return NULL;

  if (root->prevstmt == NULL) 
    return root;

  while (root->prevstmt != NULL) 
  {
    root->prevstmt->nextstmt = root;
    root = root->prevstmt;
  }

  return root;
}

/*****************************************************************************
 *                                                                           *
 * assign_array_dims                                                         *
 *                                                                           *
 * This is used by DIMENSION and COMMON to set the specified array           *
 * dimensions, possibly in the absence of a type declaration.  If we         *
 * haven't seen a delcaration for this variable yet, create a new node.      *
 * Otherwise, assign the array dimensions to the existing node.              *
 *                                                                           *
 *****************************************************************************/

void
assign_array_dims(AST *var)
{
  HASHNODE *hash_entry;
  AST *node;
  int i;

  hash_entry = type_lookup(type_table, var->astnode.ident.name);
  if(hash_entry)
    node = hash_entry->variable;
  else {
    if(debug){
      printf("Calling initalize name from assign_array_dims\n");
    }

    node = initialize_name(var->astnode.ident.name);

    /* if it's an intrinsic_named array */
    if(node->astnode.ident.which_implicit == INTRIN_NAMED_ARRAY_OR_FUNC_CALL){
       node->astnode.ident.which_implicit = INTRIN_NAMED_ARRAY;
       type_insert(type_table, node, node->vartype, var->astnode.ident.name);
    }

    if(debug)
      printf("assign_array_dims: %s\n", var->astnode.ident.name);
  }

  node->astnode.ident.localvnum = -1;
  node->astnode.ident.arraylist = var->astnode.ident.arraylist;
  node->astnode.ident.dim = var->astnode.ident.dim;
  node->astnode.ident.leaddim = var->astnode.ident.leaddim;
  for(i=0;i<MAX_ARRAY_DIM;i++) {
    node->astnode.ident.startDim[i] = var->astnode.ident.startDim[i];
    node->astnode.ident.endDim[i] = var->astnode.ident.endDim[i];
  }

  /* do the same for the array table */

  hash_entry = type_lookup(array_table, var->astnode.ident.name);
  if(hash_entry)
    node = hash_entry->variable;
  else {
    node = initialize_name(var->astnode.ident.name);
    type_insert(array_table, node, node->vartype, var->astnode.ident.name);
    hash_entry = type_lookup(array_table, var->astnode.ident.name);
    if(hash_entry)
      node = hash_entry->variable;
    else {
      fprintf(stderr, "internal error: lookup failed after insert\n");
      return;
    }
  }

  node->astnode.ident.localvnum = -1;
  node->astnode.ident.arraylist = var->astnode.ident.arraylist;
  node->astnode.ident.dim = var->astnode.ident.dim;
  node->astnode.ident.leaddim = var->astnode.ident.leaddim;
  for(i=0;i<MAX_ARRAY_DIM;i++) {
    node->astnode.ident.startDim[i] = var->astnode.ident.startDim[i];
    node->astnode.ident.endDim[i] = var->astnode.ident.endDim[i];
  }
}

/*****************************************************************************
 *                                                                           *
 * assign_common_array_dims                                                  *
 *                                                                           *
 * For arrays declared in COMMON blocks, we go ahead and assign the          *
 * dimensions in case they aren't dimensioned anywhere else.                 *
 *                                                                           *
 *****************************************************************************/

void
assign_common_array_dims(AST *root)
{
  AST *Clist, *temp;

  for(Clist = root->astnode.common.nlist; Clist != NULL; Clist = Clist->nextstmt)
  {
    for(temp=Clist->astnode.common.nlist; temp!=NULL; temp=temp->nextstmt)
    {
      if(temp->astnode.ident.arraylist)
        assign_array_dims(temp);
    }
  }
}

/*****************************************************************************
 *                                                                           *
 * type_hash                                                                 *
 *                                                                           *
 * For now, type_hash takes a tree (linked list) of type                     *
 * declarations from the Decblock rule.  It will need to                     *
 * get those from Intrinsic, External, Parameter, etc.                       *
 *                                                                           *
 *****************************************************************************/

void 
type_hash(AST * types)
{
  HASHNODE *hash_entry;
  AST * temptypes, * tempnames;
  int return_type;

   /* Outer for loop traverses typestmts, inner for()
    * loop traverses declists. Code for stuffing symbol table is
    * is in inner for() loop.   
    */
  for (temptypes = types; temptypes; temptypes = temptypes->nextstmt)
  {
      /* Long assignment, set up the for() loop here instead of
         the expression list.  */
    tempnames = temptypes->astnode.typeunit.declist;

      /* Need to set the return value here before entering
         the next for() loop.  */
    return_type = temptypes->astnode.typeunit.returns;

    if(debug)
      printf("type_hash(): type dec is %s\n", print_nodetype(temptypes));

    if(temptypes->nodetype == CommonList) {
      assign_common_array_dims(temptypes);
      continue;
    }

    /* skip parameter statements and data statements */
    if(( (temptypes->nodetype == Specification) &&
         (temptypes->astnode.typeunit.specification == Parameter)) 
        || (temptypes->nodetype == DataList))
      continue;

    for (; tempnames; tempnames = tempnames->nextstmt)
    {
      int i;

      /* ignore parameter assignment stmts */
      if((tempnames->nodetype == Assignment) ||
         (tempnames->nodetype == DataStmt))
        continue;
        
      /* Stuff names and return types into the symbol table. */
      if(debug)
        printf("Type hash: '%s' (%s)\n", tempnames->astnode.ident.name,
          print_nodetype(tempnames));
        
      if(temptypes->nodetype == Dimension)
        assign_array_dims(tempnames);
      else {
        /* check whether there is already an array declaration for this ident.
         * this would be true in case of a normal type declaration with array
         * declarator, in which case we'll do a little extra work here.  but
         * for idents that were previously dimensioned, we need to get this
         * info out of the table.
         */

        hash_entry = type_lookup(array_table,tempnames->astnode.ident.name);
        if(hash_entry) {
          AST *var = hash_entry->variable;
  
          tempnames->astnode.ident.localvnum = -1;
          tempnames->astnode.ident.arraylist = var->astnode.ident.arraylist;
          tempnames->astnode.ident.dim = var->astnode.ident.dim;
          tempnames->astnode.ident.leaddim = var->astnode.ident.leaddim;
          for(i=0;i<MAX_ARRAY_DIM;i++) {
            tempnames->astnode.ident.startDim[i] = var->astnode.ident.startDim[i];
            tempnames->astnode.ident.endDim[i] = var->astnode.ident.endDim[i];
          }
        }
        if((temptypes->token != INTRINSIC) && (temptypes->token != EXTERNAL))
        {
          hash_entry = type_lookup(type_table,tempnames->astnode.ident.name);

          if(hash_entry == NULL) {
            tempnames->vartype = return_type;
            tempnames->astnode.ident.localvnum = -1;

            if(debug){
                printf("hh type_insert: %s\n", tempnames->astnode.ident.name);
            }

            type_insert(type_table, tempnames, return_type,
               tempnames->astnode.ident.name);

            if(debug)
              printf("Type hash (non-external): %s\n",
                  tempnames->astnode.ident.name);
          }
          else {
            if(debug) {
              printf("type_hash: Entry already exists...");  
              printf("going to override the type.\n");  
            }
            hash_entry->variable->vartype = tempnames->vartype;
          }
        }
      }

      /* Now separate out the EXTERNAL from the INTRINSIC on the
       * fortran side.
       */

      if(temptypes != NULL) {
        AST *newnode;

        /* create a new node to stick into the intrinsic/external table
         * so that the type_table isn't pointing to the same node.
         */
        newnode = addnode();
        strcpy(newnode->astnode.ident.name,tempnames->astnode.ident.name);
        newnode->vartype = return_type;
        newnode->nodetype = Identifier;

        switch (temptypes->token)
        {
          case INTRINSIC:
            type_insert(intrinsic_table, 
                    newnode, return_type, newnode->astnode.ident.name);

            if(debug)
              printf("Type hash (INTRINSIC): %s\n",
                newnode->astnode.ident.name);

            break;
          case EXTERNAL:
            type_insert(external_table,
                    newnode, return_type, newnode->astnode.ident.name);

            if(debug)
              printf("Type hash (EXTERNAL): %s\n",
                newnode->astnode.ident.name);

            break;
          default:
            /* otherwise free the node that we didn't use. */
            free_ast_node(newnode);
            break;  /* ansi thing */

        } /* Close switch().  */
      }
    }  /* Close inner for() loop.  */
  }    /* Close outer for() loop.  */
}      /* Close type_hash().       */


/*****************************************************************************
 *                                                                           *
 * exp_to_double                                                             *
 *                                                                           *
 * Java recognizes numbers of the form 1.0e+1, so the `D' and `d' need       *
 * to be replaced with 'e'.                                                  *
 *                                                                           *
 *****************************************************************************/

void 
exp_to_double (char *lexeme, char *temp)
{
  char *cp = lexeme;

  while (*cp)           /* While *cp != '\0'...  */
  {
    if (*cp == 'd' ||   /*  sscanf can recognize 'E'. */
        *cp == 'D')
    {
       *cp = 'e';       /* Replace the 'd' or 'D' with 'e'. */
       break;           /* Should be only one 'd', 'D', etc. */
    }
    cp++;               /* Examine the next character. */
  }

  /* Java should be able to handle exponential notation as part
   * of the float or double constant. 
   */

 strcpy(temp,lexeme);
}  /*  Close exp_to_double().  */


/*****************************************************************************
 *                                                                           *
 * arg_table_load                                                            *
 *                                                                           *
 * Initialize and fill a table with the names of the                         *
 * variables passed in as arguments to the function or                       *
 * subroutine.  This table is later checked when variable                    *
 * types are declared so that variables are not declared                     *
 * twice.                                                                    *  
 *                                                                           *
 *****************************************************************************/

void
arg_table_load(AST * arglist)
{
  AST * temp;

  /* We traverse down `prevstmt' because the arglist is
   * built with right recursion, i.e. in reverse.  This
   * procedure, 'arg_table_load()' is called when the non-
   * terminal `functionargs' is reduced, before the
   * argument list is reversed. Note that a NULL pointer
   * at either end of the list terminates the for() loop. 
   */

   for(temp = arglist; temp; temp = temp->nextstmt)
   {
     type_insert(args_table, temp, 0, temp->astnode.ident.name);
     if(debug)
       printf("#@Arglist var. name: %s\n", temp->astnode.ident.name);
   }
}


/*****************************************************************************
 *                                                                           *
 * lowercase                                                                 *
 *                                                                           *
 * This function takes a string and converts all characters to               *
 * lowercase.                                                                *
 *                                                                           *
 *****************************************************************************/

char * lowercase(char * name)
{
  char *ptr = name;

  while (*name)
  {
    *name = tolower(*name);
     name++;
  }

  return ptr;
}

/*****************************************************************************
 *                                                                           *
 * store_array_var                                                           *
 *                                                                           *
 * We need to make a table of array variables, because                       *
 * fortran accesses arrays by columns instead of rows                        *
 * as C and java does.  During code generation, the array                    *
 * variables are emitted in reverse to get row order.                        *
 *                                                                           *
 *****************************************************************************/

void
store_array_var(AST * var)
{

  if(type_lookup(array_table, var->astnode.ident.name) != NULL)
    fprintf(stderr,"Error: more than one array declarator for array '%s'\n",
       var->astnode.ident.name);
  else
    type_insert(array_table, var, 0, var->astnode.ident.name);

  if(debug)
    printf("Array name: %s\n", var->astnode.ident.name);
}

/*****************************************************************************
 *                                                                           *
 * mypow                                                                     *
 *                                                                           *
 * Double power function.  writing this here so that we                      *
 * dont have to link in the math library.                                    *
 *                                                                           *
 *****************************************************************************/

double
mypow(double x, double y)
{
  double result;
  int i;

  if(y < 0)
  {
    fprintf(stderr,"Warning: got negative exponent in mypow!\n");
    return 0.0;
  }

  if(y == 0)
    return 1.0;

  if(y == 1)
    return x;
  
  result = x;

  for(i=0;i<y-1;i++)
    result *= x;
  
  return result;
}

/*****************************************************************************
 *                                                                           *
 * init_tables                                                               *
 *                                                                           *
 * This function initializes all the symbol tables we'll need during         *
 * parsing and code generation.                                              *
 *                                                                           *
 *****************************************************************************/

void
init_tables()
{
  if(debug)
    printf("Initializing tables.\n");

  initialize_implicit_table(implicit_table);
  array_table     = (SYMTABLE *) new_symtable(211);
  format_table    = (SYMTABLE *) new_symtable(211);
  data_table      = (SYMTABLE *) new_symtable(211);
  save_table      = (SYMTABLE *) new_symtable(211);
  common_table    = (SYMTABLE *) new_symtable(211);
  parameter_table = (SYMTABLE *) new_symtable(211);
  type_table      = (SYMTABLE *) new_symtable(211);
  intrinsic_table = (SYMTABLE *) new_symtable(211);
  external_table  = (SYMTABLE *) new_symtable(211);
  args_table      = (SYMTABLE *) new_symtable(211);
  constants_table = make_dl();
  assign_labels   = make_dl();
  equivList       = NULL;
  save_all        = FALSE;

  cur_do_label = 1000000;

  subroutine_names = make_dl();
  do_labels = make_dl();
}

/*****************************************************************************
 *                                                                           *
 * merge_common_blocks                                                       *
 *                                                                           *
 * In Fortran, different declarations of the same COMMON block may use       *
 * differently named variables.  Since f2j is going to generate only one     *
 * class file to represent the COMMON block, we can only use one of these    *
 * variable names.  What we attempt to do here is take the different names   *
 * and merge them into one name, which we use wherever that common variable  *
 * is used.                                                                  *
 *                                                                           *
 *****************************************************************************/

void
merge_common_blocks(AST *root)
{
  HASHNODE *ht;
  AST *Clist, *temp;
  int count;
  char ** name_array;
  char *comvar = NULL, *var, und_var[80], 
       var_und[80], und_var_und[80], *t;

  for(Clist = root; Clist != NULL; Clist = Clist->nextstmt)
  {
    /* 
     * First check whether this common block is already in
     * the table.
     */

    ht=type_lookup(common_block_table,Clist->astnode.common.name);

    for(temp=Clist->astnode.common.nlist, count = 0; 
              temp!=NULL; temp=temp->nextstmt) 
      count++;

    name_array = (char **) f2jalloc( count * sizeof(name_array) );

    /* foreach COMMON variable */

    for(temp=Clist->astnode.common.nlist, count = 0; 
               temp!=NULL; temp=temp->nextstmt, count++) 
    {
      var = temp->astnode.ident.name;

      /* to merge two names we concatenate the second name
       * to the first name, separated by an underscore.
       */

      if(ht != NULL) {
        comvar = ((char **)ht->variable)[count];
        und_var[0] = '_';
        und_var[1] = 0;
        strcat(und_var,var);
        strcpy(var_und,var);
        strcat(var_und,"_");
        strcpy(und_var_und,und_var);
        strcat(und_var_und,"_");
      }

      if(ht == NULL) {
        name_array[count] = (char *) f2jalloc( strlen(var) + 1 );
        strcpy(name_array[count], var);
      }
      else {
        if(!strcmp(var,comvar) || 
             strstr(comvar,und_var_und) ||
             (((t=strstr(comvar,var_und)) != NULL) && t == comvar) ||
             (((t=strstr(comvar,und_var)) != NULL) && 
               (t+strlen(t) == comvar+strlen(comvar))))
        {
          name_array[count] = (char *) f2jalloc( strlen(comvar) + 1 );
          strcpy(name_array[count], comvar);
        }
        else {
          name_array[count] = (char *) f2jalloc(strlen(temp->astnode.ident.name) 
             + strlen(((char **)ht->variable)[count]) + 2);
  
          strcpy(name_array[count],temp->astnode.ident.name);
          strcat(name_array[count],"_");
          strcat(name_array[count],((char **)ht->variable)[count]);
        }
      }
    }

    type_insert(common_block_table, (AST *)name_array, Float,
         Clist->astnode.common.name);
  }
}

/*****************************************************************************
 *                                                                           *
 * addEquiv                                                                  *
 *                                                                           *
 * Insert the given node (which is itself a list of variables) into a list   *
 * of equivalences.  We end up with a list of lists.                         *
 *                                                                           *
 *****************************************************************************/

void
addEquiv(AST *node)
{
  static int id = 1;

  /* if the list is NULL, create one */

  if(equivList == NULL) {
    equivList = addnode(); 
    equivList->nodetype = Equivalence;
    equivList->token = id++;
    equivList->nextstmt = NULL;
    equivList->prevstmt = NULL;
    equivList->astnode.equiv.clist = node;
  }
  else {
    AST *temp = addnode();

    temp->nodetype = Equivalence;
    temp->token = id++;
    temp->astnode.equiv.clist = node;

    temp->nextstmt = equivList; 
    temp->prevstmt = NULL;

    equivList = temp;
  }
}

/*****************************************************************************
 *                                                                           *
 * eval_const_expr                                                           *
 *                                                                           *
 * This function evaluates a floating-point expression which should consist  *
 * of only parameters and constants.  The floating-point result is returned. *
 *                                                                           *
 *****************************************************************************/

double
eval_const_expr(AST *root)
{
  HASHNODE *p;
  double result1, result2;

  if(root == NULL)
    return 0.0;

  switch (root->nodetype)
  {
    case Identifier:
      if(!strcmp(root->astnode.ident.name,"*"))
        return 0.0;

      p = type_lookup(parameter_table, root->astnode.ident.name);

      if(p)
      {
         if(p->variable->nodetype == Constant) {
           root->vartype = p->variable->vartype;
           return ( atof(p->variable->astnode.constant.number) );
         }
      }

      /* else p==NULL, then the array size is specified with a
       * variable, but we cant find it in the parameter table.
       * it is probably an argument to the function.  do nothing
       * here, just fall through and hit the 'return 0' below.  --keith
       */

      return 0.0;
      
    case Expression:
      if (root->astnode.expression.lhs != NULL)
        eval_const_expr (root->astnode.expression.lhs);

      result2 = eval_const_expr (root->astnode.expression.rhs);

      root->token = root->astnode.expression.rhs->token;

      root->vartype = root->astnode.expression.rhs->vartype;

      return (result2);
    
    case Power:
      result1 = eval_const_expr (root->astnode.expression.lhs);
      result2 = eval_const_expr (root->astnode.expression.rhs);
      root->vartype = MIN(root->astnode.expression.lhs->vartype,
                          root->astnode.expression.rhs->vartype);
      return( mypow(result1,result2) );
  
    case Binaryop:
      result1 = eval_const_expr (root->astnode.expression.lhs);
      result2 = eval_const_expr (root->astnode.expression.rhs);
      root->vartype = MIN(root->astnode.expression.lhs->vartype,
                          root->astnode.expression.rhs->vartype);
      if(root->astnode.expression.optype == '-')
        return (result1 - result2);
      else if(root->astnode.expression.optype == '+')
        return (result1 + result2);
      else if(root->astnode.expression.optype == '*')
        return (result1 * result2);
      else if(root->astnode.expression.optype == '/')
        return (result1 / result2);
      else
        fprintf(stderr,"eval_const_expr: Bad optype!\n");
      return 0.0;
      
    case Unaryop:
      root->vartype = root->astnode.expression.rhs->vartype;
     /*
      result1 = eval_const_expr (root->astnode.expression.rhs);
      if(root->astnode.expression.minus == '-')
        return -result1;
     */
      break;
    case Constant:
      if(debug)
        printf("### its a constant.. %s\n", root->astnode.constant.number);

      if(root->token == STRING) {
        if(!strcmp(root->astnode.ident.name,"*"))
          return 0.0;
        else
          fprintf (stderr, "String in array dec (%s)!\n",
            root->astnode.constant.number);
      }
      else
        return( atof(root->astnode.constant.number) );
      break;
    case ArrayIdxRange:
      /* I dont think it really matters what the type of this node is. --kgs */
      root->vartype = MIN(root->astnode.expression.lhs->vartype,
                          root->astnode.expression.rhs->vartype);
      return(  eval_const_expr(root->astnode.expression.rhs) - 
               eval_const_expr(root->astnode.expression.lhs) );
     
    case Logicalop:
      {
        int lhs=0, rhs;

        root->nodetype = Constant;
        root->vartype = Logical;

        eval_const_expr(root->astnode.expression.lhs);
        eval_const_expr(root->astnode.expression.rhs);

        if(root->token != NOT)
          lhs = root->astnode.expression.lhs->token == TrUE;
        rhs = root->astnode.expression.rhs->token == TrUE;

        switch (root->token) {
          case EQV:
            root->token = (lhs == rhs) ? TrUE : FaLSE;
            break;
          case NEQV:
            root->token = (lhs != rhs) ? TrUE : FaLSE;
            break;
          case AND:
            root->token = (lhs && rhs) ? TrUE : FaLSE;
            break;
          case OR:
            root->token = (lhs || rhs) ? TrUE : FaLSE;
            break;
          case NOT:
            root->token = (! rhs) ? TrUE : FaLSE;
            break;
        }
        return (double)root->token;
      }
      
    default:
      fprintf(stderr,"eval_const_expr(): bad nodetype!\n");
      return 0.0;
  }
  return 0.0;
}

void
printbits(char *header, void *var, int datalen)
{
  int i;

  printf("%s: ", header);
  for(i=0;i<datalen;i++) {
    printf("%1x", ((unsigned char *)var)[i] >> 7 );
    printf("%1x", ((unsigned char *)var)[i] >> 6 & 1 );
    printf("%1x", ((unsigned char *)var)[i] >> 5 & 1 );
    printf("%1x", ((unsigned char *)var)[i] >> 4 & 1 );
    printf("%1x", ((unsigned char *)var)[i] >> 3 & 1 );
    printf("%1x", ((unsigned char *)var)[i] >> 2 & 1 );
    printf("%1x", ((unsigned char *)var)[i] >> 1 & 1 );
    printf("%1x", ((unsigned char *)var)[i] & 1 );
  }
  printf("\n");
}

/*****************************************************************************
 *                                                                           *
 * unary_negate_string                                                       *
 *                                                                           *
 * This function accepts a string and prepends a '-' in front of it.         *
 *                                                                           *
 *****************************************************************************/

char *
unary_negate_string(char *num)
{
  char *tempstr, *mchar;

  /* allocate enough for the number, minus sign, and null char */
  tempstr = (char *)f2jalloc(strlen(num) + 5);

  if(!tempstr) return NULL;

  strcpy(tempstr, num);

  if((mchar = first_char_is_minus(tempstr)) != NULL) {
    *mchar = ' ';
    return tempstr;
  }

  strcpy(tempstr,"-");
  strcat(tempstr,num);

  return tempstr;
}

/*****************************************************************************
 *                                                                           *
 * first_char_is_minus                                                       *
 *                                                                           *
 * Determines whether the number represented by this string is negative.     *
 * If negative, this function returns a pointer to the minus sign.  if non-  *
 * negative, returns NULL.                                                   *
 *                                                                           *
 *****************************************************************************/

char *
first_char_is_minus(char *num)
{
  char *ptr = num;

  while( *ptr ) {
    if( *ptr == '-' )
      return ptr;
    if( *ptr != ' ' )
      return NULL;
    ptr++;
  }

  return NULL;
}

/*****************************************************************************
 *                                                                           *
 * gen_incr_expr                                                             *
 *                                                                           *
 * this function creates an AST sub-tree representing a calculation of the   *
 * increment for this loop.  for null increments, add one.  for non-null     *
 * increments, add the appropriate value.
 *                                                                           *
 *****************************************************************************/

AST *
gen_incr_expr(AST *counter, AST *incr)
{
  AST *plus_node, *const_node, *assign_node, *lhs_copy, *rhs_copy, *incr_copy;

  lhs_copy = addnode();
  memcpy(lhs_copy, counter, sizeof(AST));
  rhs_copy = addnode();
  memcpy(rhs_copy, counter, sizeof(AST));

  if(incr == NULL) {
    const_node = addnode();
    const_node->token = INTEGER;
    const_node->nodetype = Constant;
    const_node->astnode.constant.number = strdup("1");
    const_node->vartype = Integer;

    plus_node = addnode();
    plus_node->token = PLUS;
    rhs_copy->parent = plus_node;
    const_node->parent = plus_node;
    plus_node->astnode.expression.lhs = rhs_copy;
    plus_node->astnode.expression.rhs = const_node;
    plus_node->nodetype = Binaryop;
    plus_node->astnode.expression.optype = '+';
  }
  else {
    incr_copy = addnode();
    memcpy(incr_copy, incr, sizeof(AST));

    plus_node = addnode();
    plus_node->token = PLUS;
    rhs_copy->parent = plus_node;
    incr_copy->parent = plus_node;
    plus_node->astnode.expression.lhs = rhs_copy;
    plus_node->astnode.expression.rhs = incr_copy;
    plus_node->nodetype = Binaryop;
    plus_node->astnode.expression.optype = '+';
  }

  assign_node = addnode();
  assign_node->nodetype = Assignment;
  lhs_copy->parent = assign_node;
  plus_node->parent = assign_node;
  assign_node->astnode.assignment.lhs = lhs_copy;
  assign_node->astnode.assignment.rhs = plus_node;

  return assign_node;
}

/*****************************************************************************
 *                                                                           *
 * gen_iter_expr                                                             *
 *                                                                           *
 * this function creates an AST sub-tree representing a calculation of the   *
 * number of iterations of a DO loop:                                        *
 *     (stop-start+incr)/incr                                                *
 * the full expression is MAX(INT((stop-start+incr)/incr),0) but we will     *
 * worry about the rest of it at code generation time.                       *
 *                                                                           *
 *****************************************************************************/

AST *
gen_iter_expr(AST *start, AST *stop, AST *incr)
{
  AST *minus_node, *plus_node, *div_node, *expr_node, *incr_node;
  
  minus_node = addnode();
  minus_node->token = MINUS;
  minus_node->astnode.expression.lhs = stop;
  minus_node->astnode.expression.rhs = start;
  minus_node->nodetype = Binaryop;
  minus_node->astnode.expression.optype = '-';
  
  if(incr == NULL) {
    incr_node = addnode();
    incr_node->token = INTEGER;
    incr_node->nodetype = Constant;
    incr_node->astnode.constant.number = strdup("1");
    incr_node->vartype = Integer;
  }
  else 
    incr_node = incr;
  
  plus_node = addnode();
  plus_node->token = PLUS;
  plus_node->astnode.expression.lhs = minus_node;
  plus_node->astnode.expression.rhs = incr_node;
  plus_node->nodetype = Binaryop;
  plus_node->astnode.expression.optype = '+';

  if(incr == NULL)
    return plus_node;
    
  expr_node = addnode();
  expr_node->nodetype = Expression;
  expr_node->astnode.expression.parens = TRUE;
  expr_node->astnode.expression.rhs = plus_node;
  expr_node->astnode.expression.lhs = NULL;

  div_node = addnode();
  div_node->token = DIV;
  div_node->astnode.expression.lhs = expr_node;
  div_node->astnode.expression.rhs = incr_node;
  div_node->nodetype = Binaryop;
  div_node->astnode.expression.optype = '/';

  return div_node;
}

/*****************************************************************************
 *                                                                           *
 * initialize_name                                                           *
 *                                                                           *
 * this function initializes an Identifier node with the given name.         *
 *                                                                           *
 *****************************************************************************/

AST *
initialize_name(char *id)
{
  HASHNODE *hashtemp;
  AST *tmp, *tnode;
  char *tempname;

  if(debug)
    printf("initialize_name: '%s'\n",id);

  tmp=addnode();
  tmp->token = NAME;
  tmp->nodetype = Identifier;

  tmp->astnode.ident.needs_declaration = FALSE;
  tmp->astnode.ident.explicit = FALSE;
  tmp->astnode.ident.which_implicit = INTRIN_NOT_NAMED;
  tmp->astnode.ident.localvnum = -1;
  tmp->astnode.ident.array_len = -1;

  if(omitWrappers)
    tmp->astnode.ident.passByRef = FALSE;

  if(type_lookup(java_keyword_table,id))
    id[0] = toupper(id[0]);

  strcpy(tmp->astnode.ident.name, id);
  tempname = strdup(tmp->astnode.ident.name);
  uppercase(tempname);

  if((type_lookup(parameter_table, tmp->astnode.ident.name) == NULL) && 
     (in_dlist(subroutine_names, tmp->astnode.ident.name) == 0))
  {
    if(type_table) {
      hashtemp = type_lookup(type_table, tmp->astnode.ident.name);
      if(hashtemp)
      {
        if(debug)
          printf("initialize_name:'%s' in already hash table (type=%s)..\n",
            id, returnstring[hashtemp->variable->vartype]);
       
        tmp->vartype = hashtemp->variable->vartype;

        if(debug)
          printf("now type is %s\n", returnstring[tmp->vartype]);

        tmp->astnode.ident.len = hashtemp->variable->astnode.ident.len;
      }
      else
      {
        enum returntype ret;
  
        if(debug)
          printf("initialize_name:cannot find name %s in hash table..\n",id);

        if(methodscan(intrinsic_toks, tempname) != NULL) {
          tmp->astnode.ident.which_implicit = 
            intrinsic_or_implicit(tmp->astnode.ident.name); 
        }
      
        ret = implicit_table[tolower(id[0]) - 'a'].type;
  
        if(debug)
          printf("initialize_name:insert with default implicit type %s\n",
            returnstring[ret]);
        
        tmp->vartype = ret;
  
        if(debug)
          printf("type_insert: %s %d\n", tmp->astnode.ident.name, 
            tmp->nodetype);           	

        /* clone the ast node before inserting into the table */
        tnode = clone_ident(tmp);
        tnode->nodetype = Identifier;

        if(tmp->astnode.ident.which_implicit != 
           INTRIN_NAMED_ARRAY_OR_FUNC_CALL) 
        {
          if(debug)
            printf("insert typetable init name\n");

          type_insert(type_table, tnode, ret, tnode->astnode.ident.name);
        }
      }
    }
  }

  return tmp;
}

/*****************************************************************************
*                                                                            *
* intrinsic_or_implict                                                       *
*                                                                            *
* Only gets called if it is an intrinsic name.                               *
*                                                                            *
* this functions tries to figure out if it's intrinsic call, array           *
* or variable.                                                               *
*                                                                            *
******************************************************************************/

int
intrinsic_or_implicit(char *name)
{
  char *p, *tempname, *space_buffer, *clean_buffer, *tmp_spot;
  char *words[12] = {"INTEGER", "DOUBLEPRECISION", "CHARACTER", "DATA",
                      "PARAMETER", "LOGICAL", "INTRINSIC", "EXTERNAL", 
                      "SAVE", "IMPLICIT", "DIMENSION", "CALL"};
  int i, ret_val = INTRIN_NAMED_VARIABLE;

  tempname = (char *)malloc((strlen(name)+2)*sizeof(char));
  space_buffer = (char *)malloc((strlen(line_buffer)+2)*sizeof(char));
  clean_buffer = (char *)malloc((strlen(line_buffer)+2)*sizeof(char));

  strcpy(tempname, name);
  uppercase(tempname);
  strcat(tempname, "(");

  uppercase(line_buffer);

  tmp_spot = line_buffer;
  for(i=0; i<12; i++) {
    if(!strncmp(line_buffer, words[i], strlen(words[i]))) {
      tmp_spot = line_buffer + strlen(words[i]);
      break;
    }
  }
  strcpy(clean_buffer, " \0");
  strcat(clean_buffer, tmp_spot);

  p = strstr(clean_buffer, tempname);
  while(p) {
    if((p)&&(!isalpha((int)*(p-1)))) {
      ret_val=INTRIN_NAMED_ARRAY_OR_FUNC_CALL;
      break;
    }
    for(i=0; i< strlen(tempname); i++)
      p++;
    strcpy(space_buffer, " \0");
    strcat(space_buffer, p);
    p = strstr(space_buffer, tempname);
  }

  free(space_buffer);
  free(clean_buffer);
  free(tempname);

  return ret_val;
}

/*****************************************************************************
 *                                                                           *
 * print_sym_table_names                                                     *
 *                                                                           *
 * Routine to see what's in the symbol table.                                *
 *                                                                           *
 *****************************************************************************/

void
print_sym_table_names(SYMTABLE *table){
   Dlist t_table, tmp;
   AST *node;

   t_table = enumerate_symtable(table);
   dl_traverse(tmp, t_table){

      node = (AST *)dl_val(tmp);
      printf("sym_table %s\n", node->astnode.ident.name);
   }
}

/*****************************************************************************
 *                                                                           *
 * insert_name                                                               *
 *                                                                           *
 * this function inserts the given node into the symbol table, if it is not  *
 * already there.                                                            *
 *                                                                           *
 *****************************************************************************/

void
insert_name(SYMTABLE * tt, AST *node, enum returntype ret)
{
  HASHNODE *hash_entry;
  
  hash_entry = type_lookup(tt,node->astnode.ident.name);

  if(hash_entry == NULL)
    node->vartype = ret;
  else
    node->vartype = hash_entry->variable->vartype;

  type_insert(tt, node, node->vartype, node->astnode.ident.name);
}


/*****************************************************************************
 *                                                                           *
 * initialize_implicit_table                                                 *
 *                                                                           *
 * this function the implicit table, which indicates the implicit typing for *
 * the current program unit (i.e. which letters correspond to which data     *
 * type).                                                                    *
 *                                                                           *
 *****************************************************************************/

void
initialize_implicit_table(ITAB_ENTRY *itab)
{
  int i;

  /* first initialize everything to float */
  for(i = 0; i < 26; i++) {
    itab[i].type = Float;
    itab[i].declared = FALSE;
  }

  /* then change 'i' through 'n' to Integer */
  for(i = 'i' - 'a'; i <= 'n' - 'a'; i++)
    itab[i].type = Integer;
}

/*****************************************************************************
 *                                                                           * 
 * add_implicit_to_tree                                                      *   
 *                                                                           * 
 * this adds a node for an implicit variable to typedec                      * 
 *                                                                           * 
 *****************************************************************************/

void
add_implicit_to_tree(AST *typedec)
{
  Dlist t_table, tmp;
  AST *ast, *new_node, *last_typedec;

  if(!typedec) return;

  last_typedec = typedec;
  while(last_typedec->nextstmt!=NULL) {
    last_typedec = last_typedec->nextstmt;
  }

  t_table = enumerate_symtable(type_table);
  dl_traverse(tmp, t_table) {
    ast = (AST *)dl_val(tmp);
    if(ast->astnode.ident.explicit == FALSE) {
      if(debug)printf("implicit name=%s\n", ast->astnode.ident.name);

      new_node = addnode();
      new_node->astnode.typeunit.returns = ast->vartype;
      new_node->nodetype = Typedec;
      ast->parent = new_node;
      new_node->astnode.typeunit.declist = clone_ident(ast);
      last_typedec->nextstmt = new_node;
      last_typedec = last_typedec->nextstmt;
    }
  }
}

/*****************************************************************************
 *                                                                           * 
 * clone_ident                                                               *   
 *                                                                           * 
 * this function clones an astnode(ident) and passes back the new node       * 
 *                                                                           * 
 *****************************************************************************/

AST *
clone_ident(AST *ast)
{
  AST *new_node;
  int i;

  new_node = addnode();

  new_node->parent = ast->parent;
  new_node->vartype = ast->vartype;

  new_node->astnode.ident.dim  = ast->astnode.ident.dim;
  new_node->astnode.ident.position  = ast->astnode.ident.position;
  new_node->astnode.ident.len  = ast->astnode.ident.len;
  new_node->astnode.ident.localvnum  = ast->astnode.ident.localvnum;
  new_node->astnode.ident.which_implicit = ast->astnode.ident.which_implicit;

  new_node->astnode.ident.passByRef = ast->astnode.ident.passByRef;
  new_node->astnode.ident.needs_declaration = 
     ast->astnode.ident.needs_declaration;
  new_node->astnode.ident.explicit = FALSE;

  for(i=0; i<=MAX_ARRAY_DIM; i++) {
    new_node->astnode.ident.startDim[i] = ast->astnode.ident.startDim[i];
    new_node->astnode.ident.endDim[i] = ast->astnode.ident.endDim[i];
  }

  new_node->astnode.ident.arraylist = ast->astnode.ident.arraylist;

  if(ast->astnode.ident.leaddim)
    new_node->astnode.ident.leaddim = strdup(ast->astnode.ident.leaddim);

  if(ast->astnode.ident.opcode)
    new_node->astnode.ident.opcode = strdup(ast->astnode.ident.opcode);

  if(ast->astnode.ident.commonBlockName)
    new_node->astnode.ident.commonBlockName = 
      strdup(ast->astnode.ident.commonBlockName);

  strcpy(new_node->astnode.ident.name, ast->astnode.ident.name);

  if(ast->astnode.ident.merged_name)
    new_node->astnode.ident.merged_name = 
      strdup(ast->astnode.ident.merged_name);

  if(ast->astnode.ident.descriptor)
    new_node->astnode.ident.descriptor = 
      strdup(ast->astnode.ident.descriptor);

  return new_node;
}

/*****************************************************************************
 *                                                                           *
 * in_dlist                                                                  *
 *                                                                           *
 * Returns 1 if the given name is in the list, returns 0 otherwise.          *
 * Assumes that the list contains char pointers.                             *
 *                                                                           *
 *****************************************************************************/

int
in_dlist(Dlist list, char *name)
{
  Dlist ptr;
  char *list_name;

  dl_traverse(ptr, list){
    list_name = (char *)dl_val(ptr);
    if(!strcmp(list_name, name))
      return 1;
  }

  return 0;
}

/*****************************************************************************
 *                                                                           *
 * in_dlist_stmt_label                                                       *
 *                                                                           *
 * Returns 1 if the given label is in the list, returns 0 otherwise.         *
 * Assumes that the list contains AST pointers.                              *
 *                                                                           *
 *****************************************************************************/

int
in_dlist_stmt_label(Dlist list, AST *label)
{
  Dlist ptr;
  AST *tmp;

  dl_traverse(ptr, list){
    tmp = (AST *)dl_val(ptr);

    if(!strcmp(tmp->astnode.constant.number, label->astnode.constant.number))
      return 1;
  }

  return 0;
}

/*****************************************************************************
 *                                                                           *
 * process_typestmt                                                          *
 *                                                                           *
 * Performs processing to handle a list of variable declarations.            *
 *                                                                           *
 *****************************************************************************/

AST *
process_typestmt(enum returntype this_type, AST *tvlist)
{
  AST *temp, *new;
  enum returntype ret;
  HASHNODE *hashtemp, *hashtemp2;

  new = addnode();
  free_ast_node(tvlist->parent);
  tvlist = switchem(tvlist);
  new->nodetype = Typedec;

  for(temp = tvlist; temp != NULL; temp = temp->nextstmt)
  {
    temp->vartype = this_type;
    ret = this_type;
    if(temp->astnode.ident.len < 0)
      temp->astnode.ident.len = len;
    temp->parent = new;

    hashtemp = type_lookup(args_table, temp->astnode.ident.name);
    if(hashtemp)
      hashtemp->variable->vartype = this_type;

    hashtemp2 = type_lookup(type_table, temp->astnode.ident.name);
    if(hashtemp2) {
      temp->vartype = this_type;
      temp->astnode.ident.explicit = TRUE;
      hashtemp2->variable = temp;
      if(debug) printf("explicit: %s\n",
        hashtemp2->variable->astnode.ident.name);
    }

    if(hashtemp) {
      if(temp->vartype != hashtemp->variable->vartype){
        if(debug) printf("different vartypes\n");
        hashtemp->variable->vartype=temp->vartype;
        hashtemp2->variable->vartype=temp->vartype;
      }
    }
  }

  new->astnode.typeunit.declist = tvlist;
  new->astnode.typeunit.returns = this_type;

  return new;
}

/*****************************************************************************
 *                                                                           *
 * process_array_declaration                                                 *
 *                                                                           *
 * Performs processing to handle an array declaration.                       *
 *                                                                           *
 *****************************************************************************/

AST *
process_array_declaration(AST *varname, AST *dimlist)
{
  AST *new, *temp, *tmp, *tnode;
  int count, i, alen;
  char *tempname, *id;
  enum returntype ret;
 
  if(debug)
    printf("we have an array declaration %s\n", varname->astnode.ident.name);

  tempname = strdup(varname->astnode.ident.name);
  uppercase(tempname);
               
  /* put in type table. we now know this intrinsic name is an array */
  if(methodscan(intrinsic_toks, tempname) != NULL) {
    tmp=addnode();

    tmp->token = NAME;
    tmp->nodetype = Identifier;
    tmp->astnode.ident.needs_declaration = FALSE;
    tmp->astnode.ident.explicit = FALSE;
    tmp->astnode.ident.localvnum = -1;

    id = strdup(varname->astnode.ident.name);
    strcpy(tmp->astnode.ident.name, id);

    ret = implicit_table[tolower(id[0]) - 'a'].type;
    tmp->vartype = ret; 

    tnode = clone_ident(tmp);
    tnode->nodetype = Identifier;
    tnode->astnode.ident.which_implicit = INTRIN_NAMED_ARRAY;

    type_insert(type_table, tnode, ret, tnode->astnode.ident.name);
  }

  new = varname;

  if(debug)
    printf("reduced arraydeclaration... calling switchem\n");
  new->astnode.ident.arraylist = switchem(dimlist);
                  
  count = 0;
  for(temp=new->astnode.ident.arraylist; temp != NULL; temp=temp->nextstmt)
    count++;

  if(count > MAX_ARRAY_DIM) {
    fprintf(stderr,"Error: array %s exceeds max ", new->astnode.ident.name);
    fprintf(stderr,"number of dimensions: %d\n", MAX_ARRAY_DIM);
    exit(EXIT_FAILURE);
  }

  new->astnode.ident.dim = count;

  /*
   * If this is a one-dimensional one-length character array, for example:
   *    character foo(12)
   *    character*1 bar(12)
   * then don't treat as an array.  Set dimension to zero and arraylist
   * to NULL.  Save the arraylist in startDim[2] since we will need it
   * during code generation.
   */

  if((typedec_context == String) && (len == 1) && (count == 1)) {
    new->astnode.ident.dim = 0;
    new->astnode.ident.startDim[2] = new->astnode.ident.arraylist;
    new->astnode.ident.arraylist = NULL;
    return new;
  }

  alen = 1;

  for(temp = new->astnode.ident.arraylist, i = 0;
      temp != NULL; 
      temp=temp->nextstmt, i++)
  {
    /* if this dimension is an implied size, then set both
     * start and end to NULL.
     */

    if((temp->nodetype == Identifier) && 
      (temp->astnode.ident.name[0] == '*'))
    {
      new->astnode.ident.startDim[i] = NULL;
      new->astnode.ident.endDim[i] = NULL;
      alen = 0;
    }
    else if(temp->nodetype == ArrayIdxRange) {
      new->astnode.ident.startDim[i] = temp->astnode.expression.lhs;
      new->astnode.ident.endDim[i] = temp->astnode.expression.rhs;
      alen *= (int)(eval_const_expr(new->astnode.ident.endDim[i]) - 
               eval_const_expr(new->astnode.ident.startDim[i])) + 1;
    }
    else {
      new->astnode.ident.startDim[i] = NULL;
      new->astnode.ident.endDim[i] = temp;
      alen *= (int) eval_const_expr(new->astnode.ident.endDim[i]);
    }
  }

  if(alen)
    new->astnode.ident.array_len = alen;
  else
    new->astnode.ident.array_len = -1;

  new->astnode.ident.leaddim = NULL;
   
  /* leaddim might be a constant, so check for that.  --keith */
  if(new->astnode.ident.arraylist->nodetype == Constant) 
  {
    new->astnode.ident.leaddim = 
      strdup(new->astnode.ident.arraylist->astnode.constant.number);
  }
  else {
    new->astnode.ident.leaddim = 
      strdup(new->astnode.ident.arraylist->astnode.ident.name);
  }

  store_array_var(new);

  return new;
}

/*****************************************************************************
 *                                                                           *
 * process_subroutine_call                                                   *
 *                                                                           *
 * Performs processing to handle a subroutine/function call or array access. *
 *                                                                           *
 *****************************************************************************/

AST *
process_subroutine_call(AST *varname, AST *explist)
{
  char *tempname;
  AST *new;

  new = addnode();
  varname->parent = new;

  if(explist != NULL)
    strcpy(explist->parent->astnode.ident.name, 
      varname->astnode.ident.name);

  /*
   *  Here we could look up the name in the array table and set 
   *  the nodetype to ArrayAccess if it is found.  Then the code 
   *  generator could easily distinguish between array accesses 
   *  and function calls.  I'll have to implement the rest of 
   *  this soon.  -- Keith
   *
   *     if(type_lookup(array_table, varname->astnode.ident.name))
   *       new->nodetype = ArrayAccess;
   *     else
   *       new->nodetype = Identifier;
   */

  new->nodetype = Identifier;

  strcpy(new->astnode.ident.name, varname->astnode.ident.name);

  /* We don't switch index order.  */
  if(explist == NULL) {
    new->astnode.ident.arraylist = addnode();
    new->astnode.ident.arraylist->nodetype = EmptyArgList;
  }
  else
    new->astnode.ident.arraylist = switchem(explist);

  tempname = strdup(new->astnode.ident.name);
  uppercase(tempname);

  if(!type_lookup(external_table, new->astnode.ident.name) &&
     !type_lookup(array_table, new->astnode.ident.name) &&
     methodscan(intrinsic_toks, tempname))
  {
    HASHNODE *ife;

    /* this must be an intrinsic function call, so remove
     * the entry from the type table (because the code
     * generator checks whether something is an intrinsic
     * or not by checking whether it's in the type table).
     */
    ife = type_lookup(type_table, new->astnode.ident.name);
    if(ife)
      ife = hash_delete(type_table, new->astnode.ident.name);
  }

  free_ast_node(varname);
  free(tempname);

  return new;
}

/*****************************************************************************
 *                                                                           *
 * set_function_type                                                         *
 *                                                                           *
 * This sets the relevant fields in the Function AST node to change its type *
 * to that specified by 'ret'.                                               *
 *                                                                           *
 *****************************************************************************/

void
set_function_type(AST *func, enum returntype ret)
{
  HASHNODE *ht;

  func->astnode.source.returns = ret;
  func->vartype = ret;
  func->astnode.source.name->vartype = ret;

  ht = type_lookup(type_table, func->astnode.source.name->astnode.ident.name);

  /* the else case shouldn't be hit since the implied variable
   * should have been inserted already.
   */

  if(ht)
    ht->variable->vartype = ret;
  else {
    fprintf(stderr, "oops: expected implied variable to exist in table!\n");
    exit(EXIT_FAILURE);
  }
}

/*****************************************************************************
 *                                                                           *
 * assign_function_return_type                                               *
 *                                                                           *
 * This function scans the type declarations to see if this function was     *
 * declared.  If so, we reset the return type of the function to the         *
 * type declared here.  e.g.:                                                *
 *         function dlaneg(n)                                                *
 *         integer n                                                         *
 *         integer dlaneg                                                    *
 * Normally the function would have an implicit type of REAL, but it         *
 * will be set to INTEGER in this case.                                      *
 *                                                                           *
 *****************************************************************************/

void
assign_function_return_type(AST *func, AST *specs)
{
  AST *temp, *dec_temp;
  int override = 0;

  for(temp = specs; temp; temp=temp->nextstmt) {
    if(temp->nodetype == Typedec) {
      for(dec_temp = temp->astnode.typeunit.declist; dec_temp;
         dec_temp = dec_temp->nextstmt)
      {
        if(!strcmp(dec_temp->astnode.ident.name, 
               func->astnode.source.name->astnode.ident.name)) 
        {
          override = 1;

          set_function_type(func, temp->astnode.typeunit.returns);
        }
      }
    }
  }

  /* If the data type was not overridden by a local declaration, then we
   * should now assign it to the proper implicit type.  This is done here
   * because the previous type assignment was made before the IMPLICIT
   * statements for this function were parsed.
   */

  if(!override) {
    enum returntype ret;

    ret = implicit_table[tolower(func->astnode.source.name->astnode.ident.name[0]) - 'a'].type;

    set_function_type(func, ret);
  }
}

/*****************************************************************************
 * get_info_from_cilist                                                      *
 *                                                                           *
 * Loops through the Cilist (which is the list of IO specifiers for READ and *
 * WRITE statements), checks the nodetype, and assigns the values to the     *
 * relevant fields of the io_stmt struct.                                    *
 *                                                                           *
 *****************************************************************************/

void
get_info_from_cilist(AST *root, AST *cilist)
{
  int i, count, unit_cnt, fmt_cnt, err_cnt, end_cnt, iostat_cnt, rec_cnt;
  AST *temp, **IoSpecArr;

  unit_cnt = fmt_cnt = err_cnt = end_cnt = iostat_cnt = rec_cnt = 0;

  root->astnode.io_stmt.format_num = -1;
  root->astnode.io_stmt.err = -1;
  root->astnode.io_stmt.end_num = -1;
  root->astnode.io_stmt.unit_desc = NULL;
  root->astnode.io_stmt.iostat = NULL;
  root->astnode.io_stmt.rec = NULL;
  root->astnode.io_stmt.fmt_list = NULL;

  count = 0;
  for(temp=cilist;temp!=NULL;temp=temp->nextstmt)
    count++;

  if(count < 2) {
    yyerror("ERROR (cilist) needs at least UNIT and FMT specifiers");
    exit(EXIT_FAILURE);
  }

  IoSpecArr = (AST **) f2jcalloc(count,sizeof(AST *));

  count = 0;
  for(temp=cilist;temp!=NULL;temp=temp->nextstmt)
    IoSpecArr[count++] = temp;

  if(IoSpecArr[0]->token == IOSPEC_EMPTY) {
    /* this is either an asterisk or an expression without an
     * explicit UNIT=, so we assume it is the unit specifier.
     */

    root->astnode.io_stmt.unit_desc = IoSpecArr[0]->astnode.expression.rhs;
    if(root->astnode.io_stmt.unit_desc)
      root->astnode.io_stmt.unit_desc->parent = root; 

    unit_cnt++;

    /* set this entry to null so we know we've processed it */
    IoSpecArr[0] = NULL;
  }

  if(IoSpecArr[1]->token == IOSPEC_EMPTY) {
    AST *io_expr;

    /* this is either an asterisk or an expression without an
     * explicit FMT=, so we assume it is the format specifier.
     */

    io_expr = IoSpecArr[1]->astnode.expression.rhs;

    if(io_expr->nodetype == Constant)
    {
      if(io_expr->astnode.constant.number[0] == '*') {
        root->astnode.io_stmt.format_num = -1;
        free_ast_node(io_expr);
      }
      else if(io_expr->token == STRING) {
        root->astnode.io_stmt.format_num = -1;
        root->astnode.io_stmt.fmt_list = io_expr;
      }
      else {
        root->astnode.io_stmt.format_num = atoi(io_expr->astnode.constant.number);
        free_ast_node(io_expr);
      }
    }
    else
    {
      /* is this case ever reached??  i don't think so.  --kgs */
      root->astnode.io_stmt.format_num = -1;
      root->astnode.io_stmt.fmt_list = io_expr;
    }

    fmt_cnt++;

    /* set this entry to null so we know we've processed it */
    IoSpecArr[1] = NULL;
  }

  for(i=0;i<count;i++) {
    if(!IoSpecArr[i]) 
      continue;

    if(IoSpecArr[i]->token == IOSPEC_EMPTY) {
      yyerror("ERROR: can't put unspecified control items at this position");
      exit(EXIT_FAILURE);
    }

    if(IoSpecArr[i]->nodetype == UnitExp) {
      root->astnode.io_stmt.unit_desc = IoSpecArr[i]->astnode.expression.rhs;
      root->astnode.io_stmt.unit_desc->parent = root;
      unit_cnt++;
    }
    else if(IoSpecArr[i]->nodetype == IostatExp) {
      AST *pnode;

      /* the IOSTAT variable is emitted as the lhs of an assignment
       * for example:  k = __ftn_file_mgr.open( ... );
       * so here we create a dummy parent node of type Assignment so
       * that when we call name_emit() it will do the right thing.
       */
      root->astnode.io_stmt.iostat = IoSpecArr[i]->astnode.expression.rhs;

      pnode = addnode();
      pnode->nodetype = Assignment;
      pnode->astnode.assignment.lhs = root->astnode.io_stmt.iostat; 
      pnode->astnode.assignment.rhs = root;

      root->astnode.io_stmt.iostat->parent = pnode;
      iostat_cnt++;
    }
    else if(IoSpecArr[i]->nodetype == RecExp) {
      root->astnode.io_stmt.rec = IoSpecArr[i]->astnode.expression.rhs;
      root->astnode.io_stmt.rec->parent = root;
      rec_cnt++;
    }
    else if(IoSpecArr[i]->nodetype == EndExp) {
      root->astnode.io_stmt.end_num = 
         atoi(IoSpecArr[i]->astnode.expression.rhs->astnode.constant.number);
      if(root->astnode.io_stmt.end_num <= 0) {
        yyerror("ERROR (cilist) END specifier must be pos. integer");
        exit(EXIT_FAILURE);
      }
      end_cnt++;
    }
    else if(IoSpecArr[i]->nodetype == ErrExp) {
      root->astnode.io_stmt.err =
         atoi(IoSpecArr[i]->astnode.expression.rhs->astnode.constant.number);
      if(root->astnode.io_stmt.err <= 0) {
        yyerror("ERROR (cilist) ERR specifier must be pos. integer");
        exit(EXIT_FAILURE);
      }
      err_cnt++;
    }
    else if(IoSpecArr[i]->nodetype == FormatOrUnknownSpec) {
      AST *fmt_expr;

      fmt_expr = IoSpecArr[i]->astnode.expression.rhs;

      if(fmt_expr->nodetype == Constant) {
        if(fmt_expr->token == INTEGER) {
          root->astnode.io_stmt.fmt_list = NULL;
          root->astnode.io_stmt.format_num = 
             atoi(fmt_expr->astnode.constant.number);
        }
        else if(fmt_expr->token == STAR) {
          root->astnode.io_stmt.fmt_list = NULL;
          root->astnode.io_stmt.format_num = -1;
        }
        else if(fmt_expr->token == STRING) {
          root->astnode.io_stmt.fmt_list = fmt_expr;
          root->astnode.io_stmt.format_num = -1;
        }
      }
      else {
        yyerror("Internal error - expected Constant in fmt spec");
        exit(EXIT_FAILURE);
      }
    }
    else {
      yyerror("Internal error - unknown IO specifier in cilist");
      fprintf(stderr, "type %s\n", print_nodetype(IoSpecArr[i]));
      exit(EXIT_FAILURE);
    }
  }

  if(unit_cnt > 1) {
    yyerror("ERROR (cilist) cannot have more than one UNIT specifier");
    exit(EXIT_FAILURE);
  }

  if(fmt_cnt > 1) {
    yyerror("ERROR (cilist) cannot have more than one FMT specifier");
    exit(EXIT_FAILURE);
  }

  if(err_cnt > 1) {
    yyerror("ERROR (cilist) cannot have more than one ERR specifier");
    exit(EXIT_FAILURE);
  }

  if(end_cnt > 1) {
    yyerror("ERROR (cilist) cannot have more than one END specifier");
    exit(EXIT_FAILURE);
  }

  if(iostat_cnt > 1) {
    yyerror("ERROR (cilist) cannot have more than one IOSTAT specifier");
    exit(EXIT_FAILURE);
  }

  if(rec_cnt > 1) {
    yyerror("ERROR (cilist) cannot have more than one REC specifier");
    exit(EXIT_FAILURE);
  }
}

/*****************************************************************************
 * get_info_from_olist                                                       *
 *                                                                           *
 * Loops through the Olist (which is the list of IO specifiers for OPEN      *
 * statements), checks the nodetype, and assigns the values to the           *
 * relevant fields of the open struct.                                       *
 *                                                                           *
 *****************************************************************************/

void
get_info_from_olist(AST *root, AST *olist)
{
  int unit_cnt, file_cnt, err_cnt, end_cnt, iostat_cnt, recl_cnt,
      status_cnt, access_cnt, form_cnt, blank_cnt;
  AST *otemp;

  unit_cnt = file_cnt = err_cnt = end_cnt = iostat_cnt = recl_cnt = 0;
  status_cnt = access_cnt = form_cnt = blank_cnt = 0;

  root->vartype = Integer;
  root->expr_side = right;
  root->nodetype = Open;
  root->astnode.open.unit_expr = NULL;
  root->astnode.open.file_expr = NULL;
  root->astnode.open.iostat = NULL;
  root->astnode.open.recl = NULL;
  root->astnode.open.status = NULL;
  root->astnode.open.access = NULL;
  root->astnode.open.form = NULL;
  root->astnode.open.blank = NULL;
  root->astnode.open.err = -1;

  if(!olist) {
    yyerror("ERROR: OPEN stmt needs at least a UNIT specifier");
    exit(EXIT_FAILURE);
  }

  if(olist->token == IOSPEC_EMPTY) {
    /* this is either an asterisk or an expression without an
     * explicit UNIT=, so we assume it is the unit specifier.
     */

    root->astnode.open.unit_expr = olist->astnode.expression.rhs;
    root->astnode.open.unit_expr->parent = root;
    olist = olist->nextstmt;
    unit_cnt++;
  }

  for(otemp=olist;otemp!=NULL;otemp=otemp->nextstmt) {
    if(otemp->token == IOSPEC_EMPTY) {
      yyerror("ERROR: can't put unspecified control items at this position");
      exit(EXIT_FAILURE);
    }

    if(otemp->nodetype == UnitExp) {
      root->astnode.open.unit_expr = otemp->astnode.expression.rhs;
      root->astnode.open.unit_expr->parent = root;
      unit_cnt++;
    }
    else if(otemp->nodetype == IostatExp) {
      AST *pnode;

      /* the IOSTAT variable is emitted as the lhs of an assignment
       * for example:  k = __ftn_file_mgr.open( ... );
       * so here we create a dummy parent node of type Assignment so
       * that when we call name_emit() it will do the right thing.
       */
      root->astnode.open.iostat = otemp->astnode.expression.rhs;

      pnode = addnode();
      pnode->nodetype = Assignment;
      pnode->astnode.assignment.lhs = root->astnode.open.iostat; 
      pnode->astnode.assignment.rhs = root;

      root->astnode.open.iostat->parent = pnode;
      iostat_cnt++;
    }
    else if(otemp->nodetype == OpenFileSpec) {
      root->astnode.open.file_expr = otemp->astnode.expression.rhs;
      root->astnode.open.file_expr->parent = root;
      file_cnt++;
    }
    else if(otemp->nodetype == StatusExp) {
      root->astnode.open.status = otemp->astnode.expression.rhs;
      root->astnode.open.status->parent = root;
      status_cnt++;
    }
    else if(otemp->nodetype == AccessExp) {
      root->astnode.open.access = otemp->astnode.expression.rhs;
      root->astnode.open.access->parent = root;
      access_cnt++;
    }
    else if(otemp->nodetype == FormExp) {
      root->astnode.open.form = otemp->astnode.expression.rhs;
      root->astnode.open.form->parent = root;
      form_cnt++;
    }
    else if(otemp->nodetype == BlankExp) {
      root->astnode.open.blank = otemp->astnode.expression.rhs;
      root->astnode.open.blank->parent = root;
      blank_cnt++;
    }
    else if(otemp->nodetype == ReclExp) {
      root->astnode.open.recl = otemp->astnode.expression.rhs;
      root->astnode.open.recl->parent = root;
      recl_cnt++;
    }
    else if(otemp->nodetype == ErrExp) {
      root->astnode.open.err =
         atoi(otemp->astnode.expression.rhs->astnode.constant.number);
      if(root->astnode.open.err <= 0) {
        yyerror("ERROR: OPEN() ERR specifier must be pos. integer");
        exit(EXIT_FAILURE);
      }
      err_cnt++;
    }
  }

  if(unit_cnt > 1) {
    yyerror("ERROR (olist) cannot have more than one UNIT specifier");
    exit(EXIT_FAILURE);
  }

  if(file_cnt > 1) {
    yyerror("ERROR (olist) cannot have more than one FILE specifier");
    exit(EXIT_FAILURE);
  }

  if(err_cnt > 1) {
    yyerror("ERROR (olist) cannot have more than one ERR specifier");
    exit(EXIT_FAILURE);
  }

  if(end_cnt > 1) {
    yyerror("ERROR (olist) cannot have more than one END specifier");
    exit(EXIT_FAILURE);
  }

  if(iostat_cnt > 1) {
    yyerror("ERROR (olist) cannot have more than one IOSTAT specifier");
    exit(EXIT_FAILURE);
  }

  if(recl_cnt > 1) {
    yyerror("ERROR (olist) cannot have more than one RECL specifier");
    exit(EXIT_FAILURE);
  }

  if(status_cnt > 1) {
    yyerror("ERROR (olist) cannot have more than one STATUS specifier");
    exit(EXIT_FAILURE);
  }

  if(access_cnt > 1) {
    yyerror("ERROR (olist) cannot have more than one ACCESS specifier");
    exit(EXIT_FAILURE);
  }

  if(form_cnt > 1) {
    yyerror("ERROR (olist) cannot have more than one FORM specifier");
    exit(EXIT_FAILURE);
  }

  if(blank_cnt > 1) {
    yyerror("ERROR (olist) cannot have more than one BLANK specifier");
    exit(EXIT_FAILURE);
  }

  if(root->astnode.open.unit_expr->token == STAR) {
    yyerror("ERROR: OPEN statement may not have unit specifier '*'");
    exit(EXIT_FAILURE);
  }
}

/*****************************************************************************
 * get_info_from_cllist                                                      *
 *                                                                           *
 * Loops through the Cllist (which is the list of IO specifiers for CLOSE    *
 * statements), checks the nodetype, and assigns the values to the           *
 * relevant fields of the open struct.                                       *
 *                                                                           *
 *****************************************************************************/

void
get_info_from_cllist(AST *root, AST *cllist)
{
  int unit_cnt, iostat_cnt, err_cnt, status_cnt;
  AST *cltemp;

  unit_cnt = iostat_cnt = err_cnt = status_cnt = 0;

  root->vartype = Integer;
  root->expr_side = right;
  root->astnode.close.unit_expr = NULL;
  root->astnode.close.iostat = NULL;
  root->astnode.close.err = -1;
  root->astnode.close.status = NULL;

  if(!cllist) {
    yyerror("ERROR: CLOSE stmt needs at least a UNIT specifier");
    exit(EXIT_FAILURE);
  }

  if(cllist->token == IOSPEC_EMPTY) {
    /* this is either an asterisk or an expression without an
     * explicit UNIT=, so we assume it is the unit specifier.
     */

    root->astnode.close.unit_expr = cllist->astnode.expression.rhs;
    root->astnode.close.unit_expr->parent = root;
    cllist = cllist->nextstmt;
    unit_cnt++;
  }

  /* need to look for the required unit number */

  for(cltemp=cllist;cltemp!=NULL;cltemp=cltemp->nextstmt) {
    if(cltemp->token == IOSPEC_EMPTY) {
      yyerror("ERROR: can't put unspecified control items at this position");
      exit(EXIT_FAILURE);
    }

    if(cltemp->nodetype == UnitExp) {
      root->astnode.close.unit_expr = cltemp->astnode.expression.rhs;
      root->astnode.close.unit_expr->parent = root;
      unit_cnt++;
    }
    else if(cltemp->nodetype == IostatExp) {
      AST *pnode;

      /* the IOSTAT variable is emitted as the lhs of an assignment
       * for example:  k = __ftn_file_mgr.close( ... );
       * so here we create a dummy parent node of type Assignment so
       * that when we call name_emit() it will do the right thing.
       */
      root->astnode.close.iostat = cltemp->astnode.expression.rhs;

      pnode = addnode();
      pnode->nodetype = Assignment;
      pnode->astnode.assignment.lhs = root->astnode.close.iostat; 
      pnode->astnode.assignment.rhs = root;

      root->astnode.close.iostat->parent = pnode;
      iostat_cnt++;
    }
    else if(cltemp->nodetype == StatusExp) {
      root->astnode.close.status = cltemp->astnode.expression.rhs;
      root->astnode.close.status->parent = root;
      status_cnt++;
    }
    else if(cltemp->nodetype == ErrExp) {
      root->astnode.close.err =
         atoi(cltemp->astnode.expression.rhs->astnode.constant.number);
      if(root->astnode.close.err <= 0) {
        yyerror("ERROR: CLOSE() ERR specifier must be pos. integer");
        exit(EXIT_FAILURE);
      }
      err_cnt++;
    }
  }

  if(unit_cnt > 1) {
    yyerror("ERROR (cllist) cannot have more than one UNIT specifier");
    exit(EXIT_FAILURE);
  }

  if(iostat_cnt > 1) {
    yyerror("ERROR (cllist) cannot have more than one IOSTAT specifier");
    exit(EXIT_FAILURE);
  }

  if(err_cnt > 1) {
    yyerror("ERROR (cllist) cannot have more than one ERR specifier");
    exit(EXIT_FAILURE);
  }

  if(status_cnt > 1) {
    yyerror("ERROR (cllist) cannot have more than one STATUS specifier");
    exit(EXIT_FAILURE);
  }

  if(root->astnode.close.unit_expr->token == STAR) {
    yyerror("ERROR: CLOSE statement may not have unit specifier '*'");
    exit(EXIT_FAILURE);
  }
}
