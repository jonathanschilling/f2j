/*
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */

%{
#include<stdio.h>
#include<stdlib.h>
#include<ctype.h>
#include "f2j.h"
#include<string.h>

#define YYDEBUG 0

int debug = FALSE;

extern char yytext[]; 
extern enum contexts context;
extern BOOLEAN typedecs;

int yylex();

char *strdup(const char *);
char *strcat(char *, const char*);
void yyerror(char *);
AST * addnode();
AST * switchem();
char funname[30];
char tempname[60];
char * tname;
int temptok;
char * lowercase(char * );
void start_vcg();
void emit();
void jas_emit();
void init_tables();
void addEquiv(AST *);
AST * tempnode;
AST * headnode;
AST * localvarlist; 
enum returntype typetemp;

SYMTABLE *ident_table; 
SYMTABLE *jasmin_table;

int emittem = 1;
int len = 1;

AST *equivList = NULL;

void assign_local_vars(AST *);
void assign(AST *);
void typecheck(AST *);
void optScalar(AST *);
int hash(char *);
void type_insert (HASHNODE ** , AST * , int , char *);
void type_hash(AST *);
void merge_common_blocks(AST *);
void arg_table_load(AST *);
int eval_const_expr(AST *, int);
char * print_nodetype (AST *);
int hash_insert (SYMTABLE * , AST *);
char * tok2str(int );
void exp_to_double (char *, char *);
SYMTABLE * new_symtable (int );

%}

%union {
       struct ast_node *ptnode;
       int tok;
       enum returntype type;
       char lexeme[80];
}

/* generic tokens */
%token PLUS MINUS OP CP STAR POW DIV CAT CM EQ COLON NL
%token NOT AND OR
%token  RELOP EQV NEQV
%token <lexeme>  NAME DOUBLE INTEGER EXPONENTIAL 
%token CONST TrUE FaLSE ICON RCON LCON CCON
%token FLOAT CHARACTER LOGICAL COMPLEX NONE

/* a zillion keywords */
%token IF THEN ELSE ELSEIF ENDIF DO GOTO ASSIGN TO CONTINUE STOP
%token RDWR END  STRING CHAR
%token OPEN CLOSE BACKSPACE REWIND ENDFILE FORMAT
%token PROGRAM FUNCTION SUBROUTINE ENTRY CALL RETURN
%token <type> TYPE  
%token DIMENSION
%token COMMON EQUIVALENCE EXTERNAL PARAMETER INTRINSIC IMPLICIT
%token SAVE DATA COMMENT READ WRITE FMT EDIT_DESC REPEAT

%nonassoc RELOP 
%nonassoc LOWER_THAN_COMMENT
%nonassoc COMMENT

/*  All of my additions or changes to Levine's code. These 
non-terminals are in alphabetic order because I have had to 
change the grammar quite a bit.  It is tiring trying to root
out the location of a non-terminal, much easier to find when
in alphabetic order. */

%type <ptnode> Arraydeclaration Arrayname Arraynamelist Assignment
%type <ptnode> Arrayindexlist Arithmeticif
%type <ptnode> Blockif Boolean Close Comment
%type <ptnode> Call Constant Continue
%type <ptnode> Data DataList DataConstant DataItem /* DataElement */ Do_incr Doloop 
%type <ptnode> DataLhs DataConstantList LoopBounds
%type <ptnode> Do_vals Double
%type <ptnode> EquivalenceStmt EquivalenceList EquivalenceItem
%type <ptnode> Else Elseif Elseifs End Exp Explist Exponential External
%type <ptnode> Function Functionargs F2java
%type <ptnode> Fprogram Ffunction Fsubroutine
%type <ptnode> Goto Common CommonList CommonSpec ComputedGoto
%type <ptnode> Implicit Integer Intlist Intrinsic
%type <ptnode> Label Lhs Logicalif
%type <ptnode> Name Namelist LhsList
%type <ptnode> Parameter  Pdec Pdecs Program 
%type <ptnode> Read IoExp IoExplist Return 
%type <ptnode> Save Specstmt Specstmts SpecStmtList Statements 
%type <ptnode> Statement Subroutinecall
%type <ptnode> Sourcecodes  Sourcecode Star
%type <ptnode> String  Subroutine Stop SubstringOp
%type <ptnode> Typestmt Typevar Typevarlist
%type <type>   Types Type 
%type <ptnode> Write WriteFileDesc FormatSpec EndSpec
%type <ptnode> Format FormatExplist FormatExp FormatSeparator
%type <ptnode> RepeatableItem UnRepeatableItem RepeatSpec 
%type <ptnode> log_disjunct log_term log_factor log_primary
%type <ptnode> arith_expr term factor char_expr primary

%%

/*  The new stuff is here.  */
F2java:   Sourcecodes
          {
            AST *temp, *prev, *commentList = NULL;

            if(debug)
              printf("F2java -> Sourcecodes\n");
	    $$ = addnode();
	    $$ = switchem($1);

#if VCG
            if(emittem) start_vcg($$);
#endif
            prev = NULL;
            for(temp=$$;temp!=NULL;temp=temp->nextstmt)
            {
              if(JAS) {
                if(temp->nodetype != Comment) {
                  assign_local_vars(localvarlist); 
                  assign_local_vars( $1->astnode.source.typedecs );
                  assign(temp); 
                }

                if(emittem) jas_emit(temp);
              }else {
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
              }
              prev = temp;
            }
          }
;

Sourcecodes:   Sourcecode 
               {
                 AST *temp;
                 char *hashid;
                 int index;

                 if(debug)
                   printf("Sourcecodes -> Sourcecode\n"); 
                 $$=$1;

                 if(omitWrappers && ($1->nodetype != Comment)) {
                   temp = $1->astnode.source.progtype->astnode.source.name;

                   hashid =temp->astnode.ident.name;
                   index = hash(hashid) % global_func_table->num_entries;
                   type_insert(&(global_func_table->entry[index]), $1, 0,
                     temp->astnode.ident.name);
                 }
               }
             | Sourcecodes Sourcecode 
               {
                 AST *temp;
                 char *hashid;
                 int index;

                 if(debug)
                   printf("Sourcecodes -> Sourcecodes Sourcecode\n");
                 $2->prevstmt = $1; 
                 $$=$2;

                 if(omitWrappers && ($2->nodetype != Comment)) {
                   temp = $2->astnode.source.progtype->astnode.source.name;

                   hashid =temp->astnode.ident.name;
                   index = hash(hashid) % global_func_table->num_entries;
                   type_insert(&(global_func_table->entry[index]), $2, 0,
                     temp->astnode.ident.name);
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

                $$ = addnode();

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
                $$->astnode.source.equivalences = equivList; 

                $$->astnode.source.needs_input = FALSE;
                $$->astnode.source.needs_reflection = FALSE;
                $$->astnode.source.needs_blas = FALSE;

                if(omitWrappers)
                  $$->astnode.source.scalarOptStatus = NOT_VISITED;

	        $1->parent = $$; /* 9-4-97 - Keith */
	        $2->parent = $$; /* 9-4-97 - Keith */
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
              }
;


Fsubroutine: Subroutine Specstmts Statements End 
              {
                HASHNODE *ht;
                AST *temp;
                char *hashid;
                int index;

                if(debug)
                  printf("Fsubroutine -> Subroutine Specstmts Statements End\n");
                $$ = addnode();
	        $1->parent = $$; 
	        $2->parent = $$;
	        $3->parent = $$;
	        $4->parent = $$;
                $$->nodetype = Progunit;
                $$->astnode.source.progtype = $1;

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
                $$->astnode.source.equivalences = equivList; 

                $$->astnode.source.needs_input = FALSE;
                $$->astnode.source.needs_reflection = FALSE;
                $$->astnode.source.needs_blas = FALSE;

                if(omitWrappers)
                  $$->astnode.source.scalarOptStatus = NOT_VISITED;

                $$->astnode.source.typedecs = $2;
                $4->prevstmt = $3;
                $$->astnode.source.statements = switchem($4);

                for(temp=$1->astnode.source.args;temp!=NULL;temp=temp->nextstmt)
                {
                  if((ht=type_lookup(type_table,temp->astnode.ident.name)) != NULL)
                  {
                    temp->vartype=ht->variable->vartype;
                    temp->astnode.ident.arraylist=ht->variable->astnode.ident.arraylist;
                  }
                }

                hashid = $1->astnode.source.name->astnode.ident.name;
                index = hash(hashid) % function_table->num_entries;
                type_insert(&(function_table->entry[index]), $1, 0,
                   $1->astnode.source.name->astnode.ident.name);
              }
;

Ffunction:   Function Specstmts Statements  End
              {
                HASHNODE *ht;
                AST *temp;
                char *hashid;
                int index;

                if(debug)
                  printf("Ffunction ->   Function Specstmts Statements  End\n");

                $$ = addnode();

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
                $$->astnode.source.equivalences = equivList; 

                $$->astnode.source.needs_input = FALSE;
                $$->astnode.source.needs_reflection = FALSE;
                $$->astnode.source.needs_blas = FALSE;
                if(omitWrappers)
                  $$->astnode.source.scalarOptStatus = NOT_VISITED;

	        $1->parent = $$; /* 9-4-97 - Keith */
	        $2->parent = $$; /* 9-4-97 - Keith */
	        $3->parent = $$; /* 9-4-97 - Keith */
	        $4->parent = $$; /* 9-4-97 - Keith */
                $$->nodetype = Progunit;
                $$->astnode.source.progtype = $1;
                $$->astnode.source.typedecs = $2;
		$4->prevstmt = $3;
                $$->astnode.source.statements = switchem($4);

                for(temp=$1->astnode.source.args;temp!=NULL;temp=temp->nextstmt)
                {
                  if((ht=type_lookup(type_table,temp->astnode.ident.name)) != NULL)
                  {
                    temp->vartype=ht->variable->vartype;
                    temp->astnode.ident.arraylist=ht->variable->astnode.ident.arraylist;
                  }
                }

                hashid = $1->astnode.source.name->astnode.ident.name;
                index = hash(hashid) % function_table->num_entries;
                type_insert(&(function_table->entry[index]), $1, 0,
                  $1->astnode.source.name->astnode.ident.name);
              }
;

Program:      PROGRAM Name NL
              {
                 if(debug)
                   printf("Program ->  PROGRAM Name\n");
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
;

Subroutine: SUBROUTINE Name Functionargs NL
              {
                 if(debug)
                   printf("Subroutine ->  SUBROUTINE Name Functionargs NL\n");
                 $$ = addnode();
                 $2->parent = $$; /* 9-4-97 - Keith */
                 if($3 != NULL)
                   $3->parent = $$; /* 9-4-97 - Keith */

        /*         lowercase($2->astnode.ident.name);
                      commented out 11-7-97 - Keith */

                 $$->astnode.source.name = $2; 
                 $$->nodetype = Subroutine;
                 $$->token = SUBROUTINE;
                 if($3 == NULL)
                   $$->astnode.source.args = NULL;
                 else
                   $$->astnode.source.args = switchem($3);

                 fprintf(stderr,"\t%s:\n",$2->astnode.ident.name);
              }
          | SUBROUTINE Name NL
              {
                 if(debug)
                   printf("Subroutine ->  SUBROUTINE Name NL\n");
                 init_tables();
                 $$ = addnode();
                 $2->parent = $$; /* 9-4-97 - Keith */

        /*         lowercase($2->astnode.ident.name);  
                       commented out 11-7-97 - Keith */
                 $$->astnode.source.name = $2; 
                 $$->nodetype = Subroutine;
                 $$->token = SUBROUTINE;
                 $$->astnode.source.args = NULL;
                 fprintf(stderr,"\t%s:\n",$2->astnode.ident.name);
              }
;

Function:  Type FUNCTION Name Functionargs NL 
           {
             HASHNODE *hash_entry;
             char *hashid;
             int index;

             if(debug)
               printf("Function ->  Type FUNCTION Name Functionargs NL\n");
             $$ = addnode();

  	     $3->parent = $$;  /* 9-4-97 - Keith */
             if($4 != NULL)
               $4->parent = $$;  /* 9-4-97 - Keith */
             $$->astnode.source.name = $3;
             $$->nodetype = Function;
             $$->token = FUNCTION;
             $$->astnode.source.returns = $1;
             $$->vartype = $1;
             if($4 == NULL)
               $$->astnode.source.args = NULL;
             else 
               $$->astnode.source.args = switchem($4);

             if(omitWrappers) {
               hashid = $3->astnode.ident.name;

               /*  Hash...  */
               index = hash(hashid) % type_table->num_entries;
               hash_entry = search_hashlist (type_table->entry[index], hashid);
               if(hash_entry != NULL)
                  if(debug) printf("Duplicate symbol table entry: %s\n", hashid);  

               if(hash_entry == NULL)
                 $3->vartype = $1;
               else
                 $3->vartype = hash_entry->variable->vartype;

               type_insert(&(type_table->entry[index]), $3, $3->vartype,
                    $3->astnode.ident.name);
             }
             fprintf(stderr,"\t%s:\n",$3->astnode.ident.name);
           }
; 

Specstmts: SpecStmtList    %prec LOWER_THAN_COMMENT
           {
             $1 = switchem($1);
             type_hash($1); 
             $$=$1;
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

Specstmt:  DIMENSION
           {
	     $$ = 0;
	     fprintf(stderr,"DIMENSION is not implemented.\n");
	     exit(-1);
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

Common:     COMMON CommonList NL
            {
              $$ = switchem($2);
              merge_common_blocks($$);
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

CommonSpec: DIV Name DIV Namelist
           {
              AST *temp;
              int idx, pos;

              $$ = addnode();
              $$->nodetype = Common;
              $$->astnode.common.name = strdup($2->astnode.ident.name);
              $$->astnode.common.nlist = switchem($4);

              pos = 0;
              for(temp=$$->astnode.common.nlist;temp!=NULL;temp=temp->nextstmt)
              {
                temp->astnode.ident.commonBlockName = 
                  strdup($2->astnode.ident.name);

                if(omitWrappers)
                  temp->astnode.ident.position = pos++;

                idx = hash(temp->astnode.ident.name)%common_table->num_entries;
                if(debug)
                  printf("@insert %s (block = %s) into common table (idx=%d)\n",
                   temp->astnode.ident.name, $2->astnode.ident.name, idx);
                type_insert(&(common_table->entry[idx]), temp, Float,
                   temp->astnode.ident.name);
              }

              idx = hash($$->astnode.common.name) % global_common_table->num_entries;
              type_insert(&(global_common_table->entry[idx]), $$, Float,
                 $$->astnode.common.name);
           }
         | CAT Namelist     /* CAT is // */
           {
              AST *temp;
              int idx;

              $$ = addnode();
              $$->nodetype = Common;
              $$->astnode.common.name = strdup("Blank");
              $$->astnode.common.nlist = switchem($2);

              for(temp=$2;temp!=NULL;temp=temp->prevstmt) {
                temp->astnode.ident.commonBlockName = "Blank";
                idx = hash(temp->astnode.ident.name) % common_table->num_entries;
                if(debug)
                  printf("@@insert %s (block = unnamed) into common table\n",
                    temp->astnode.ident.name);
                type_insert(&(common_table->entry[idx]), temp, Float,
                   temp->astnode.ident.name);
              }

              idx = hash($$->astnode.common.name) % global_common_table->num_entries;
              type_insert(&(global_common_table->entry[idx]), $$, Float,
                 $$->astnode.common.name);
           }
;

Save: SAVE NL
       {
         /*
          * I think in this case every variable is supposed to
          * be saved, but we already emit every variable as
          * static.  do nothing here.  --Keith
          */

         $$ = addnode();
         $$->nodetype = Save;
       }
    | SAVE DIV Namelist DIV NL
           {
             AST *temp;
             int idx;

             $$ = addnode();
             $3->parent = $$; /* 9-4-97 - Keith */
             $$->nodetype = Save;

             for(temp=$3;temp!=NULL;temp=temp->prevstmt) {
               idx = hash(temp->astnode.ident.name) % save_table->num_entries;
               if(debug)
                 printf("@@insert %s into save table\n",
                    temp->astnode.ident.name);
               type_insert(&(save_table->entry[idx]), temp, Float,
                   temp->astnode.ident.name);
             }
	   }
    | SAVE Namelist NL
           {
             AST *temp;
             int idx;

             $$ = addnode();
             $2->parent = $$; /* 9-4-97 - Keith */
             $$->nodetype = Save;

             for(temp=$2;temp!=NULL;temp=temp->prevstmt) {
               idx = hash(temp->astnode.ident.name) % save_table->num_entries;
               if(debug)
                 printf("@@insert %s into save table\n",
                    temp->astnode.ident.name);
               type_insert(&(save_table->entry[idx]), temp, Float,
                   temp->astnode.ident.name);
             }
	   }
;
Implicit:   IMPLICIT
            {
	      $$=0;
	      fprintf(stderr,"Must use IMPLICIT NONE.\n");
	      exit(-1);
	    }
         |  IMPLICIT NONE NL
            {
	      $$=addnode();
	      $$->nodetype = Specification;
	      $$->token = IMPLICIT;
	      $$ = 0;
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
              int idx;

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

                if(temp->nodetype == Forloop) 
                {
                  idx = hash(temp->astnode.forloop.counter->astnode.ident.name)
                          % data_table->num_entries;

                  type_insert(&(data_table->entry[idx]), temp, Float,
                     temp->astnode.forloop.counter->astnode.ident.name);
                }
                else
                {
                  idx = hash(temp->astnode.ident.name) % data_table->num_entries;

                  type_insert(&(data_table->entry[idx]), temp, Float,
                     temp->astnode.ident.name);
                }
              }
            }
;

DataConstantList:  DataConstant
                   {
                     $$ = $1;
                   }
                |  DataConstantList CM DataConstant
                   {
                     $3->prevstmt = $1;
                     $$ = $3;
                   }
;

DataConstant:  Constant
               {
                 $$ = $1;
               }
            |  MINUS Constant   
               {
                 $$ = $2;
                 $$->astnode.constant.sign = 1;
               }
            |  Constant STAR Constant
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

LhsList:  DataLhs
          {
            $$ = $1;
          }
        | DataLhs CM LhsList
          {
            $3->prevstmt = $1;
            $$ = $3;
          }
;

DataLhs:  Lhs
          {
            $$ = $1;
          }
        | OP Lhs CM Name EQ LoopBounds CP
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
               $$->nodetype = Forloop;
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
               $$->nodetype = Forloop;
               $$->astnode.forloop.start = $1;
               $$->astnode.forloop.stop = $3;
               $$->astnode.forloop.incr = $5;
             }
;

/*
Constantlist: DataElement
              { 
                $$ = $1;
              }
            | Constantlist CM DataElement
              {
                $3->prevstmt = $1;
                $$ = $3;
              }
;

DataElement:  DataConstant
              {
                $$ = $1;
              }
           |  DataConstant STAR DataConstant
              {
                $$=addnode();
                $$->astnode.expression.lhs = $1;
                $$->astnode.expression.rhs = $3;
                $$->nodetype = Binaryop;
                $$->astnode.expression.optype = '*';
              }
;

DataConstant: Constant
              {
                $$ = $1;
              }
           |  MINUS Constant
              {
                $$ = $2;
                $$->astnode.constant.sign = 1;
              }
;
*/

/*  Here is where the fun begins.  */
/*  No newline token here.  Newlines have to be dealt with at 
    a lower level.
 */
Statements:    Statement  { $$ = $1; }
             | Statements  Statement { $2->prevstmt = $1; $$ = $2; }
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
            | Close
              {
                $$ = $1;
                $$->nodetype = Unimplemented;
              }
            | Comment
              {
                $$ = $1;
                $$->nodetype = Comment;
              }
;           

Comment: COMMENT NL
         {
           $$ = addnode();
           $$->token = COMMENT;
           $$->nodetype = Comment;
           strcpy($$->astnode.ident.name, yylval.lexeme);
         }
;

Close:  CLOSE OP Name CP NL
        {
          fprintf(stderr,"WArning: CLOSE not implemented.\n");
          $$ = $3;
        }
;

End:    END  NL 
        {
          $$ = addnode();
          $$->token = END;
          $$->nodetype = End;
        }
;

/* 
 * We have to load up a symbol table here with the names of all the
 * variables that are passed in as arguments to our function or
 * subroutine.  Also need to pass `namelist' off to a procedure
 * to load a local variable table for opcode generation.   
 */

Functionargs:   OP Namelist CP   
                {
                  if(JAS)
  		    localvarlist = switchem($2);
                  else
	 	    $2 = switchem($2);  
                  init_tables();
		  arg_table_load($2);
                  $$ = $2;
                }
              | OP CP
                {
                  init_tables();
                  $$ = NULL;
                }
;


Namelist:   Name  
            {
              $$=$1;
            }
          | Namelist CM Name 
            {
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

Typestmt:      Types Typevarlist NL
              {
                 AST *temp;

                 $$ = addnode();
	         $2->parent = $$; /* 9-4-97 - Keith */
                 /* store_local_var($2);  */
                 $2 = switchem($2);
                 $$->nodetype = Typedec;

                 for(temp = $2; temp != NULL; temp = temp->nextstmt)
                 {
                   temp->vartype = $1;
                   temp->astnode.ident.len = len;
                 }

                 $$->astnode.typeunit.declist = $2;
                 $$->astnode.typeunit.returns = $1; 
	       }
;


Types:       Type 
             {
               $$ = $1;
               len = 1;
             }
          |  Type Star Integer
             {
               $$ = $1;
               len = atoi($3->astnode.constant.number);
             }
	  |  Type Star OP Star CP
             {
               $$ = $1;
               len = 1;
             }
;

Type:  TYPE
       { 
         $$ = yylval.type;
       }
;

/* Here I'm going to do the same thing I did with Explist.  That is,
 * each element in the list of typevars will have a parent link to a 
 * single node indicating that the context of the array is a
 * declaration.  --Keith 
 */

Typevarlist: Typevar
             {
               AST *temp;

               temp = addnode();
               temp->nodetype = Typedec;
               $1->parent = temp;

               $$ = $1;
             }
          |  Typevarlist CM  Typevar
             {
               $3->prevstmt = $1;
               $3->parent = $1->parent;
               $$ = $3;
             }
;

Typevar:   Name 
           {
             $$ = $1;
           }
         | Arraydeclaration 
           {
             $$ = $1;
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

           $$=addnode();
	   $$->token = NAME;
           $$->nodetype = Identifier;

           $$->astnode.ident.needs_declaration = FALSE;
           $$->astnode.ident.lead_expr = NULL;

           if(omitWrappers) {
             $$->astnode.ident.passByRef = FALSE;
             $$->astnode.ident.isLhs     = FALSE;
           }

           lowercase(yylval.lexeme);

           if(type_lookup(java_keyword_table,yylval.lexeme) ||
              type_lookup(jasmin_keyword_table,yylval.lexeme))
                 yylval.lexeme[0] = toupper(yylval.lexeme[0]);

           strcpy($$->astnode.ident.name, yylval.lexeme);

           hashtemp = type_lookup(type_table, $$->astnode.ident.name);
           if(hashtemp)
           {
             $$->vartype = hashtemp->variable->vartype;
             $$->astnode.ident.len = hashtemp->variable->astnode.ident.len;
           }
         }

/*
       | Char {$$ = $1;}
       | String {$$ = $1;}
*/

;

/*
Char:     CHAR
         {
           $$=addnode();
           $$->token = CHAR; 
           $$->nodetype = Identifier;
           $$->astnode.ident.lead_expr = NULL;
           strcpy($$->astnode.ident.name, yylval.lexeme);
         }
;
*/

String:  STRING
         {
           $$=addnode();
           $$->token = STRING;
           $$->nodetype = Identifier;
           $$->astnode.ident.lead_expr = NULL;
           strcpy($$->astnode.ident.name, yylval.lexeme);
           $$->vartype = String;
           if(debug)
             printf("**The string value is %s\n",$$->astnode.ident.name);
         }
       | CHAR
         {
           $$=addnode();
           $$->token = STRING;
           $$->nodetype = Identifier;
           $$->astnode.ident.lead_expr = NULL;
           strcpy($$->astnode.ident.name, yylval.lexeme);
           $$->vartype = String;
           if(debug)
             printf("**The char value is %s\n",$$->astnode.ident.name);
         }
;

Arraydeclaration: Name OP Arraynamelist CP 
                  {
                    AST *temp;
                    int count, i;

		    /*
                     *  $$ = addnode();
                     *  $$->nodetype = Identifier;
                     *  strcpy($$->astnode.ident.name, $1->astnode.ident.name);
		     */

		    $$ = $1;
		    $$->astnode.ident.arraylist = switchem($3);
                  
                    count = 0;
                    for(temp = $$->astnode.ident.arraylist; temp != NULL; 
                        temp=temp->nextstmt)
                      count++;

                    for(temp = $$->astnode.ident.arraylist, i = 0;
                        temp != NULL; 
                        temp=temp->nextstmt, i++)
                    {
                      $$->astnode.ident.D[i] = eval_const_expr(temp,count);
                      if(temp->nodetype == ArrayIdxRange)
                        printf("@#@# %s dim %d is a range\n",$$->astnode.ident.name,
                           i);
                    }
                       
                    $$->astnode.ident.dim = count;
                    $$->astnode.ident.lead_expr = NULL;
   
                    /* leaddim might be a constant, so check for that.  --keith */
                    if($$->astnode.ident.arraylist->nodetype == Constant) 
                    {
		      $$->astnode.ident.leaddim = 
                       strdup($$->astnode.ident.arraylist->astnode.constant.number);
                    }
                    else if(($$->astnode.ident.arraylist->nodetype == Binaryop) ||
                            ($$->astnode.ident.arraylist->nodetype == ArrayIdxRange)) {
		      $$->astnode.ident.lead_expr = $$->astnode.ident.arraylist;
                    } else {
		      $$->astnode.ident.leaddim = 
                       strdup($$->astnode.ident.arraylist->astnode.ident.name);
                    }

                    if(debug)
                    {
                      printf("leaddim nodetype = %s\n",
                        print_nodetype($$->astnode.ident.arraylist));

                      if($$->astnode.ident.leaddim != NULL)
                        printf("setting leaddim = %s\n",$$->astnode.ident.leaddim);
                    }

		    store_array_var($$);
                  }

Arraynamelist:    Arrayname 
                  {
                    AST *temp;

                    temp = addnode();
                    temp->nodetype = ArrayDec;
                    $1->parent = temp;

                    $$=$1;
                  }
                | Arraynamelist CM Arrayname 
                  {
                    $3->prevstmt = $1; 
                    $3->parent = $1->parent;
                    $$ = $3;
                  }
;

Arrayname: Exp {$$ = $1; }
         | Star {$$=$1;}
         | Exp COLON Exp { 
             $$ = addnode();
             $$->nodetype = ArrayIdxRange;
             $$->astnode.expression.lhs = $1;
             $$->astnode.expression.rhs = $3;
           }
/*
Arrayname:   Name {$$=$1;}
           | Star {$$=$1;}
           | Integer {$$=$1;}
;
*/

/*  We reduce STAR here, make changes in the Binaryops
    reductions for that.  This handles the fortran array
    declaration, e.g., array(*).  */
Star:  STAR 
       {
         $$=addnode();
         $$->nodetype = Identifier;
         $$->astnode.ident.lead_expr = NULL;
        *$$->astnode.ident.name = '*';
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
                HASHNODE *ht;

                $$ = addnode();
                $1->parent = $$; /* 9-4-97 - Keith */
                $3->parent = $$; /* 9-4-97 - Keith */
                $$->nodetype = Assignment;
                $$->astnode.assignment.lhs = $1;
                $$->astnode.assignment.rhs = $3;

                if(omitWrappers) {
                  ht = type_lookup(type_table, $1->astnode.ident.name);
                  if(ht)
                    ht->variable->astnode.ident.isLhs = TRUE;                    
                }
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
           /*   Use the following declaration in case we 
            *   need to switch index order. 
            *
            *   HASHNODE * hashtemp;  
            */

           $$ = addnode();
           $1->parent = $$; /* 9-4-97 - Keith */
           $3->parent = $$; /* 9-4-97 - Keith */
           $$->nodetype = Identifier;
           $$->astnode.ident.lead_expr = NULL;
           $$->prevstmt = NULL;
           $$->nextstmt = NULL;

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
         }
      |  Name OP Exp COLON Exp CP
         {
           $$=addnode();
           $1->parent = $$;
           $3->parent = $$;
           $5->parent = $$;
           strcpy($$->astnode.ident.name, $1->astnode.ident.name);
           $$->nodetype = Substring;
           $$->prevstmt = NULL;
           $$->nextstmt = NULL;
           $$->astnode.ident.arraylist = $3;
           $3->nextstmt = $5;
         }
;

Arrayindexlist:   Exp 
                  { 
                    AST *temp;

                    temp = addnode();
                    temp->nodetype = Identifier;
                    temp->astnode.ident.lead_expr = NULL;
                    $1->parent = temp;

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
    to emit java source code.  */

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
;


Do_vals:  Assignment CM Exp   NL
          {
            $$ = addnode();
	    $1->parent = $$; /* 9-4-97 - Keith */
	    $3->parent = $$; /* 9-4-97 - Keith */
            $$->astnode.forloop.counter = $1->astnode.assignment.lhs;
            $$->astnode.forloop.start = $1;
            $$->astnode.forloop.stop = $3;
            $$->astnode.forloop.incr = 0;
          }

      
       | Assignment CM Exp CM Exp   NL
         {
           $$ = addnode();
	   $1->parent = $$; /* 9-4-97 - Keith */
	   $3->parent = $$; /* 9-4-97 - Keith */
	   $5->parent = $$; /* 9-4-97 - Keith */
           $$->nodetype = Forloop;
           $$->astnode.forloop.start = $1;
           $$->astnode.forloop.stop = $3;
           $$->astnode.forloop.incr = $5;
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
       }
     | Integer Format NL 
       {
         $$ = addnode();
         $1->parent = $$;
         $2->parent = $$;
         $$->nodetype = Format;
         $$->astnode.label.number = atoi($1->astnode.constant.number);
         $$->astnode.label.stmt = $2;
         $2->astnode.label.number = $$->astnode.label.number;
         if(debug)
           printf("@@ inserting format line num %d\n",$$->astnode.label.number);
         hash_insert(format_table,$2);
       }

/*  The following productions for FORMAT parsing are derived
    from Robert K. Moniot's grammar (see ftnchek-2.9.4) */

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
     | Name
       {
         $$ = $1;
       }
     | Name '.' Constant
       {
         /* ignore the constant part for now */
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
       }
;

Write: WRITE OP WriteFileDesc CM FormatSpec CP IoExplist NL
       {
         AST *temp;

         $$ = addnode();
         $$->astnode.io_stmt.io_type = Write;
         $$->astnode.io_stmt.fmt_list = NULL;

         /*  unimplemented
           $$->astnode.io_stmt.file_desc = ;
         */

         if($5->nodetype == Constant)
         {
           if($5->astnode.constant.number[0] == '*') 
             $$->astnode.io_stmt.format_num = -1;
           else
             $$->astnode.io_stmt.format_num = atoi($5->astnode.constant.number);
         }
         else
         {
           $$->astnode.io_stmt.format_num = -1;
           $$->astnode.io_stmt.fmt_list = $5;
         }
 
         if($7 == NULL)
           $$->astnode.io_stmt.arg_list = NULL;
         else 
           $$->astnode.io_stmt.arg_list = switchem($7);

         for(temp=$$->astnode.io_stmt.arg_list;temp!=NULL;temp=temp->nextstmt)
           temp->parent->nodetype = Write;
       }
;

/* 
   Maybe I'll implement this stuff someday. 
*/
WriteFileDesc: 
      Exp
       {
         /* do nothing for now */
         $$ = $1;
       }
    | STAR
       {
         /* do nothing for now */
         ;
       }
;
     
FormatSpec:
       FMT EQ Integer
        {
          $$ = $3;
        }
     | Integer
        {
          $$ = $1;
        }
     | FMT EQ STAR
        {
          $$ = addnode();
	  $$->token = INTEGER;
          $$->nodetype = Constant;
          strcpy($$->astnode.constant.number,"*");
	  $$->astnode.constant.type = Integer;
        }
     | STAR
        {
          $$ = addnode();
	  $$->token = INTEGER;
          $$->nodetype = Constant;
          strcpy($$->astnode.constant.number,"*");
	  $$->astnode.constant.type = Integer;
        }
     | FMT EQ String
        {
          $$ = $3;
        }
;

Read: READ OP WriteFileDesc CM FormatSpec CP IoExplist NL
      {
         $$ = addnode();
         $$->astnode.io_stmt.io_type = Read;
         $$->astnode.io_stmt.fmt_list = NULL;
         $$->astnode.io_stmt.end_num = -1;

         if($7 == NULL)
           $$->astnode.io_stmt.arg_list = NULL;
         else 
           $$->astnode.io_stmt.arg_list = switchem($7);
      }
    | READ OP WriteFileDesc CM FormatSpec CM EndSpec CP IoExplist NL
      {
         $$ = addnode();
         $$->astnode.io_stmt.io_type = Read;
         $$->astnode.io_stmt.fmt_list = NULL;
         $$->astnode.io_stmt.end_num = atoi($7->astnode.constant.number);

         if($9 == NULL)
           $$->astnode.io_stmt.arg_list = NULL;
         else 
           $$->astnode.io_stmt.arg_list = switchem($9);
      }
;

IoExplist: IoExp
           {
             AST *temp;

             temp = addnode();
             temp->nodetype = IoExplist;
             $1->parent = temp;

             $$ = $1;
           }
         | IoExplist CM IoExp
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

IoExp: Exp
       {
         $$ = $1;
       }
     | OP Exp CM Name EQ Exp CM Exp CP /* implied do loop */
       {
         $$ = addnode();
         $$->nodetype = ImpliedLoop;
         $$->astnode.forloop.start = $6;
         $$->astnode.forloop.stop = $8;
         $$->astnode.forloop.incr = NULL;
         $$->astnode.forloop.counter = $4;
         $$->astnode.forloop.Label = $2;

         $2->parent = $$;
         $4->parent = $$;
         $6->parent = $$;
         $8->parent = $$;
       }
;


EndSpec: END EQ Integer
         {
           $$ = $3;
         }
;

/*  Got a problem when a Blockif opens with a Blockif.  The
    first statement of the second Blockif doesn't get into the
    tree.  Might be able to use do loop for example to fix this. */

Blockif:   IF OP Exp CP THEN NL Statements Elseifs Else  ENDIF NL
           {
             $$ = addnode();
             $3->parent = $$;
             $7->parent = $$; /* 9-4-97 - Keith */
             if($8 != NULL) 
               $8->parent = $$; /* 9-4-97 - Keith */
             if($9 != NULL)
               $9->parent = $$; /* 9-4-97 - Keith */
             $$->nodetype = Blockif;
             $$->astnode.blockif.conds = $3;
             if($7 != 0) $7 = switchem($7);
             $$->astnode.blockif.stmts = $7;
             /*  If there are any `else if' statements,
                 switchem. Otherwise, NULL pointer checked
                 in code generating functions. */
             if($8 != 0) $8 = switchem($8); 
             $$->astnode.blockif.elseifstmts = $8; /* Might be NULL. */
             $$->astnode.blockif.elsestmts = $9;   /* Might be NULL. */
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
              }
;

/******************************************************************

changed Logicalif production to IF OP Exp CP Statement, so there's
 no longer a need for this production.

Logicalifstmts:  Assignment NL 
                 {
                   $$=$1;
                   $$->nodetype = Assignment;
                 }
                | Return
                  {
                    $$=$1;

                    if(debug) 
                       printf("Return from lif.\n");

                    $$->nodetype = Return;
                    $$->token = RETURN;
                  }
                | Goto 
                  {
                    $$=$1;
                    $$->nodetype = Goto;
                  }
                | Call 
                  {
                    $$=$1;
                    $$->nodetype = Call;
                  }
                | Write 
                  {
                    $$=$1;
                    $$->nodetype = Write;
                  }
;

*******************************************************************/

/* 
 * This _may_ have to be extended to deal with 
 * jasmin opcode.  Variables of type array need 
 * to have their arguments emitted in reverse order 
 * so that java can increment in row instead of column
 * order.  So we look each name up in the array table, 
 * it is in there we leave the argument list reversed, 
 * otherwise, it is a subroutine or function (method) 
 * call and we reverse the arguments.
 */

Subroutinecall:   Name OP Explist CP
                  {
                    /* Use the following declarations in case we 
                     * need to switch index order.
                     * 
                     * HASHNODE * hashtemp;  
                     * HASHNODE * ht;
                     */

                    $$ = addnode();
                    $1->parent = $$;  /* 9-4-97 - Keith */

                    /*  $3->parent = $$;  9-4-97 - Keith */

                    if($3 != NULL)
                      strcpy($3->parent->astnode.ident.name, 
                        $1->astnode.ident.name);

                    /*
                     *  Here we could look up the name in the array table and set 
                     *  the nodetype to ArrayAccess if it is found.  Then the code 
                     *  generator could easily distinguish between array accesses 
                     *  and function calls.  I'll have to implement the rest of 
                     *  this soon.  -- Keith
                     *
                     *     if(type_lookup(array_table, $1->astnode.ident.name))
                     *       $$->nodetype = ArrayAccess;
                     *     else
                     *       $$->nodetype = Identifier;
                     */

                    $$->nodetype = Identifier;

                    $$->astnode.ident.lead_expr = NULL;
                    strcpy($$->astnode.ident.name, $1->astnode.ident.name);

                    /*  This is in case we want to switch index order later.
                     *
                     *  hashtemp = type_lookup(array_table, $1->astnode.ident.name);
                     *  if(hashtemp != NULL)
                     *    $$->astnode.ident.arraylist = $3;
                     *  else
                     */

                    /* We don't switch index order.  */
                    if($3 == NULL) {
                      $$->astnode.ident.arraylist = addnode();
                      $$->astnode.ident.arraylist->nodetype = EmptyArgList;
                    }
                    else
                      $$->astnode.ident.arraylist = switchem($3);
                  }
;

SubstringOp: Name OP Exp COLON Exp CP
           {
              if(debug)
                printf("SubString!\n");
              $$ = addnode();
              $1->parent = $$;
              $3->parent = $$;
              $5->parent = $$;
              strcpy($$->astnode.ident.name, $1->astnode.ident.name);
              $$->nodetype = Substring;
              $$->astnode.ident.arraylist = $3;
              $3->nextstmt = $5;
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
Call:     CALL   Subroutinecall  NL
          {
             $$ = $2;
	     $$->nodetype = Call;
          }
       |  CALL Name NL
          {
            $$ = addnode();
            $2->parent = $$;
            $$->nodetype = Identifier;
            $$->astnode.ident.lead_expr = NULL;
            strcpy($$->astnode.ident.name, $2->astnode.ident.name);
            $$->astnode.ident.arraylist = addnode();
            $$->astnode.ident.arraylist->nodetype = EmptyArgList;
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
              $$ = addnode();
              $2->parent = $$;
              $$->astnode.expression.rhs = $2;
              $$->astnode.expression.lhs = 0;
              $$->astnode.expression.minus = '-';   
              $$->nodetype = Unaryop;
            }
          | PLUS term
            {
              $$ = addnode();
              $2->parent = $$;
              $$->astnode.expression.rhs = $2;
              $$->astnode.expression.lhs = 0;
              $$->astnode.expression.minus = '+';
              $$->nodetype = Unaryop;
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
             $$->nodetype = Binaryop;
             $$->astnode.expression.optype = '+';
           }
;

primary:     Name {$$=$1;}
          |  Constant
             {
	       $$ = $1;
               $$->nodetype = Constant;
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
               $$->astnode.expression.lhs = 0;
             }
;

/*
Complex: OP Constant CM Constant CP {$$=addnode();}
;
*/

/* `TRUE' and `FALSE' have already been typedefed
   as BOOLEANs.  */
Boolean:  TrUE
             {
               $$ = addnode();
               $$->token = TrUE;
               $$->nodetype = Constant;
               strcpy($$->astnode.constant.number, "true");
               $$->astnode.constant.type = INTEGER;
               $$->astnode.constant.sign = 0;
               $$->vartype = Logical;
             }
         | FaLSE
             {
               $$ = addnode();
               $$->token = FaLSE;
               $$->nodetype = Constant;
               strcpy($$->astnode.constant.number, "false");
               $$->astnode.constant.type = INTEGER;
               $$->astnode.constant.sign = 0;
               $$->vartype = Logical;
             }

;

Constant:   
         Integer  
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
               $$ = addnode();
               $$->token = INTEGER;
               $$->nodetype = Constant;
               strcpy($$->astnode.constant.number, yylval.lexeme);
               $$->astnode.constant.type = Integer;
               $$->astnode.constant.sign = 0;
               $$->vartype = Integer;
             }
;

Double:       DOUBLE
             {
               $$ = addnode();
	       $$->token = DOUBLE;
               $$->nodetype = Constant;
               strcpy($$->astnode.constant.number, yylval.lexeme);
	       /*               $$->astnode.constant.type = DOUBLE; */
	       $$->astnode.constant.type = Double;
               $$->astnode.constant.sign = 0;
               $$->vartype = Double;
             }
;
               
/*  Since jasmin doesn't have an EXPONENTIAL data type,
    the function exp_to_double rewrite numbers in the
    nn.dde+nn as floats.  The float is written back into
    the string temp.  

    For small numbers, exp_to_double isn't good.  e.g., 
    something like 5.5e-15 would be transformed into
    "0.00000".
    
    I'll just change the 'D' to 'e' and emit as-is for
    Java.  With Jasmin, I'll still use exp_to_double
    for now, but it will be wrong.

    3/11/98  -- Keith 
 */

Exponential:   EXPONENTIAL
             {
               $$ = addnode();
	       $$->token = EXPONENTIAL;
               $$->nodetype = Constant;
	       exp_to_double(yylval.lexeme, tempname);
               strcpy($$->astnode.constant.number, tempname);
               $$->astnode.constant.type = EXPONENTIAL;
               $$->astnode.constant.sign = 0;
               $$->vartype = Double;
             }
;

/*  All the easy productions that work go here.  */

Return:      RETURN NL
             {
                $$= addnode();
             }
;

Stop:   STOP NL
        {
          $$ = addnode();
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
            AST *temp;
            int index;
            char *hashid;

            if(debug)
              printf("Parameter...\n");

            $$ = $1;
            $$->nodetype = Assignment;

            temp = $$->astnode.assignment.rhs;

            hashid = $$->astnode.assignment.lhs->astnode.ident.name;
            index = hash(hashid) % parameter_table->num_entries;
            type_insert(&(parameter_table->entry[index]), temp, 0,
               $$->astnode.assignment.lhs->astnode.ident.name);

            /*
             *  $$->astnode.typeunit.specification = Parameter; 
             *
             * Attach the Assignment node to a list... Hack.
             *  $$->astnode.typeunit.declist = $1;  
             */
          }
;

External:  EXTERNAL Namelist NL
           {
             $$=addnode(); 
             $2->parent = $$;  /* 9-3-97 - Keith */
             $$->nodetype = Specification;
             $$->token = EXTERNAL;
             $$->astnode.typeunit.declist = switchem($2);
             $$->astnode.typeunit.specification = External;
           }
;

Intrinsic: INTRINSIC Namelist NL
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


/* The standard error routine, which is not currently implemented. */
void 
yyerror(char *s)
{
  printf("%d: %s\n", lineno, s);
}

/* To keep things simple, there is only one type of parse tree
   node.  If there is any way to ensure that all the pointers
   in this are NULL, it would be a good idea to do that.  I am
   not sure what the default behavior is.
   */
AST * 
addnode() 
{
AST * newnode;

    if ((newnode = (AST*)calloc(1,sizeof(AST))) == NULL) 
    {
        perror("calloc\n");
        exit(1);
    }
    return newnode;
} 


/*  
 * Need to turn the linked list around,
 * so that it can traverse forward instead of in reverse.
 * What I do here is create a doubly linked list. 
 * Note that there is no `sentinel' or `head' node
 * in this list.  It is acyclic and terminates in 
 * NULL pointers.
 */

AST * 
switchem(AST * root) 
{

if (root->prevstmt == NULL) 
return root;

  while ( root->prevstmt != 0) 
    {
      root->prevstmt->nextstmt = root;
      root = root->prevstmt;
    }
  return root;
}

/* 
 * For now, type_hash takes a tree (linked list) of type
 * declarations from the Decblock rule.  It will need to
 * get those from Intrinsic, External, Parameter, etc.
 */
void 
type_hash(AST * types)
{
  HASHNODE *hash_entry;
  AST * temptypes, * tempnames;
  int return_type, index;
  char * hashid;
  extern SYMTABLE * type_table, * intrinsic_table, * external_table; 
   
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

    for (; tempnames; tempnames = tempnames->nextstmt)
    {
      /* Stuff names and return types into the symbol table. */
      hashid = tempnames->astnode.ident.name;
      if(debug)printf("Type hash: %s\n", tempnames->astnode.ident.name);

      /*  Hash...  */
      index = hash(hashid) % type_table->num_entries;
      if((hash_entry = search_hashlist (type_table->entry[index], hashid)) != NULL)
      {
        if(debug)printf("Duplicate entry.\n");  
        /*  exit(-1);  */
      }

      if(hash_entry == NULL)
        tempnames->vartype = return_type;
      else
        tempnames->vartype = hash_entry->variable->vartype;

      /* 
       * All names go into the name table.  
       */

      type_insert(&(type_table->entry[index]), tempnames, return_type,
          tempnames->astnode.ident.name);

      /* Now separate out the EXTERNAL from the INTRINSIC on the
         fortran side.  */

      if(temptypes->token != 0)
        switch (temptypes->token)
        {
          case INTRINSIC:
            type_insert(&(intrinsic_table->entry[index]), 
               tempnames, return_type, tempnames->astnode.ident.name);
            break;
          case EXTERNAL:
            type_insert(&(external_table->entry[index]), tempnames, 
               return_type, tempnames->astnode.ident.name);
            break;
        } /* Close switch().  */
    }  /* Close inner for() loop.  */
  }    /* Close outer for() loop.  */
}      /* Close type_hash().       */


/*  Since jasmin doesn't have any EXPONENTIAL data types, these
   have to be turned into floats.  exp_to_double really just
   replaces instances of 'd' and 'D' in the exponential number
   with 'e' so that c can convert it on a string scan and
   string print.  Java does recognize numbers of the
   form 1.0e+1, so the `d' and `d' need to be replaced with
   `e'.  For now, leave as double for uniformity with jasmin.  */
void 
exp_to_double (char *lexeme, char *temp)
{
    float tempnum;
    char *cp = lexeme;
    while (*cp)                /* While *cp != '\0'...  */
      {
         if (*cp == 'd' ||     /*  sscanf can recognize 'E'. */
             *cp == 'D')
           {
              *cp = 'e';       /* Replace the 'd' or 'D' with 'e'. */
              break;           /* Should be only one 'd', 'D', etc. */
           }
         cp++;                 /* Examine the next character. */
      }
    /* Java should be able to handle exponential notation as part
       of the float or double constant. */
   if(JAS) {
     sscanf(lexeme,"%e", &tempnum); /* Read the string into a number.  */
     sprintf(temp,"%f", tempnum);   /* Reformat the number into a string. */
   } else {
     strcpy(temp,lexeme);
   }

}  /*  Close exp_to_double().  */


/* Initialize and fill a table with the names of the
   variables passed in as arguments to the function or
   subroutine.  This table is later checked when variable
   types are declared so that variables are not declared
   twice.  */

void
arg_table_load(AST * arglist)
{
   char * hashid;
   int index;
   AST * temp;
   extern SYMTABLE * args_table;

   /* We traverse down `prevstmt' because the arglist is
      built with right recursion, i.e. in reverse.  This
      procedure, 'arg_table_load()' is called when the non-
      terminal `functionargs' is reduced, before the
      argument list is reversed. Note that a NULL pointer
      at either end of the list terminates the for() loop. */
   for(temp = arglist; temp; temp = temp->nextstmt)
     {
       hashid = temp->astnode.ident.name;
       index = hash(hashid) % args_table->num_entries;
       type_insert(&(args_table->entry[index]), temp, 0,
             temp->astnode.ident.name);
       if(debug)printf("Arglist var. name: %s\n", temp->astnode.ident.name);
     }

}


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


/* Horribly kludged routines with massive loop of
   duplicated code.  */
void
assign_local_vars(AST * root)
{
  AST * locallist, * declist;
  HASHNODE * hashtemp;
  extern SYMTABLE * type_table, * args_table;
  static int localnum = 0;
  extern int locals;

locallist = root;

if (root->nodetype == Typedec || root->nodetype == Specification)
  {
    for (; locallist; locallist = locallist->nextstmt)
      {
	declist = locallist->astnode.typeunit.declist;
	for (; declist; declist = declist->nextstmt)
	  {
	    if(debug)printf("dec list name: %s\n", declist->astnode.ident.name);
            hashtemp = type_lookup(type_table, declist->astnode.ident.name);
            if(hashtemp == NULL)
	     {
	       fprintf(stderr,"Type table is screwed in assign locals.\n");
	       exit(-1);
             }
            if(hashtemp->localvarnum > -1)
	      {
	       /* printf("Duplicate local found.\n"); */
	       continue;
	      }
  
             /* Check to see if it is a double, but make sure it isn't
	        an array of doubles. */
            if (hashtemp->type == Double &&
	    hashtemp->variable->astnode.ident.arraylist == NULL)
            {
              hashtemp->localvarnum = localnum;
              hashtemp->variable->astnode.ident.localvnum = localnum;
	      if(debug)printf("%s %d\n", hashtemp->variable->astnode.ident.name, localnum);
              localnum += 2;
           }
           else
           {
             hashtemp->localvarnum = localnum;
              hashtemp->variable->astnode.ident.localvnum = localnum;
	     if(debug)printf("%s %d\n", hashtemp->variable->astnode.ident.name, localnum); 
             localnum++;
           }
        }
      }
  }

/*  I added this else {} to block this code out.  It was
    executing after the previous loop.  Also, for some 
    reason, the `localvnum' field of the of the hashed
    ident structure is not being initialized properly. */
 else 
  {
	
/*  This loop takes care of the stuff coming in from the
    argument list.  */
  for (; locallist; locallist = locallist->nextstmt)
    {

      if(debug)printf("arg list name: %s\n", locallist->astnode.ident.name);
      hashtemp = type_lookup(type_table, locallist->astnode.ident.name);
      if(hashtemp == NULL)
	{
	  fprintf(stderr,"Type table is screwed in assign locals.\n");
	  exit(-1);
	}
      if(hashtemp->localvarnum > -1)
	{
	  /* printf("Duplicate local found.\n"); */
	  continue;
	}
  
      /* Check to see if it is a double, but make sure it isn't
	 an array of doubles. */
      if (hashtemp->type == Double &&
	  hashtemp->variable->astnode.ident.arraylist == NULL)
        {
          hashtemp->localvarnum = localnum;
              hashtemp->variable->astnode.ident.localvnum = localnum;
	  if(debug)printf("%s %d\n", hashtemp->variable->astnode.ident.name, 
                                     localnum);
          localnum += 2;
        }
      else
        {
          hashtemp->localvarnum = localnum;
              hashtemp->variable->astnode.ident.localvnum = localnum;
	  if(debug)printf("%s %d\n", hashtemp->variable->astnode.ident.name, 
                                     localnum); 
          localnum++;
        }
     }
  }
locals = localnum;
} /* Close assign_local_vars().  */


/* We need to make a table of array variables, because
   fortran accesses arrays by columns instead of rows
   as C and java does.  During code generation, the array
   variables are emitted in reverse to get row order. */
void
store_array_var(AST * var)
{
  extern SYMTABLE * array_table;
  char * hashid;
  int index;

  hashid = var->astnode.ident.name;
  index = hash(hashid) % array_table->num_entries;
  type_insert(&(array_table->entry[index]), var, 0,
     var->astnode.ident.name);

  if(debug)
    printf("Array name: %s\n", var->astnode.ident.name);
}

/*
 * integer power function.  writing this here so that we
 * dont have to include the math library.
 */
int
mypow(int x, int y)
{
  int i;

  if(y < 0)
  {
    fprintf(stderr,"Warning: got negative exponent in mypow!\n");
    return 0;
  }

  if(y == 0)
    return 1;

  if(y == 1)
    return x;
  
  for(i=0;i<y-1;i++)
    x *= x;
  
  return x;
}

void
init_tables()
{
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
  equivList       = NULL;
}

void
merge_common_blocks(AST *root)
{
  HASHNODE *ht;
  AST *Clist, *temp;
  int idx, count;
  char ** name_array;
  char *comvar = NULL, *var = NULL, und_var[80], 
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

    name_array = (char **) malloc( count * sizeof(name_array) );
    if(name_array == NULL) {
      perror("Unsuccessful malloc");
      exit(1);
    }

    for(temp=Clist->astnode.common.nlist, count = 0; 
               temp!=NULL; temp=temp->nextstmt, count++) 
    {
      var = temp->astnode.ident.name;

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

      if(ht == NULL)
        name_array[count] = strdup(var);
      else {
        if(!strcmp(var,comvar) || 
             strstr(comvar,und_var_und) ||
             (((t=strstr(comvar,var_und)) != NULL) && t == comvar) ||
             (((t=strstr(comvar,und_var)) != NULL) && 
               (t+strlen(t) == comvar+strlen(comvar))))
        {
          name_array[count] = strdup(comvar);
        }
        else {
          name_array[count] = (char *) malloc(strlen(temp->astnode.ident.name) 
             + strlen(((char **)ht->variable)[count]) + 2);
  
          if(name_array[count] == NULL) {
             perror("Unsuccessful malloc");
             exit(1);
          }
  
          strcpy(name_array[count],temp->astnode.ident.name);
          strcat(name_array[count],"_");
          strcat(name_array[count],((char **)ht->variable)[count]);
        }
      }
    }

    idx = hash(Clist->astnode.common.name)%common_block_table->num_entries;
    type_insert(&(common_block_table->entry[idx]), (AST *)name_array, 
       Float, Clist->astnode.common.name);
  }
}

void
addEquiv(AST *node)
{
  static int id = 1;

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

int
eval_const_expr(AST *root, int dims)
{
  HASHNODE *p;
  int result1, result2;

  switch (root->nodetype)
  {
    case Identifier:
      if(!strcmp(root->astnode.ident.name,"*"))
        return 0;

      p = type_lookup(parameter_table, root->astnode.ident.name);

      if(p == NULL)
      {
        /* The array size is specified with a variable, but we
         * cant find it in the parameter table.  it is probably
         * an argument to the function.  do nothing here, just
         * fall through and hit the 'return 0' below.  --keith
         */
      }
      else
      {
         if(p->variable->nodetype == Constant)
           return ( atoi(p->variable->astnode.constant.number) );
         else 
           if(dims == 3)
             fprintf(stderr,"Cant determine array dimensions!\n");
      }
      return 0;
      break;
    case Expression:
      if (root->astnode.expression.lhs != NULL)
        result1 = eval_const_expr (root->astnode.expression.lhs, dims);

      result2 = eval_const_expr (root->astnode.expression.rhs, dims);
      return (result2);
      break;
    case Power:
      result1 = eval_const_expr (root->astnode.expression.lhs, dims);
      result2 = eval_const_expr (root->astnode.expression.rhs, dims);
      return( mypow(result1,result2) );
      break;
    case Binaryop:
      result1 = eval_const_expr (root->astnode.expression.lhs, dims);
      result2 = eval_const_expr (root->astnode.expression.rhs, dims);
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
      return 0;
      break;
    case Unaryop:
      result1 = eval_const_expr (root->astnode.expression.rhs, dims);
      if(root->astnode.expression.minus == '-')
        return -result1;
      break;
    case Constant:
      if(root->token == STRING)
        fprintf (stderr, "String in array dec!\n");
      else
        return( atoi(root->astnode.constant.number) );
      break;
    case ArrayIdxRange:
      return(  eval_const_expr(root->astnode.expression.rhs, dims) - 
               eval_const_expr(root->astnode.expression.lhs, dims) );
      break;
    default:
      fprintf(stderr,"eval_const_expr(): bad nodetype!\n");
      return 0;
  }
  return 0;
}
