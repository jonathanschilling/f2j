%{
#include<stdio.h>
#include "f2j.h"
#include<string.h>

#define YYDEBUG 0
#define TYPECHECK

extern char yytext[]; 
extern enum contexts context;
extern BOOLEAN typedecs;
/* extern Dlist tokenstack; */


/* Some of these are probably not necessary.  Need to
   run gcc with -Wall to filter unused variables. */
char *strdup(const char *);
void yyerror(char *);
AST * addnode();
AST * switchem();
char funname[30];
char tempname[30];
char * tname;
int temptok;
char * lowercase(char * );
void start_vcg();
void emit();
void jas_emit();
AST * tempnode;
AST * headnode;
AST * localvarlist; 
enum returntype typetemp;

SYMTABLE *ident_table; 
SYMTABLE *jasmin_table;

int emittem = 1;
int debug = 0;

char *retstring[] =
{"String", "complex", "double", "float", "int", "boolean"};

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
%token  RELOP  EQV
%token <lexeme>  NAME DOUBLE INTEGER EXPONENTIAL 
%token CONST TrUE FaLSE ICON RCON LCON CCON
%token FLOAT CHARACTER LOGICAL COMPLEX NONE

/* a zillion keywords */
%token IF THEN ELSE ELSEIF ENDIF DO GOTO ASSIGN TO CONTINUE STOP
%token RDWR END  STRING CHAR
%token OPEN CLOSE BACKSPACE REWIND ENDFILE FORMAT
%token PROGRAM FUNCTION SUBROUTINE ENTRY END CALL RETURN
%token <type> TYPE  
%token DIMENSION
%token COMMON EQUIVALENCE EXTERNAL PARAMETER INTRINSIC IMPLICIT
%token SAVE DATA COMMENT WRITE FMT EDIT_DESC REPEAT

%left EQV
%left OR
%left AND
%nonassoc NOT
%nonassoc RELOP 
%left CAT
%left PLUS MINUS
%left STAR DIV
%right POW
%nonassoc UMINUS


/*  All of my additions or changes to Levine's code. These 
non-terminals are in alphabetic order because I have had to 
change the grammar quite a bit.  It is tiring trying to root
out the location of a non-terminal, much easier to find when
in alphabetic order. */

%type <ptnode> Arraydeclaration Arrayname Arraynamelist Assignment
%type <ptnode> Arrayindexlist Arrayindex Arrayindexop
%type <ptnode> Binaryop Blockif Boolean
%type <ptnode> Call /* Char */ Complex Constant  Constantlist Continue
%type <ptnode> Data DataList DataConstant DataItem DataElement Do_incr Doloop 
%type <ptnode> Do_vals Do_statements Do_statement Double
%type <ptnode> Else Elseif Elseifs End Exp Explist Exponential External
%type <ptnode> Function Functionargs F2java
%type <ptnode> Fprogram Ffunction Fsubroutine
%type <ptnode> Goto Common CommonList CommonSpec ComputedGoto
%type <ptnode> Implicit Integer Intlist Intrinsic
%type <ptnode> Label Lhs Logicalop Logicalif  Logicalifstmts
%type <ptnode> Name Namelist
%type <ptnode> Parameter  Pdec Pdecs Program 
%type <ptnode> Relationalop Return 
%type <ptnode> Save Specstmt Specstmts Statements Statement Subroutinecall
%type <ptnode> Sourcecodes  Sourcecode Star /* Startindex */  
%type <ptnode> String  Subroutine Stop SubstringOp
%type <ptnode> Typestmt Typevar Typevarlist
%type <type>   Types Type
%type <ptnode> Write WriteFileDesc FormatSpec
%type <ptnode> Format FormatExplist FormatExp FormatSeparator
%type <ptnode> RepeatableItem UnRepeatableItem RepeatSpec 


%%

/*  The new stuff is here.  */
F2java:   Sourcecodes
          {
            if(debug)
              printf("F2java -> Sourcecodes\n");
	    $$ = addnode();
	    $$ = switchem($1);
          }
;


Sourcecodes:   Sourcecode 
               {
                 if(debug)
                   printf("Sourcecodes -> Sourcecode\n"); 
                 $$=$1;
               }
             | Sourcecodes Sourcecode 
               {
                 if(debug)
                   printf("Sourcecodes -> Sourcecodes Sourcecode\n");
                 $2->prevstmt = $1; 
                 $$=$2;
               }
;



Sourcecode :    Fprogram
                { 
                  if(debug)
                    printf("Sourcecode -> Fprogram\n"); 
                  array_table  = (SYMTABLE *) new_symtable (211);
                  format_table = (SYMTABLE *) new_symtable (211);
                  data_table   = (SYMTABLE *) new_symtable (211);
                  save_table   = (SYMTABLE *) new_symtable (211);
                  common_table = (SYMTABLE *) new_symtable (211);
                }
              | Fsubroutine
                { 
                  if(debug)
                    printf("Sourcecode -> Fsubroutine\n"); 
                  array_table  = (SYMTABLE *) new_symtable (211);
                  format_table = (SYMTABLE *) new_symtable (211);
                  data_table   = (SYMTABLE *) new_symtable (211);
                  save_table   = (SYMTABLE *) new_symtable (211);
                  common_table = (SYMTABLE *) new_symtable (211);
                }
              | Ffunction
                { 
                  if(debug)
                    printf("Sourcecode -> Ffunction\n"); 
                  array_table  = (SYMTABLE *) new_symtable (211);
                  format_table = (SYMTABLE *) new_symtable (211);
                  data_table   = (SYMTABLE *) new_symtable (211);
                  save_table   = (SYMTABLE *) new_symtable (211);
                  common_table = (SYMTABLE *) new_symtable (211);
                }
;

Fprogram:   Program  Specstmts  Statements End 
              {
                if(debug)
                  printf("Fprogram -> Program  Specstmts  Statements End\n");
		$2 = switchem($2);
        	type_hash($2); 
                $$ = addnode();
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

#if VCG
                if(emittem) start_vcg($$);
#endif

                if(JAS) {
		  assign_local_vars(localvarlist); 
		  assign_local_vars($2); 
                  assign($$); 
                  if(emittem) jas_emit($$);
                }else {
                  if(emittem) {
#ifdef TYPECHECK
                    typecheck($$);
#endif
                    emit($$);
                  }
                }
              }
;


Fsubroutine: Subroutine Specstmts Statements End 
              {
                if(debug)
                  printf("Fsubroutine -> Subroutine Specstmts Statements End\n");
                $$ = addnode();
	        $1->parent = $$; 
	        $2->parent = $$;
	        $3->parent = $$;
	        $4->parent = $$;
                $$->nodetype = Progunit;
                $$->astnode.source.progtype = $1;
		$2 = switchem($2);
        	type_hash($2); 
                $$->astnode.source.typedecs = $2;
                $4->prevstmt = $3;
                $$->astnode.source.statements = switchem($4);
#if VCG
                if(emittem) start_vcg($$);
#endif
                if(JAS) {
		  assign_local_vars(localvarlist);
		  assign_local_vars($2);
                  assign($$);
                  if(emittem) jas_emit($$);
                }else {
                  if(emittem) {
#ifdef TYPECHECK
                    typecheck($$);
#endif
                    emit($$);
                  }
                }
              }
;

Ffunction:   Function Specstmts Statements  End
              {
                if(debug)
                  printf("Ffunction ->   Function Specstmts Statements  End\n");
                $2 = switchem($2);
		type_hash($2);
                $$ = addnode();
	        $1->parent = $$; /* 9-4-97 - Keith */
	        $2->parent = $$; /* 9-4-97 - Keith */
	        $3->parent = $$; /* 9-4-97 - Keith */
	        $4->parent = $$; /* 9-4-97 - Keith */
                $$->nodetype = Progunit;
                $$->astnode.source.progtype = $1;
                $$->astnode.source.typedecs = $2;
		$4->prevstmt = $3;
                $$->astnode.source.statements = switchem($4);

#if VCG
                if(emittem) start_vcg($$);
#endif
                if(JAS) {
		  assign_local_vars(localvarlist);
		  assign_local_vars($2);
                  assign($$);
                  if(emittem) jas_emit($$);
                }else {
                  if(emittem) {
#ifdef TYPECHECK
                    typecheck($$);
#endif
                    emit($$);
                  }
                }
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
              }
;

/*  Subroutine is handled correctly.  */
Subroutine:   SUBROUTINE Name Functionargs NL
              {
                 int index;
                 char * hashid;

                 if(debug)
                   printf("Subroutine ->  SUBROUTINE Name Functionargs NL\n");
                 $$ = addnode();
                 $2->parent = $$; /* 9-4-97 - Keith */
                 $3->parent = $$; /* 9-4-97 - Keith */
                 lowercase($2->astnode.ident.name);
                 $$->astnode.source.name = $2; 
                 $$->nodetype = Subroutine;
                 $$->token = SUBROUTINE;
                 $$->astnode.source.args = switchem($3);
                 hashid = $$->astnode.source.name->astnode.ident.name;
                 index = hash(hashid) % function_table->num_entries;
                 type_insert(&(function_table->entry[index]), $$, NULL,
                    $$->astnode.source.name->astnode.ident.name);
              }
;

Function:  Type FUNCTION Name Functionargs NL 
           {
             int index;
             char * hashid;

             if(debug)
               printf("Function ->  Type FUNCTION Name Functionargs NL\n");
             $$ = addnode();

  	     $3->parent = $$;  /* 9-4-97 - Keith */
  	     $4->parent = $$;  /* 9-4-97 - Keith */
             $$->astnode.source.name = $3;
             $$->nodetype = Function;
             $$->token = FUNCTION;
             $$->astnode.source.returns = $1;
#ifdef TYPECHECK
             $$->vartype = $1;
#endif
             $$->astnode.source.args = switchem($4);

             hashid = $$->astnode.source.name->astnode.ident.name;
             index = hash(hashid) % function_table->num_entries;
             type_insert(&(function_table->entry[index]), $$, NULL,
               $$->astnode.source.name->astnode.ident.name);
           }
; 

Specstmts: Specstmt  
           {
             $$=$1;
           }
         | Specstmts  Specstmt 
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
         | EQUIVALENCE
	   {
	    $$ = 0;
	    fprintf(stderr,"EQUIVALENCE is not implemented.\n");
	    exit(-1);
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
;

Common:     COMMON CommonList NL
            {
              $$ = switchem($2);
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
              int idx;

              $$ = addnode();
              $$->nodetype = Common;
              $$->astnode.common.name = strdup($2->astnode.ident.name);
              $$->astnode.common.nlist = switchem($4);

              for(temp=$4;temp!=NULL;temp=temp->prevstmt) {
                temp->astnode.ident.commonBlockName = 
                  strdup($2->astnode.ident.name);
                idx = hash(temp->astnode.ident.name)%common_table->num_entries;
                if(debug)
                  printf("@@insert %s (block = %s) into common table (idx=%d)\n",
                   temp->astnode.ident.name, $2->astnode.ident.name, idx);
                type_insert(&(common_table->entry[idx]), temp, Float,
                   temp->astnode.ident.name);
              }
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
           }
;

Save:   SAVE Namelist NL
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

DataItem:   Namelist DIV Constantlist DIV
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
                idx = hash(temp->astnode.ident.name) % data_table->num_entries;
                if(debug)
                  printf("@@insert %s into data table\n",
                     temp->astnode.ident.name);
                type_insert(&(data_table->entry[idx]), temp, Float,
                   temp->astnode.ident.name);
              }
            }
;

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
            | Stop
              {
                $$ = $1;
                $$->nodetype = Stop;
              }
;           

End:    END  NL 
        {
          $$ = addnode();
	  $$->token = END;
          $$->nodetype = End;
        }
;

/* We have to load up a symbol table here with the names of all the
   variables that are passed in as arguments to our function or
   subroutine.  Also need to pass `namelist' off to a procedure
   to load a local variable table for opcode generation.   */
Functionargs:   OP Namelist CP   
                {
                  if(JAS)
  		    localvarlist = switchem($2);
                  else
		    $2 = switchem($2);  
		  arg_table_load($2);
                  $$ = $2;
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

/* Somewhere in the actions associated with this production,
   I need to ship off the type and variable list to get hashed.
   Also need to pass `typevarlist' off to a procedure
   to load a local variable table for opcode generation.
   */
Typestmt:      Types Typevarlist NL
              {
                 $$ = addnode();
	         $2->parent = $$; /* 9-4-97 - Keith */
                 /* store_local_var($2);  */
                 $2 = switchem($2);
                 $$->nodetype = Typedec;
                 $$->astnode.typeunit.declist = $2;
                 $$->astnode.typeunit.returns = $1; 
	       }
;


Types:       Type 
          |  Type Star Integer
	  |  Type Star OP Star CP
;

Type:  TYPE { $$ = yylval.type;   }
;

/* Here I'm going to do the same thing I did with Explist.  That is,
   each element in the list of typevars will have a parent link to a 
   single node indicating that the context of the array is a
   declaration.  --Keith */

Typevarlist:     Typevar
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
    lexer.  This CHAR and STRING stuff is in the wrong place and
    needs to get axed.  Putting the TYPE back in ...
          ^^^^^^^^^^^ it is commented out for now 9-12-97, Keith
                   moved to 'Constant' production 9-17-97, Keith
 */

/*  Might have to explicitly set the arraydeclist pointer to
    NULL in this action.  `Name' gets pointed to by the node
    that carries the array information.
    */
Name:    NAME  
         {
           HASHNODE *hashtemp;

           $$=addnode();
	   $$->token = NAME;
           $$->nodetype = Identifier;
           strcpy($$->astnode.ident.name, strdup(lowercase(yylval.lexeme)));
#ifdef TYPECHECK
    hashtemp = type_lookup(type_table, $$->astnode.ident.name);
    if(hashtemp)
      $$->vartype = hashtemp->variable->vartype;
    else
      $$->vartype = -1;
#endif
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
           strcpy($$->astnode.ident.name, yylval.lexeme);
         }
;
*/

String:  STRING
         {
           $$=addnode();
           $$->token = STRING;
           $$->nodetype = Identifier;
           strcpy($$->astnode.ident.name, yylval.lexeme);
#ifdef TYPECHECK
           $$->vartype = Character;
#endif
           if(debug)
             printf("**The string value is %s\n",$$->astnode.ident.name);
         }
       | CHAR
         {
           $$=addnode();
           $$->token = STRING;
           $$->nodetype = Identifier;
           strcpy($$->astnode.ident.name, yylval.lexeme);
#ifdef TYPECHECK
           $$->vartype = Character;
#endif
           if(debug)
             printf("**The string value is %s\n",$$->astnode.ident.name);
         }
;

Arraydeclaration: Name OP Arraynamelist CP 
                  {
		    /*
                    $$ = addnode();
		    $$->nodetype = Identifier;
		    strcpy($$->astnode.ident.name, $1->astnode.ident.name);
		    */
		    $$ = $1;
		    $$->astnode.ident.arraylist = switchem($3);
                  
                    /* leaddim might be a constant, so check for that.  --keith */
                    if($$->astnode.ident.arraylist->nodetype == Constant) {
		      $$->astnode.ident.leaddim = 
                        strdup ($$->astnode.ident.arraylist->astnode.constant.number);
                    } else {
		      $$->astnode.ident.leaddim = 
                          strdup ($$->astnode.ident.arraylist->astnode.ident.name);
                    }
		    store_array_var($$);
                  }

Arraynamelist:    Arrayname 
                  {
                    AST *temp;

                    temp = addnode();
                    temp->nodetype = Typedec;
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

Arrayname:   Name {$$=$1;}
           | Star {$$=$1;}
           | Integer {$$=$1;}
;

/*  We reduce STAR here, make changes in the Binaryops
    reductions for that.  This handles the fortran array
    declaration, e.g., array(*).  */
Star:  STAR 
       {
         $$=addnode();
         $$->nodetype = Identifier;
        *$$->astnode.ident.name = '*';
       }
;

/*  At some point, I will need to typecheck the `Name' on the left
    hand side of this rule in case it has an array form.  If it looks like
    an array, but it isn't in the array table, that's an error. 
 */
Assignment:  Lhs  EQ Exp /* NL (Assignment is also used in the parameter
                             declaration, where it is not followed by a NL.
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

Lhs:     Name {$$=$1;} 
      |  Name OP Arrayindexlist CP
         {
	   HASHNODE * hashtemp;  /* In case we need to switch index order. */
	   $$ = addnode();
	   $1->parent = $$; /* 9-4-97 - Keith */
	   $3->parent = $$; /* 9-4-97 - Keith */
	   $$->nodetype = Identifier;
	   strcpy($$->astnode.ident.name, $1->astnode.ident.name);
	   /*  This is in case we want to switch index order later. */
	   /*
	   hashtemp = type_lookup(array_table, $1->astnode.ident.name);
	   if(hashtemp)
	     $$->astnode.ident.arraylist = $3;
	   else
	   */
	   /* We don't switch index order.  */
	   $$->astnode.ident.arraylist = switchem($3);
	 }
;


/*  This whole deal with these arrayindex operations is a
    massive kludge.  The grammar shadows that of expressions,
    but trying to combine the two resulted in terrible
    shift reduce errors.  */
Arrayindexlist:   Arrayindexop 
                  { 
                    AST *temp;

                    temp = addnode();
                    temp->nodetype = Identifier;
                    $1->parent = temp;

                    $$ = $1;
                  }
                | Arrayindexlist CM Arrayindexop
                  {
                    $3->prevstmt = $1;
                    $3->parent = $1->parent;
		    $$ = $3;
		  }
;

Arrayindex:    Name {$$=$1;}
             | Integer {$$=$1;}
/*             | Arrayindexop {$$ = $1;}  */
;

Arrayindexop:  Arrayindex
             | OP Arrayindexop CP 
               { 
                  $$=$2;
               }       
             | Arrayindexop PLUS Arrayindexop
               {
		  $$=addnode();
	          $1->parent = $$; /* 9-4-97 - Keith */
	          $3->parent = $$; /* 9-4-97 - Keith */
		  $$->astnode.expression.lhs = $1;
		  $$->astnode.expression.rhs = $3;
		  $$->nodetype = Binaryop;
		  $$->astnode.expression.optype = '+';
	       }
              | Arrayindexop MINUS Arrayindexop
              {
		  $$=addnode();
	          $1->parent = $$; /* 9-4-97 - Keith */
	          $3->parent = $$; /* 9-4-97 - Keith */
		  $$->astnode.expression.lhs = $1;
		  $$->astnode.expression.rhs = $3;
		  $$->nodetype = Binaryop;
		  $$->astnode.expression.optype = '-';
	       }
              | Arrayindexop STAR Arrayindexop
              {
		  $$=addnode();
	          $1->parent = $$; /* 9-4-97 - Keith */
	          $3->parent = $$; /* 9-4-97 - Keith */
		  $$->astnode.expression.lhs = $1;
		  $$->astnode.expression.rhs = $3;
		  $$->nodetype = Binaryop;
		  $$->astnode.expression.optype = '*';
	       }
              | Arrayindexop DIV Arrayindexop
              {
                  /* this production added 10-8-97 --Keith */
		  $$=addnode();
	          $1->parent = $$; /* 9-4-97 - Keith */
	          $3->parent = $$; /* 9-4-97 - Keith */
		  $$->astnode.expression.lhs = $1;
		  $$->astnode.expression.rhs = $3;
		  $$->nodetype = Binaryop;
		  $$->astnode.expression.optype = '/';
	       }
;

/*  New do loop productions.  Entails rewriting in codegen.c
    to emit java source code.  */
Doloop:   Do_incr Do_vals Do_statements  Continue /* Integer CONTINUE  NL */
          {
            $$ = $2;
            $$->nodetype = Forloop;
            $3 = switchem($3);
            $$->astnode.forloop.stmts = $3;
            $$->astnode.forloop.Label = $1;
            $$->astnode.forloop.Continue = $4;
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
Startindex:  Exp {$$=$1;}
;
*/

Do_statements:   Do_statement 
                 { 
                   $$=$1;
                 }
               | Do_statements  Do_statement 
                 {
                   $2->prevstmt = $1;
                   $$ = $2;
                 }
;

Do_statement:    Assignment NL {$$=$1; $$->nodetype = Assignment;}
               | Call {$$=$1; $$->nodetype = Call;}
               | Logicalif {$$=$1; $$->nodetype = Logicalif;}
               | Blockif {$$=$1; $$->nodetype = Blockif;}
               | Doloop {$$=$1; $$->nodetype = Forloop;}
               | Return {$$=$1; $$->nodetype = Return;}
               | Goto {$$=$1; $$->nodetype = Goto;}
               | Label {$$=$1; $$->nodetype = Label;}
               | Write {$$=$1; $$->nodetype = Write;}
;           


Label: Integer Doloop 
       {
         $$ = addnode();
	 $1->parent = $$; /* 9-4-97 - Keith */
	 $2->parent = $$; /* 9-4-97 - Keith */
	 $$->nodetype = Label;
	 $$->astnode.label.number = atoi($1->astnode.constant.number);
	 $$->astnode.label.stmt = $2;
       }
      | Integer Assignment NL
       {
         $$ = addnode();
	 $1->parent = $$; /* 9-4-97 - Keith */
	 $2->parent = $$; /* 9-4-97 - Keith */
	 $$->nodetype = Assignment;
	 $$->astnode.label.number = atoi($1->astnode.constant.number);
	 $$->astnode.label.stmt = $2;
       }
      | Integer Format NL 
       {
         $$ = addnode();
	 $1->parent = $$; /* 9-4-97 - Keith */
	 $2->parent = $$; /* 9-4-97 - Keith */
	 $$->nodetype = Format;
	 $$->astnode.label.number = atoi($1->astnode.constant.number);
	 $$->astnode.label.stmt = $2;
         $2->astnode.label.number = $$->astnode.label.number;
         if(debug)
           printf("@@ inserting format line num %d\n",$$->astnode.label.number);
         hash_insert(format_table,$2);
       }
      | Integer Return
       {
         $$ = addnode();
	 $1->parent = $$; /* 9-4-97 - Keith */
	 $2->parent = $$; /* 9-4-97 - Keith */
	 $$->nodetype = Return;
	 $$->astnode.label.number = atoi($1->astnode.constant.number);
         $2->nodetype = Return;
	 $$->astnode.label.stmt = $2;
       }
      | Integer Write
       {
         $$ = addnode();
	 $1->parent = $$; /* 9-4-97 - Keith */
	 $2->parent = $$; /* 9-4-97 - Keith */
	 $$->nodetype = Write;
	 $$->astnode.label.number = atoi($1->astnode.constant.number);
         $2->nodetype = Write;
	 $$->astnode.label.stmt = $2;
       }
;

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

Write: WRITE OP WriteFileDesc CM FormatSpec CP Explist NL
       {
         AST *temp;

         $$ = addnode();
         $$->astnode.io_stmt.io_type = Write;

         /*  unimplemented
           $$->astnode.io_stmt.file_desc = ;
         */

         if($5->astnode.constant.number[0] == '*') 
           $$->astnode.io_stmt.format_num = -1;
         else
           $$->astnode.io_stmt.format_num = atoi($5->astnode.constant.number);
         $$->astnode.io_stmt.arg_list = switchem($7);

         for(temp=$$->astnode.io_stmt.arg_list;temp!=NULL;temp=temp->nextstmt)
           temp->parent->nodetype = Write;
       }
     | WRITE OP WriteFileDesc CM FormatSpec CP NL
       {
         $$ = addnode();
         $$->astnode.io_stmt.io_type = Write;

         /*  unimplemented
           $$->astnode.io_stmt.file_desc = ;
         */

         if($5->astnode.constant.number[0] == '*') 
           $$->astnode.io_stmt.format_num = -1;
         else
           $$->astnode.io_stmt.format_num = atoi($5->astnode.constant.number);
         $$->astnode.io_stmt.arg_list = NULL;
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


Logicalif: IF OP Exp CP Logicalifstmts   
           {
             $$ = addnode();
             $3->parent = $$;
             $5->parent = $$; /* 9-4-97 - Keith */
             $$->astnode.logicalif.conds = $3;
             $$->astnode.logicalif.stmts = $5;
           }           
;


Logicalifstmts:  Assignment NL {
                   $$=$1;
                   $$->nodetype = Assignment;
                 }
                | Return
                  {
                    $$=$1;
		    /*  if(debug)printf("Return from lif.\n");  */
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

/* This _may_ have to be extended to deal with jasmin opcode. 
   Variables of type array need to have their
   arguments emitted in reverse order so that
   java can increment in row instead of column
   order.  So we look each name up in the array
   table, it is in there we leave the argument
   list reversed, otherwise, it is a subroutine
   or function (method) call and we reverse
   the arguments.  */
Subroutinecall:   Name OP Explist CP
                  {
                    HASHNODE * hashtemp;  /* In case we need to switch index order. */
                    $$ = addnode();
                    $1->parent = $$;  /* 9-4-97 - Keith */
                    /*  $3->parent = $$;  9-4-97 - Keith */

                    strcpy($3->parent->astnode.ident.name, 
                        $1->astnode.ident.name);

/*
                    if(type_lookup(array_table, $1->astnode.ident.name))
                      $$->nodetype = ArrayAccess;
                    else
*/
                      $$->nodetype = Identifier;

                    strcpy($$->astnode.ident.name, $1->astnode.ident.name);

                    /*  This is in case we want to switch index order later. */
                    /*
                       hashtemp = type_lookup(array_table, $1->astnode.ident.name);
                       if(hashtemp != NULL)
                         $$->astnode.ident.arraylist = $3;
                       else
                    */

                    /* We don't switch index order.  */
                    $$->astnode.ident.arraylist = switchem($3);
                  }
;

SubstringOp: Name OP Exp COLON Exp CP
           {
              if(debug)
                printf("SubString!\n");
              $$ = addnode();
              $1->parent = $$;
              strcpy($$->astnode.ident.name, $1->astnode.ident.name);
              $$->nodetype = Substring;
              $$->astnode.ident.arraylist = $3;
              $3->nextstmt = $5;
           }
;


/* What I'm going to try to do here is have each element
   of the list linked back to a single node through its
   parent pointer.  This will allow the code generator
   to check the array context (whether it is being used
   as part of an external call or part of a call to an
   intrinsic function or some other use). --Keith */

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
;


Logicalop:  Exp AND Exp 
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
          | Exp OR Exp  
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
          | NOT Exp   
              {
                $$=addnode();
                $2->parent = $$;  /* 9-4-97 - Keith */
		$$->token = NOT;
		$$->nodetype = Logicalop;
		$$->astnode.expression.lhs = 0;
		$$->astnode.expression.rhs = $2;
              }
;
                   

Relationalop: Exp RELOP {temptok = yylval.tok;} Exp
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

/*  I have tried factoring out the common blocks of code here,
    but I get 11 shift/reduce errors when I try.  See
    `Arithmeticop' below.  */
Binaryop: Exp PLUS Exp
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
;

            | Exp MINUS Exp
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
            | Exp STAR Exp
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
            | Exp DIV Exp
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
;

/*  For some reason, trying to factor out aritmetic operations
    from the above code results in 11 shift/reduce errors.
    The conflict most likely come from the `Arrayindex'
    productions above.  */
/*
Arithmeticop:   PLUS
                {
		  $$=addnode();
		  $$->astnode.expression.optype = '+';
		}
            |   MINUS
                {
		  $$=addnode();
		  $$->astnode.expression.optype = '-';
		}
            |   STAR
                {
		  $$=addnode();
		  $$->astnode.expression.optype = '*';
		}
            |   DIV
                {
		  $$=addnode();
		  $$->astnode.expression.optype = '/'
                }
;
*/

/*  This is not exactly right.  There will need to 
    be a struct to handle this.
 */
Call:     CALL   Subroutinecall  NL
          {
             $$ = $2;
	     $$->nodetype = Call;
          }
;


Exp:         Name {$$=$1;}
          |  Constant
             {
	       $$ = $1;
               $$->nodetype = Constant;
	     }
          |  Complex {$$=$1;}
          |  Subroutinecall {$$=$1;}    
          |  SubstringOp {$$=$1;}    
/*          |  Boolean  {$$=$1;}       */
          |  Relationalop {$$=$1; } 
          |  Logicalop {$$=$1;} 
          |  OP Exp CP  
             {
               $$ = addnode();
               $2->parent = $$;   /* 9-4-97 - Keith */
               $$->nodetype = Expression;
               $$->astnode.expression.parens = TRUE;
               $$->astnode.expression.rhs = $2;
               $$->astnode.expression.lhs = 0;
             }
          |  Binaryop {$$=$1;}
          |  Exp POW Exp
             {
               $$=addnode();
               $1->parent = $$;   /* 9-4-97 - Keith */
               $3->parent = $$;   /* 9-4-97 - Keith */
	       $$->nodetype = Power;
	       $$->astnode.expression.lhs = $1;
	       $$->astnode.expression.rhs = $3;
             }  
          |  Exp CAT Exp {$$=$1;}  /*  Look up def'n. */
          |  Exp EQV Exp {$$=$1;}  /*  Look up def'n. */
          |  MINUS Exp %prec UMINUS
             {
               $$ = addnode();
               $2->parent = $$;   /* 9-4-97 - Keith */
               $$->astnode.expression.rhs = $2;
               $$->astnode.expression.lhs = 0;
               $$->astnode.expression.minus = '-';   
               $$->nodetype = Unaryop;
             }
;

Complex: OP Constant CM Constant CP {$$=addnode();}
;

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
#ifdef TYPECHECK 
               $$->vartype = Logical;
#endif
             }
         | FaLSE
             {
               $$ = addnode();
               $$->token = FaLSE;
               $$->nodetype = Constant;
               strcpy($$->astnode.constant.number, "false");
               $$->astnode.constant.type = INTEGER;
               $$->astnode.constant.sign = 0;
#ifdef TYPECHECK 
               $$->vartype = Logical;
#endif
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
	       /*              $$->astnode.constant.type = INTEGER; */
	       $$->astnode.constant.type = Integer;
               $$->astnode.constant.sign = 0;
#ifdef TYPECHECK 
               $$->vartype = Integer;
#endif
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
#ifdef TYPECHECK 
               $$->vartype = Double;
#endif
             }
;
               
/*  Since java doesn't have an EXPONENTIAL data type,
    the function exp_to_double rewrite numbers in the
    nn.dde+nn as floats.  The float is written back into
    the string temp.  */
Exponential:   EXPONENTIAL
             {
               $$ = addnode();
	       $$->token = EXPONENTIAL;
               $$->nodetype = Constant;
	       exp_to_double(yylval.lexeme, tempname);
               strcpy($$->astnode.constant.number, tempname);
               $$->astnode.constant.type = EXPONENTIAL;
               $$->astnode.constant.sign = 0;
#ifdef TYPECHECK 
               $$->vartype = Double;
#endif
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
	    if(debug)
              printf("Parameter...\n");
	    $$ = $1;
	    $$->nodetype = Assignment;
	    /*
	    $$->astnode.typeunit.specification = Parameter; */
	    /* Attach the Assignment node to a list... Hack. */
	    /*	    $$->astnode.typeunit.declist = $1;  */
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


/*  Need to turn the linked list around,
so that it can traverse forward instead of in reverse.
What I do here is create a doubly linked list. 
Note that there is no `sentinel' or `head' node
in this list.  It is acyclic and terminates in 
NULL pointers.
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

/* For now, type_hash takes a tree (linked list) of type
   declarations from the Decblock rule.  It will need to
   get those from Intrinsic, External, Parameter, etc.
   */
int 
type_hash(AST * types)
{
  AST * temptypes, * tempnames;
  int return_type, index;
  char * hashid;
  extern SYMTABLE * type_table, * intrinsic_table, * external_table; 
  type_table = (SYMTABLE*)new_symtable(211);
  intrinsic_table = (SYMTABLE*)new_symtable(211);
  external_table = (SYMTABLE*)new_symtable(211);
   
   /* Outer for loop traverses typestmts, inner for()
      loop traverses declists. Code for stuffing symbol table is
      is in inner for() loop.  */
  for (temptypes = types; temptypes; temptypes = temptypes->nextstmt)
  {
      /* Long assignment, set up the for() loop here instead of
         the expression list.  */
    tempnames = temptypes->astnode.typeunit.declist;

      /* Need to set the return value here before entering
         the next for() loop.  */
    return_type = temptypes->astnode.typeunit.returns;

    for (tempnames; tempnames; tempnames = tempnames->nextstmt)
    {
      /* Stuff names and return types into the symbol table. */
      hashid = tempnames->astnode.ident.name;
      if(debug)printf("Type hash: %s\n", tempnames->astnode.ident.name);

      /*  Hash...  */
      index = hash(hashid) % type_table->num_entries;
      if(search_hashlist (type_table->entry[index], hashid) != NULL)
      {
        if(debug)printf("Duplicate entry.\n");  
        /*  exit(-1);  */
      }

#ifdef TYPECHECK
      tempnames->vartype = return_type;
#endif

      /* All names go into the name table.  */

      type_insert(&(type_table->entry[index]), tempnames, return_type,
          tempnames->astnode.ident.name);

      /* Now separate out the EXTERNAL from the INTRINSIC on the
         fortran side.  */

      if(temptypes->token != (int)NULL)
      switch (temptypes->token)
      {
        case INTRINSIC:
          type_insert(&(intrinsic_table->entry[index]), tempnames, return_type,
             tempnames->astnode.ident.name);
          break;
        case EXTERNAL:
          type_insert(&(external_table->entry[index]), tempnames, return_type,
             tempnames->astnode.ident.name);
          break;
      } /* Close switch().  */
    }  /* Close inner for() loop.  */
  }    /* Close outer for() loop.  */
}     /* Close type_hash().       */


/*  Since jasmin doesn't have any EXPONENTIAL data types, these
   have to be turned into floats.  exp_to_double really just
   replaces instances of 'd' and 'D' in the exponential number
   with 'e' so that c can convert it on a string scan and
   string print.  Java does recognize numbers of the
   form 1.0e+1, so the `d' and `d' need to be replaced with
   `e'.  For now, leave as double for uniformity with jasmin.  */
int 
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
   sscanf(lexeme,"%e", &tempnum); /* Read the string into a number.  */
   sprintf(temp,"%f", tempnum);   /* Reformat the number into a string. */

}  /*  Close exp_to_double().  */


/* Initialize and fill a table with the names of the
   variables passed in as arguments to the function or
   subroutine.  This table is later checked when variable
   types are declared so that variables are not declared
   twice.  */
arg_table_load(AST * arglist)
{
   char * hashid;
   int index;
   AST * temp;
   extern SYMTABLE * args_table;

   args_table = (SYMTABLE*)new_symtable(211);

   temp = arglist;

   /* We traverse down `prevstmt' because the arglist is
      built with right recursion, i.e. in reverse.  This
      procedure, 'arg_table_load()' is called when the non-
      terminal `functionargs' is reduced, before the
      argument list is reversed. Note that a NULL pointer
      at either end of the list terminates the for() loop. */
   for(temp; temp; temp = temp->nextstmt)
     {
       hashid = temp->astnode.ident.name;
       index = hash(hashid) % args_table->num_entries;
       type_insert(&(args_table->entry[index]), temp, NULL,
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
int
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
    for (locallist; locallist; locallist = locallist->nextstmt)
      {
	declist = locallist->astnode.typeunit.declist;
	for (declist; declist; declist = declist->nextstmt)
	  {
	    if(debug)printf("dec list name: %s\n", declist->astnode.ident.name);
            hashtemp = type_lookup(type_table, declist->astnode.ident.name);
            if(hashtemp == NULL)
	     {
	       if(debug)printf("Type table is screwed in assign locals.\n");
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
  for (locallist; locallist; locallist = locallist->nextstmt)
    {

      if(debug)printf("arg list name: %s\n", locallist->astnode.ident.name);
      hashtemp = type_lookup(type_table, locallist->astnode.ident.name);
      if(hashtemp == NULL)
	{
	  if(debug)printf("Type table is screwed in assign locals.\n");
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
  type_insert(&(array_table->entry[index]), var, NULL,
     var->astnode.ident.name);

  if(debug)
    printf("Array name: %s\n", var->astnode.ident.name);
}
