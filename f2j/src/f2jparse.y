%{
#include<stdio.h>
#include "f2j.h"
#include<string.h>

#define YYDEBUG 1

extern char yytext[]; 
extern enum contexts context;
extern BOOLEAN typedecs;
/* extern Dlist tokenstack; */


/* Some of these are probably not necessary.  Need to
   run gcc with -Wall to filter unused variables. */
void yyerror(char *);
AST * addnode();
AST * switchem();
char funname[30];
char tempname[30];
char * tname;
int temptok;
void emit();
AST * tempnode;
AST * headnode;
AST * localvarlist; 
enum returntype typetemp;

SYMTABLE *ident_table; 
SYMTABLE *jasmin_table;

int emittem = 1;
int debug = 0;

%}

%union {
       struct ast_node *ptnode;
       int tok;
       enum returntype type;
       char lexeme[30];
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
%token SAVE DATA

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
%type <ptnode> /* Arithmeticop */  Assignment 
%type <ptnode> Binaryop Blockif Boolean
%type <ptnode> Call Char Complex Constant  Constantlist Continue
%type <ptnode> Data Do_incr Doloop 
%type <ptnode> Do_vals Do_statements Do_statement Double
%type <ptnode> Else Elseif Elseifs End Exp Explist Exponential External
%type <ptnode> Function Functionargs
%type <ptnode> Goto 
%type <ptnode> Implicit Integer Intlist Intrinsic
%type <ptnode> Label Lhs Logicalop Logicalif  Logicalifstmts
%type <ptnode> Name Namelist
%type <ptnode> Parameter  Pdec Pdecs /* Power */ Program
%type <ptnode> Relationalop Return 
%type <ptnode> Save Specstmt Specstmts Statements Statement Subroutinecall
%type <ptnode> /* Sourcecodes */ Sourcecode Star /* Startindex */  String  Subroutine
%type <ptnode> Typestmt Typevar Typevarlist
%type <type>   Types Type


%%

/* Sourcecode is the start symbol.  Programs, subroutines and
 functions will all derive from program.
*/
/*
Sourcecodes: Sourcecode
           | Sourcecodes Sourcecode
;
*/

Sourcecode:   Program  Specstmts  Statements End 
              {
                $$ = addnode();
                $$->nodetype = Source;
                $$->astnode.source.progtype = $1;
		$2 = switchem($2);
        	type_hash($2); 
                $$->astnode.source.typedecs = $2;
                $4->prevstmt = $3;
                $$->astnode.source.statements = switchem($4);
#if JAS
		assign_local_vars(localvarlist); 
		assign_local_vars($2); 
                assign($$); 
#endif
                if(emittem) emit($$); 
              }

           |  Subroutine Specstmts Statements End 
              {
                $$ = addnode();
                $$->nodetype = Source;
                $$->astnode.source.progtype = $1;
		$2 = switchem($2);
        	type_hash($2); 
                $$->astnode.source.typedecs = $2;
                $4->prevstmt = $3;
                $$->astnode.source.statements = switchem($4);
#if JAS
		assign_local_vars(localvarlist); 
		assign_local_vars($2); 
                assign($$); 
#endif
                if(emittem) emit($$); 
              }
          |   Function Specstmts Statements  End
              {
                $2 = switchem($2);
		type_hash($2);
                $$ = addnode();
                $$->nodetype = Source;
                $$->astnode.source.progtype = $1;
                $$->astnode.source.typedecs = $2;
		$4->prevstmt = $3;
                $$->astnode.source.statements = switchem($4);
#if JAS
		assign_local_vars(localvarlist);
		assign_local_vars($2);
                assign($$);
#endif
                if(emittem) emit($$);    
              }
;

Program:      PROGRAM Name
              {
                 $$ = addnode();
		 lowercase($2->astnode.ident.name);
		 $$->astnode.source.name = $2;
              }
;

/*  Subroutine is handled correctly.  */
Subroutine:   SUBROUTINE Name Functionargs NL
              {
                 $$ = addnode();
                 lowercase($2->astnode.ident.name);
                 $$->astnode.source.name = $2; 
                 $$->nodetype = Subroutine;
		   $$->token = SUBROUTINE;
                 $$->astnode.source.args = switchem($3);
              }
;

Function:  Type FUNCTION Name Functionargs NL 
           {
             $$ = addnode();
             $$->astnode.source.name = $3;
             $$->nodetype = Function;
	      $$->token = FUNCTION;
             $$->astnode.source.returns = $1;
             $$->astnode.source.args = switchem($4);
           }
; 

Specstmts: Specstmt  {$$=$1;}
          | Specstmts  Specstmt { $2->prevstmt = $1; $$ = $2; }
;

Specstmt:  DIMENSION
           {
	    $$ = 0;
	    printf("DIMENSION is not implemented.\n");
	    exit(-1);
	   }
         | EQUIVALENCE
	   {
	    $$ = 0;
	    printf("EQUIVALENCE is not implemented.\n");
	    exit(-1);
	   }
         | COMMON
	   {
	    $$ = 0;
	    printf("COMMON is not implemented.\n");
	    exit(-1);
	   }
         | Save      {$$=$1;}
         | Intrinsic {$$=$1;}
         | Typestmt  {$$=$1;}
         | External  {$$=$1;}
         | Parameter {$$=$1;}
         | Implicit  {$$=$1;}
         | Data {$$=$1;}
;

Save:   SAVE Namelist NL
	   {
	    $$ = addnode();
	    $$->nodetype = Unimplemented;
	   }
;
Implicit:   IMPLICIT
            {
	      $$=0;
	      printf("Must use IMPLICIT NONE.\n");
	      exit(-1);
	    }
         |  IMPLICIT NONE
            {
	      $$=addnode();
	      $$->nodetype = Specification;
	      $$->token = IMPLICIT;
	      $$ = 0;
	    }
;


/*  DATA statement hasn't really been implemented.
    This is just some dummy code to allow parsing. */
Data:       DATA Namelist DIV Constantlist DIV NL
            { 
              $$ = addnode();
	      $$->nodetype = Unimplemented;
            }
;

Constantlist: Constant
            | Constantlist CM Constant
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
#if JAS
  		  localvarlist = switchem($2);
#endif
#if JAVA
		  $2 = switchem($2);  
#endif	
		  arg_table_load($2);
                  $$ = $2;
                }
;


Namelist:   Name {$$=$1;}
          |  Namelist CM Name {$3->prevstmt = $1; $$ = $3;}
;

/* Somewhere in the actions associated with this production,
   I need to ship off the type and variable list to get hashed.
Also need to pass `typevarlist' off to a procedure
   to load a local variable table for opcode generation.
   */
Typestmt:      Types Typevarlist NL
              {
                 $$ = addnode();
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

Typevarlist:     Typevar
             {
               $$ = $1;
             }
          |  Typevarlist CM  Typevar
             {
               $3->prevstmt = $1;
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
 */
/*  Might have to explicitely set the arraydeclist pointer to
    NULL in this action.  `Name' gets pointed to by the node
    that carries the array information.
    */
Name:    NAME  
         {
           $$=addnode();
	   $$->token = NAME;
           $$->nodetype = Identifier;
           strcpy($$->astnode.ident.name, yylval.lexeme);
	   /*  if(debug)printf("Name NAME, %s.\n",yylval.lexeme); */
         }
       | Char {$$ = $1;}
       | String {$$ = $1;}
;

Char:     CHAR
         {
           $$=addnode();
 	    $$->token = CHAR; 
           $$->nodetype = Identifier;
           strcpy($$->astnode.ident.name, yylval.lexeme);
         }
;

String:  STRING
         {
           $$=addnode();
	    $$->token = STRING;
           $$->nodetype = Identifier;
           strcpy($$->astnode.ident.name, yylval.lexeme);
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
		    $$->astnode.ident.leaddim = strdup ($$->astnode.ident.arraylist->astnode.ident.name);
		    store_array_var($$);
		    
		    
                  }

Arraynamelist:    Arrayname {$$=$1;}
                   | Arraynamelist CM Arrayname {$3->prevstmt = $1; $$ = $3;}
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
Arrayindexlist:   Arrayindexop {$$=$1;}

                | Arrayindexlist CM Arrayindexop
                  {
                    $3->prevstmt = $1;
		    $$ = $3;
		  }
;

Arrayindex:    Name {$$=$1;}
             | Integer {$$=$1;}
/*             | Arrayindexop {$$ = $1;}  */
;

Arrayindexop:  Arrayindex
             | OP Arrayindexop CP {$$=$2;}       
             | Arrayindexop PLUS Arrayindexop
               {
		  $$=addnode();
		  $$->astnode.expression.lhs = $1;
		  $$->astnode.expression.rhs = $3;
		  $$->nodetype = Binaryop;
		  $$->astnode.expression.optype = '+';
	       }
              | Arrayindexop MINUS Arrayindexop
              {
		  $$=addnode();
		  $$->astnode.expression.lhs = $1;
		  $$->astnode.expression.rhs = $3;
		  $$->nodetype = Binaryop;
		  $$->astnode.expression.optype = '-';
	       }
              | Arrayindexop STAR Arrayindexop
              {
		  $$=addnode();
		  $$->astnode.expression.lhs = $1;
		  $$->astnode.expression.rhs = $3;
		  $$->nodetype = Binaryop;
		  $$->astnode.expression.optype = '*';
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
            $$->astnode.forloop.counter = $1->astnode.assignment.lhs;
            $$->astnode.forloop.start = $1;
            $$->astnode.forloop.stop = $3;
            $$->astnode.forloop.incr = 0;
          }

      
       | Assignment CM Exp CM Exp   NL
         {
           $$ = addnode();
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
;           


Label: Integer Doloop 
       {
         $$ = addnode();
	 $$->nodetype = Label;
	 $$->astnode.label.number = atoi($1->astnode.constant.number);
	 $$->astnode.label.stmt = $2;
       }
      | Integer Assignment NL
       {
         $$ = addnode();
	 $$->nodetype = Assignment;
	 $$->astnode.label.number = atoi($1->astnode.constant.number);
	 $$->astnode.label.stmt = $2;
       }
;

Continue:  Integer CONTINUE NL
       {
         $$ = addnode();
	 $$->nodetype = Label;
	 $$->astnode.label.number = atoi($1->astnode.constant.number);
	 $$->astnode.label.stmt = NULL;
       }
;
/*  Got a problem when a Blockif opens with a Blockif.  The
    first statement of the second Blockif doesn't get into the
    tree.  Might be able to use do loop for example to fix this. */
Blockif:   IF OP Exp CP THEN NL Statements Elseifs Else  ENDIF NL
           {
             $$ = addnode();
	     $3->parent = $$;
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
	  $$->nodetype = Elseif;
	  $$->astnode.blockif.conds = $3;
	  $$->astnode.blockif.stmts = switchem($7);
        }
;


Else:  /* Empty. */  {$$=0;}  /* No `else' statements, NULL pointer. */
        | ELSE NL  Statements 
          {
             $$=addnode();
	     $$->nodetype = Else;
	     $$->astnode.blockif.stmts = switchem($3);
          }
;


Logicalif: IF OP Exp CP Logicalifstmts   
           {
             $$ = addnode();
	     $3->parent = $$;
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
;

/* This has to extended to deal with jasmin opcode. 
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
		    HASHNODE * hashtemp;/* In case we need to switch index order. */
                    $$ = addnode();
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


Explist:   Exp
           {
            $$ = $1;
           }
         | Explist CM Exp
           {
	       $3->prevstmt = $1;
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
/*          |  Boolean  {$$=$1;}       */
          |  Relationalop {$$=$1; } 
          |  Logicalop {$$=$1;} 
          |  OP Exp CP  
             {
               $$ = addnode();
               $$->nodetype = Expression;
               $$->astnode.expression.parens = TRUE;
               $$->astnode.expression.rhs = $2;
               $$->astnode.expression.lhs = 0;
             }
          |  Binaryop {$$=$1;}
          |  Exp POW Exp
             {
               $$=addnode();
	       $$->nodetype = Power;
	       $$->astnode.expression.lhs = $1;
	       $$->astnode.expression.rhs = $3;
             }  
          |  Exp CAT Exp {$$=$1;}  /*  Look up def'n. */
          |  Exp EQV Exp {$$=$1;}  /*  Look up def'n. */
          |  MINUS Exp %prec UMINUS
             {
               $$ = addnode();
               $$->astnode.expression.rhs = $2;
               $$->astnode.expression.lhs = 0;
               $$->astnode.expression.minus = '-';   
               $$->nodetype = Unaryop;
             }
;

Complex: OP Constant CM Constant CP {$$=addnode();}
;

/*
Power:   POW {
             $$ = addnode();
	     $$->nodetype = Unimplemented;
         }
;
*/

/* `TRUE' and `FALSE' have already been typedefed
   as BOOLEANs.  */
Boolean:  TrUE
 {
               $$ = addnode();
               $$->nodetype = Constant;
               strcpy($$->astnode.constant.number, "true");
               $$->astnode.constant.type = INTEGER;
             }
         | FaLSE
 {
               $$ = addnode();
               $$->nodetype = Constant;
               strcpy($$->astnode.constant.number, "false");
               $$->astnode.constant.type = INTEGER;
             }

;

Constant:   Integer
       | Double
       | Exponential
       | Boolean
; 

Integer :     INTEGER 
             {
               $$ = addnode();
	       $$->token = INTEGER;
               $$->nodetype = Constant;
               strcpy($$->astnode.constant.number, yylval.lexeme);
	       /*              $$->astnode.constant.type = INTEGER; */
	       $$->astnode.constant.type = Integer;
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
             }
;
               
/*  Since java doesn't have an EXPONENTIAL data type,
    the function exp_to_double rewrite numbers in the
    nn.dde+nn as floats.  The float is written back into
    the string temp.  */
Exponential:   EXPONENTIAL
             {
               $$ = addnode();
               $$->nodetype = Constant;
	       exp_to_double(yylval.lexeme, tempname);
               strcpy($$->astnode.constant.number, tempname);
               $$->astnode.constant.type = EXPONENTIAL;
             }
;

/*  All the easy productions that work go here.  */

Return:      RETURN NL
             {
                $$= addnode();
             }
;
Goto:   GOTO Integer  NL
        {
          $$ = addnode();
          $$->nodetype = Goto;
	  if(debug)printf("goto label: %d\n", atoi(yylval.lexeme)); 
          $$->astnode.go_to.label = atoi(yylval.lexeme);
        }
    |   GOTO OP Intlist CP Name NL
        {
          $$ = addnode();
          $$->nodetype = Unimplemented;
	  printf("Computed go to,\n");
        }    
;

Intlist:   Integer
      | Intlist CM Integer
;

Parameter:   PARAMETER OP Pdecs CP NL 
             {
	       $$ = addnode();
	       $$->nodetype = Specification;
	       $$->astnode.typeunit.specification = Parameter;
               $$->astnode.typeunit.declist = switchem($3); 
             }
;

Pdecs:    Pdec {$$=$1;}
        | Pdecs CM Pdec {$3->prevstmt = $1; $$=$3;}
;

Pdec:     Assignment
          {
	    if(debug)printf("Parameter...\n");
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
             $$->nodetype = Specification;
	      $$->token = EXTERNAL;
             $$->astnode.typeunit.declist = switchem($2);
             $$->astnode.typeunit.specification = External;
           }
;

Intrinsic: INTRINSIC Namelist NL
           {
             $$=addnode(); 
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
   node.  If there is anyway to ensure that all the pointers
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
	      /*	      exit(-1);   */
	    }
	  /* All names go into the name table.  */
	  type_insert(&(type_table->entry[index]), tempnames, return_type);

	  /* Now separate out the EXTERNAL from the INTRINSIC on the
	     fortran side.  */
	  if(temptypes->token != (int)NULL)
	  switch (temptypes->token)
	    {
	    case INTRINSIC:
	      type_insert(&(intrinsic_table->entry[index]), tempnames, return_type);
	      break;
	    case EXTERNAL:
	      type_insert(&(external_table->entry[index]), tempnames, return_type);
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
       type_insert(&(args_table->entry[index]), temp, NULL);
       if(debug)printf("Arglist var. name: %s\n", temp->astnode.ident.name);
     }

}


lowercase(char * name)
{
  while (*name)
    {
     *name = tolower(*name);
      name++;
    }
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
  type_insert(&(array_table->entry[index]), var, NULL);
  if(debug)
  printf("Array name: %s\n", var->astnode.ident.name);
}
