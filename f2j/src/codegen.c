/* 
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */


/*  codegen.c
   Generates java source code for checking.
 */

#include<stdio.h>
#include<string.h>
#include<ctype.h>
#include"f2j.h"
#include"f2jparse.tab.h"

#define ONED 1
#define TWOD 0

char *strdup(const char *);
char * print_nodetype (AST *); 

char *progname;
char *returnname;
int gendebug = 0;

/*  Global variables, a necessary evil when working with
   yacc. */


char *returnstring[] =
{"String", "complex", "double", "float", "int", "boolean"};

void
emit (AST * root)
{


    switch (root->nodetype)
      {
      case 0:
	  if (gendebug)
	      printf ("Bad node\n");
	  emit (root->nextstmt);
      case Progunit:
	  if (gendebug)
	      printf ("Source.\n");
	  emit (root->astnode.source.progtype);
	  fprintf (javafp, "// Type declarations.\n");
	  emit (root->astnode.source.typedecs);
	  fprintf (javafp, "\n// Executable code.\n");
	  emit (root->astnode.source.statements);
	  break;
      case Subroutine:
	  if (gendebug)
	      printf ("Subroutine.\n");
	  returnname = NULL;	/* Subroutines return void. */
	  constructor (root);
	  break;
      case Function:
	  if (gendebug)
	      printf ("Function.\n");
	  returnname = root->astnode.source.name->astnode.ident.name;
	  printf ("Function name: %s\n", root->astnode.source.name->astnode.ident.name);
	  constructor (root);
	  break;
      case Typedec:
	  if (gendebug)
	      printf ("Typedec.\n");
	  typedec_emit (root);
	  if (root->nextstmt != NULL)	/* End of typestmt list. */
	      emit (root->nextstmt);
	  break;
      case Specification:
	  if (gendebug)
	      printf ("Specification.\n");
	  spec_emit (root);
	  if (root->nextstmt != NULL)	/* End of typestmt list. */
	      emit (root->nextstmt);
	  break;
      case Statement:
	  if (gendebug)
	      printf ("Statement.\n");
	  if (root->nextstmt != NULL)	/* End of typestmt list. */
	      emit (root->nextstmt);
	  break;

      case Assignment:
	  if (gendebug)
	      printf ("Assignment.\n");
	  assign_emit (root);
	  fprintf (javafp, ";\n");
	  if (root->nextstmt != NULL)
	      emit (root->nextstmt);
	  break;
      case Call:
	  if (gendebug)
	      printf ("Call.\n");
	  call_emit (root);
	  if (root->nextstmt != NULL)	/* End of typestmt list. */
	      emit (root->nextstmt);
	  break;
      case Forloop:
	  if (gendebug)
	      printf ("Forloop.\n");
	  forloop_emit (root);
	  if (root->nextstmt != NULL)	/* End of typestmt list. */
	      emit (root->nextstmt);
	  break;

      case Blockif:
	  if (gendebug)
	      printf ("Blockif.\n");
	  blockif_emit (root);
	  if (root->nextstmt != NULL)	/* End of typestmt list. */
	      emit (root->nextstmt);
	  break;
      case Elseif:
	  if (gendebug)
	      printf ("Elseif.\n");
	  elseif_emit (root);
	  if (root->nextstmt != NULL)	/* End of typestmt list. */
	      emit (root->nextstmt);
	  break;
      case Else:
	  if (gendebug)
	      printf ("Else.\n");
	  else_emit (root);
	  if (root->nextstmt != NULL)	/* End of typestmt list. */
	      emit (root->nextstmt);
	  break;
      case Logicalif:
	  if (gendebug)
	      printf ("Logicalif.\n");
	  logicalif_emit (root);
	  if (root->nextstmt != NULL)	/* End of typestmt list. */
	      emit (root->nextstmt);
	  break;
      case Return:
	  if (gendebug)
	      printf ("Return: %s.\n", returnname);
	  if (returnname != NULL)
	      fprintf (javafp, "return %s;\n", returnname);
	  else
	      fprintf (javafp, "return;\n");
	  if (root->nextstmt != NULL)	/* End of typestmt list. */
	      emit (root->nextstmt);
	  break;
      case Goto:
	  if (gendebug)
	      printf ("Goto.\n");
	  goto_emit (root);
	  if (root->nextstmt != NULL)
	      emit (root->nextstmt);
	  break;

      case Label:
	  if (gendebug)
	      printf ("Label.\n");
	  label_emit (root);
	  if (root->nextstmt != NULL)	/* End of typestmt list. */
	      emit (root->nextstmt);
	  break;
      case End:
	  if (gendebug)
	      printf ("End.\n");
	  fprintf (javafp, "   }\n}  //  End class.");
	  break;
      case Unimplemented:
	  fprintf (javafp, "Unimplemented statement in Fortran source.\n");
	  if (root->nextstmt != NULL)
	      emit (root->nextstmt);
	  break;
      case Constant:
      default:
	  if (gendebug)
	      printf ("Default\n");
      }				/* switch on nodetype.  */
}

/* Emit all the type declarations.  This procedure checks
   whether variables are typed in the argument list, and
   does not redeclare thoose arguments. */
int
typedec_emit (AST * root)
{
    AST *temp;
    AST *temp2;
    HASHNODE *hashtemp;
    enum returntype returns;

    temp = root->astnode.typeunit.declist;

    /* This may have to be moved into the looop also.  Could be
       why I have had problems with this stuff.  */
    hashtemp = type_lookup (external_table, temp->astnode.ident.name);
    if (hashtemp)
	return 1;

    returns = root->astnode.typeunit.returns;

    /*  Somewhere in here I need to do a table lookup
       to see whether the variable is in the argument
       list for the method.  If so, it takes the type
       in the argument list and is not retyped here. */
    for (temp; temp != NULL; temp = temp->nextstmt)
      {

	  /* Let's do the argument lookup first. No need to retype variables
	     that are already declared in the argument list, or declared
	     as externals.  So if it is already declared, loop again.  */
	  hashtemp = type_lookup (args_table, temp->astnode.ident.name);
	  if (hashtemp)
	      continue;

          /* check to see if this is an array declaration or not. 
             if so, we must generate the appropriate "new" statement.
             otherwise, just declare & initialize in one statement. --keith */

          if(temp->astnode.ident.arraylist != NULL) {
            fprintf (javafp, "%s [] ", returnstring[returns]);
            if (gendebug)
              printf ("%s\n", returnstring[returns]);
	    name_emit (temp);

	    if (returns == Integer)
	      fprintf (javafp, "= new int[");
            else if (returns == Double)
	      fprintf (javafp, "= new double[");
	    else if (returns == Logical)
	      fprintf (javafp, "= new boolean[");
            else
              fprintf(stderr,"typdec_emit():  Unknown type!\n");
         
            for(temp2=temp->astnode.ident.arraylist;temp2!=NULL;temp2=temp2->nextstmt) {
              if(temp2 != temp->astnode.ident.arraylist)
                fprintf(javafp, " * ");   /* if not the first iteration */
              expr_emit(temp2);
            }

	    fprintf (javafp, "];\n");
          } else {
	     fprintf (javafp, "%s ", returnstring[returns]);
	     if (gendebug)
	       printf ("%s\n", returnstring[returns]);
	     name_emit (temp);

	     /*  initialize local variables to zero or
	        false to keep the java compiler from
	        squawking.  */

	     if (returns == Integer || returns == Double)
	       fprintf (javafp, "= 0");
	     else if (returns == Logical)
	       fprintf (javafp, "= false");

	     fprintf (javafp, ";\n");
          }
      }
}				/* Close typedec_emit(). */

/* A name will either fly solo or lead off
   a named array.  So far, this code will emit
   a name or an array with integer indices.  The
   procedure also needs to check all relevant tables
   to determine whether the name is an array or
   a procedure (i.e. Class.method) call, and whether
   the name is a STRING, CHAR, etc.  Frankly, this is
   a hideous procedure and really needs to
   be rewritten. 

   ...and it's getting worse by the day  --Keith */

int
name_emit (AST * root)
{
    AST *temp;
    HASHNODE *hashtemp;
    char *javaname, * tempname;
    extern METHODTAB intrinsic_toks[];
    extern SYMTABLE *array_table;

    /*  Check to see whether name is in external table.  Names are
       loaded into the external table from the parser.   */

    hashtemp = type_lookup (external_table, root->astnode.ident.name);

    /* If the name is in the external table, then check to see if
       is an intrinsic function instead.  */

    if (hashtemp != NULL)
      {

	  javaname = (char *) methodscan (intrinsic_toks, root->astnode.ident.name);
	  /*  This block of code is only called if the identifier
	     absolutely does not have an entry in any table,
	     and corresponds to a method invocation of
	     something in the blas or lapack packages.  */
	  if (javaname == NULL)
	    {
		if (root->astnode.ident.arraylist != NULL)
		  {
		      call_emit (root);
		      return;
		  }
		return;
	    }

	  if (root->astnode.ident.arraylist != NULL)
	    {
		if (!strcmp (root->astnode.ident.name, "LSAME"))
		  {
		      temp = root->astnode.ident.arraylist;
		      fprintf (javafp, "%s", temp->astnode.ident.name);
		      fprintf (javafp, "%s(", javaname);
		      name_emit (temp->nextstmt);
		      fprintf (javafp, ")");
		      /* goto end; *//*  Hack ... */
		      return;
		  }
	    }
      }

    /*  Check to see whether name is in intrinsic table.  */

    /*

    hashtemp = type_lookup (intrinsic_table, root->astnode.ident.name);
    if (hashtemp != NULL)
      {

    */
         
    tempname = strdup(root->astnode.ident.name);
    uppercase(tempname);
    if(gendebug)printf ("Tempname  %s\n", tempname);
	  javaname = (char *) methodscan (intrinsic_toks, tempname);
	  
	  if (javaname != NULL)
	    {
	  
	      /* This goes into an infinite loop. I don't
		 know why.  Might be a result from parsing.
		 It will replace the entire lower block
		 when I get it to work.  */
#ifdef  KJGKJKJKKJH
	       temp = root->astnode.ident.arraylist;
	       assert(temp != NULL);
	      fprintf (javafp, "%s(", javaname);
	      for (temp; temp != NULL; temp->nextstmt)
		{
		  printf("Yoikes\n");
		  expr_emit (temp);
		  if(temp->nextstmt)
		    fprintf (javafp, ", ");
		}
	      fprintf (javafp, ")");
	      return;
	    }
#endif	  
	  /* #ifdef ININININ  */
		if (!strcmp (root->astnode.ident.name, "MAX"))
		  {
		      temp = root->astnode.ident.arraylist;
		      fprintf (javafp, "%s(", javaname);
		      expr_emit (temp);
		      fprintf (javafp, ", ");
		      expr_emit (temp->nextstmt);
		      fprintf (javafp, ")");
		      return;
		  }

		if (!strcmp (root->astnode.ident.name, "MIN"))
		  {
		      temp = root->astnode.ident.arraylist;
		      fprintf (javafp, "%s(", javaname);
		      expr_emit (temp);
		      fprintf (javafp, ", ");
		      expr_emit (temp->nextstmt);
		      fprintf (javafp, ")");
		      return;
		  }

		if (!strcmp (root->astnode.ident.name, "ABS"))
		  {
		      temp = root->astnode.ident.arraylist;
		      fprintf (javafp, "%s(", javaname);
		      expr_emit (temp);
		      fprintf (javafp, ")");
		      return;
		  }

		if (!strcmp (tempname, "DABS"))
		  {
		      temp = root->astnode.ident.arraylist;
		      fprintf (javafp, "%s(", javaname);
		      expr_emit (temp);
		      fprintf (javafp, ")");
		      return;
		  }

		if ( (!strcmp (tempname, "DSQRT")) ||
                     (!strcmp (tempname, "SQRT")))
		  {
		      temp = root->astnode.ident.arraylist;
		      fprintf (javafp, "%s(", javaname);
		      expr_emit (temp);
		      fprintf (javafp, ")");
		      return;
		  }
	
		if (!strcmp (tempname, "MOD"))
		  {
		      temp = root->astnode.ident.arraylist;
		      fprintf (javafp, "%s(", javaname);
		      expr_emit (temp);
		      fprintf (javafp, ", ");
		      expr_emit (temp->nextstmt);
		      fprintf (javafp, ")");
		      return;
		  }
	    }
/* #endif	 */
	  /*
      }
      */

    hashtemp = type_lookup (array_table, root->astnode.ident.name);


    switch (root->token)
      {
      case STRING:
	  fprintf (javafp, "\"%s\"", root->astnode.ident.name);
	  break;
      case CHAR:
	  fprintf (javafp, "\"%s\"", root->astnode.ident.name);
	  break;
      case NAME:
      default:
	  /* At some point in here I will have to switch on the
	     token type check whether it is a variable or
	     string or character literal. Also have to look up whether
	     name is intrinsic or external.  */

	  if (root->astnode.ident.arraylist == NULL) {
	      fprintf (javafp, "%s", root->astnode.ident.name);
          }
	  else if (hashtemp != NULL)
	    {
		if (gendebug)
		  printf ("Array... %s, My node type is %s\n", 
                    root->astnode.ident.name,
                    print_nodetype(root));

              /* Now, what needs to happen here is the context of the
                 array needs to be determined.  If the array is being
                 passed as a parameter to a method, then the array index
                 needs to be passed separately and the array passed as
                 itself.  If not, then an array value is being set,
                 so dereference with index arithmetic.  */

		fprintf (javafp, "%s", root->astnode.ident.name);
		temp = root->astnode.ident.arraylist;

                if(root->parent == NULL) {
                  /* Under normal circumstances, I dont think this should 
                     be reached */
		  printf ("Array... %s, NO PARENT - ", root->astnode.ident.name);
		  printf ("This is not good!\n");
                } else {
		  printf ("Array... %s, Parent node type... %s\n", 
                    root->astnode.ident.name,
                    print_nodetype(root->parent));
                  if((root->parent->nodetype == Call)) 
                  {
                    if((type_lookup(external_table, 
                                    root->parent->astnode.ident.name) != NULL))
                    {
                      printf("Function name is: %s - ",
                        root->parent->astnode.ident.name);
                        /* This is an external function */
#if ONED
                        fprintf (javafp, ",");
                        expr_emit (temp);
                        if (hashtemp->variable->astnode.ident.leaddim[0] != '*' &&
                            temp->nextstmt != NULL)
                        {
                          temp = temp->nextstmt;
                          fprintf (javafp, "+");
                          expr_emit (temp);
                          fprintf (javafp, "*");
                          fprintf(javafp,  "%s", 
                               hashtemp->variable->astnode.ident.leaddim);
                        }  /* Multi dimension.  */
#endif
#if TWOD
                        printf("TWOD not implemented yet!\n");
#endif
                    } else {
                        /* I dont think this is an external function */
#if ONED
                        fprintf (javafp, "[");
                        expr_emit (temp);
                        if (hashtemp->variable->astnode.ident.leaddim[0] != '*' &&
                            temp->nextstmt != NULL)
                        {
                          temp = temp->nextstmt;
                          fprintf (javafp, "+");
                          expr_emit (temp);
                          fprintf (javafp, "*");
                          fprintf(javafp,  "%s", 
                               hashtemp->variable->astnode.ident.leaddim);
                        }  /* Multi dimension.  */
                        fprintf(javafp, "]");
#endif
#if TWOD
                        for (temp; temp != NULL; temp = temp->prevstmt)
                        {
                          fprintf (javafp, "[");
                          if (*temp->astnode.ident.name != '*')
                            expr_emit (temp);
                          fprintf (javafp, "]");
                        }           /* Close for() loop. */
#endif
                    } 
                  } else if((root->parent->nodetype == Typedec)) {
                     printf("I guess this is an array declaration\n");
                  } else {
#if ONED
                      fprintf (javafp, "[");
                      expr_emit (temp);
                      if (hashtemp->variable->astnode.ident.leaddim[0] != '*' &&
                          temp->nextstmt != NULL)
                      {
                        temp = temp->nextstmt;
                        fprintf (javafp, "+");
                        expr_emit (temp);
                        fprintf (javafp, "*");
                        fprintf(javafp,  "%s", 
                             hashtemp->variable->astnode.ident.leaddim);
                      }  /* Multi dimension.  */
                      fprintf(javafp, "]");
#endif
#if TWOD
                      for (temp; temp != NULL; temp = temp->prevstmt)
                      {
                        fprintf (javafp, "[");
                        if (*temp->astnode.ident.name != '*')
                          expr_emit (temp);
                        fprintf (javafp, "]");
                      }           /* Close for() loop. */
#endif
                  }
                }
	    }
	  /*  I think this code is redundant.  */
                      /* possibly not -- keith */
	  else
	    {
printf("hi\n");
                /* else it's not in the array table? */

		fprintf (javafp, "%s", root->astnode.ident.name);
		temp = root->astnode.ident.arraylist;
		fprintf (javafp, "(");
		for (temp; temp != NULL; temp = temp->nextstmt)
		  {
		      /* fprintf (javafp, "["); */

                      if(temp != root->astnode.ident.arraylist)
		        fprintf (javafp, ",");  /* if not first iteration */
                        
		      if (*temp->astnode.ident.name != '*')
			  expr_emit (temp);

		      /* fprintf (javafp, "]"); */
		  }		/* Close for() loop. */
		fprintf (javafp, ")");
	    }
	  break;
      }				/* Close switch(). */
    /* end: */
}				/* Close name_emit().  */


/* All this will do is emit a number if there is one.
   Needs to be extended for arrays, etc.  Consider using
   a switch/case structure for this.
 */
int
expr_emit (AST * root)
{
    switch (root->nodetype)
      {
      case Identifier:
	  name_emit (root);
	  break;
      case Expression:
	  if (root->astnode.expression.parens)
	      fprintf (javafp, "(");
	  if (root->astnode.expression.lhs != NULL)
	      expr_emit (root->astnode.expression.lhs);
	  expr_emit (root->astnode.expression.rhs);
	  if (root->astnode.expression.parens)
	      fprintf (javafp, ")");
	  break;
      case Power:
	  fprintf (javafp, "Math.pow(");
	  expr_emit (root->astnode.expression.lhs);
	  fprintf (javafp, ", ");
	  expr_emit (root->astnode.expression.rhs);
	  fprintf (javafp, ")");
	  break;
      case Binaryop:
	  expr_emit (root->astnode.expression.lhs);
	  fprintf (javafp, "%c", root->astnode.expression.optype);
	  expr_emit (root->astnode.expression.rhs);
	  break;
      case Unaryop:
	  fprintf (javafp, "%c", root->astnode.expression.minus);
	  expr_emit (root->astnode.expression.rhs);
	  break;
      case Constant:
	  fprintf (javafp, "%s", root->astnode.constant.number);
	  break;
      case Logicalop:
	  /* Change all of this code to switch on the tokens.
	     The parser code will have to store the NOT token.
	   */
	  if (root->astnode.expression.lhs != NULL)
	      expr_emit (root->astnode.expression.lhs);
	  else
	      fprintf (javafp, "!");
	  if (root->token == AND)
	      fprintf (javafp, " && ");
	  if (root->token == OR)
	      fprintf (javafp, " || ");
	  expr_emit (root->astnode.expression.rhs);
	  break;
      case Relationalop:
	  expr_emit (root->astnode.expression.lhs);
	  /*  Leave all the rest of this stuff out because the
	     preceding doesn't work.
	   */
	  /* */ switch (root->token)
	    {
	    case rel_eq:
		fprintf (javafp, " == ");
		break;
	    case rel_ne:
		fprintf (javafp, " != ");
		break;
	    case rel_lt:
		fprintf (javafp, " < ");
		break;
	    case rel_le:
		fprintf (javafp, " <= ");
		break;
	    case rel_gt:
		fprintf (javafp, " > ");
		break;
	    case rel_ge:
		fprintf (javafp, " >= ");
		break;
	    }
	  expr_emit (root->astnode.expression.rhs);	/*  */
	  break;
      }
}

int
constructor (AST * root)
{
    enum returntype returns;
    extern char *returnstring[];
    AST *tempnode, *temp;
    extern SYMTABLE *type_table;
    char *tempstring;
    HASHNODE *hashtemp;

    /* In fortran, functions return a value implicitely
       associated with there own name. In java, we declare a
       variable in the constructor that shadows the class
       (function) name and returns the same type. */

    if (root->nodetype == Function)
      {
	  returns = root->astnode.source.returns;
	  /* Test code.... */
	  fprintf (javafp, "static %s %s;\n\n", returnstring[returns],
		   root->astnode.source.name->astnode.ident.name);

	  /* Define the constructor for the class. */
	  fprintf (javafp, "\npublic static %s %s (",
		   returnstring[returns],
		   root->astnode.source.name->astnode.ident.name);

      }
    /* Else we have a subroutine, which returns void. */
    else
      {
	  fprintf (javafp, "\npublic static void %s (",
		   root->astnode.source.name->astnode.ident.name);
      }

/* Now traverse the list of constructor arguments for either
   functions or subroutines.   This is where I will
   have to check what the variable type is in the
   symbol table. */

    tempnode = root->astnode.source.args;

    for (tempnode; tempnode != NULL; tempnode = tempnode->nextstmt)
      {
	  hashtemp = type_lookup (type_table, tempnode->astnode.ident.name);
	  if (hashtemp == NULL)
	    {
		fprintf (stderr,"Type table is screwed (codegen.c).\n");
		fprintf (stderr,"  (looked up: %s)\n", tempnode->astnode.ident.name);
		exit (-1);
	    }
	  /* Since all of fortran is call-by-reference, we have to pass
	     in the arguments as java Objects.  */

	  returns = hashtemp->type;
	  /* Check the numerical value returns.  It should not 
	     exceed the value of the enum returntypes.  */
	  if (returns > 6)
	    {
		printf ("Bad return value, check types.\n");
	    }

	  tempstring = returnstring[returns];

	  /* I haven't yet decided how the pass-by-reference
	     pass-by-value problem will be resolved.  It may
	     not ba an issue at all in a java calling java
	     situation.  The next line, when used, will list
	     all the arguments to the method as references.
	     This means that primitives such as int and
	     double are wrapped as objects. */
	  /* *tempstring = toupper (*tempstring);  */
	  fprintf (javafp, "%s ", tempstring);

	  if (hashtemp->variable->astnode.ident.arraylist == NULL)
	      fprintf (javafp, "%s", tempnode->astnode.ident.name);
	  else
	      /* Declare as array variables.  */
	    {
               char temp2[100];
#if ONED
	fprintf (javafp, "[]");
#endif      
#if TWOD
		temp = hashtemp->variable->astnode.ident.arraylist;
		for (temp; temp != NULL; temp = temp->nextstmt)
		  {
		      fprintf (javafp, "[]");
		  }		/* Close for() loop. */
#endif
		fprintf (javafp, " %s", tempnode->astnode.ident.name);

               /* for arrays, add a parameter representing the base 
                  index.   -- Keith */
                strcpy( temp2, "_");
                strcat( temp2, tempnode->astnode.ident.name);
                strcat( temp2, "idx");
                fprintf(javafp, ", int %s",temp2);
	    }
	  /* Don't emit a comma on the last iteration. */
	  if (tempnode->nextstmt)
	      fprintf (javafp, ",\n");
      }

    fprintf (javafp, ")  {\n\n");

}				/*  Close  constructor(). */


int
forloop_emit (AST * root)
{

    char *indexname;

/*  Some point I will need to test whether this is really a name
   because it will crash if not.  */
    indexname = root->astnode.forloop.start->astnode.assignment.lhs->astnode.ident.name;

/* This block writes out the loop parameters.  */
    fprintf (javafp, "for (");
    assign_emit (root->astnode.forloop.start);
    fprintf (javafp, " -1; %s<", indexname);
    expr_emit (root->astnode.forloop.stop);
    fprintf (javafp, "; ");
    if (root->astnode.forloop.incr == NULL)
	fprintf (javafp, "%s++", indexname);
    else
      {
	  fprintf (javafp, "%s+=", indexname);
	  expr_emit (root->astnode.forloop.incr);
      }

    fprintf (javafp, ") {\n");

/*  Done with loop parameters.  */

/* Statements in the body of the for() loop. */
    emit (root->astnode.forloop.stmts);

    fprintf (javafp, "}              //  Close for() loop. \n");


}

goto_emit (AST * root)
{
    fprintf (javafp, "goto label%d;\n", root->astnode.go_to.label);
}

logicalif_emit (AST * root)
{
    fprintf (javafp, "if (");
    if (root->astnode.logicalif.conds != NULL)
	expr_emit (root->astnode.logicalif.conds);
    fprintf (javafp, ")  \n    ");
    emit (root->astnode.logicalif.stmts);
}

int
label_emit (AST * root)
{
    fprintf (javafp, "label%d:\n   ", root->astnode.label.number);
    if (root->astnode.label.stmt != NULL)
	emit (root->astnode.label.stmt);
}

int
blockif_emit (AST * root)
{
    fprintf (javafp, "if (");
    if (root->astnode.blockif.conds != NULL)
	expr_emit (root->astnode.blockif.conds);
    fprintf (javafp, ")  {\n    ");
    emit (root->astnode.blockif.stmts);
    fprintf (javafp, "}              // Close if()\n");
    if (root->astnode.blockif.elseifstmts != NULL)
	emit (root->astnode.blockif.elseifstmts);
    if (root->astnode.blockif.elsestmts != NULL)
	emit (root->astnode.blockif.elsestmts);
}

void
elseif_emit (AST * root)
{
    fprintf (javafp, "else if (");
    if (root->astnode.blockif.conds != NULL)
	expr_emit (root->astnode.blockif.conds);
    fprintf (javafp, ")  {\n    ");
    emit (root->astnode.blockif.stmts);
    fprintf (javafp, "}              // Close else if()\n");
}

void
else_emit (AST * root)
{
    fprintf (javafp, "else  {\n  ");
    emit (root->astnode.blockif.stmts);
    fprintf (javafp, "}              //  Close else.\n");
}

/* This procedure implements Lapack and Blas type methods.
   They are translated to static method invocations.
   This is not a portable solution, it is specific to
   the Blas and Lapack. */
int
call_emit (AST * root)
{
    AST *temp;
    char *tempname;

    assert (root != NULL);

printf("HERE IN CALL_EMIT\n");
    lowercase (root->astnode.ident.name);
    tempname = strdup (root->astnode.ident.name);
    *tempname = toupper (*tempname);

    /* Assume all methods that are invoked are static.  */
    fprintf (javafp, "%s.%s", tempname, root->astnode.ident.name);

    assert (root->astnode.ident.arraylist != NULL);

    temp = root->astnode.ident.arraylist;
    fprintf (javafp, "(");
    while (temp->nextstmt != NULL)
      {
	  expr_emit (temp);
	  fprintf (javafp, ",");
	  temp = temp->nextstmt;
      }
    expr_emit (temp);
    /*  Problem here, depends on who called this procedure.
        When this is used by the CALL keyword, it works as
	written.  When used to create an external function call,
	it adds an extra ; and \n to the output.  Might be
	able to fix this by checking the nodetype. */
    if(root->nodetype == Call)
      fprintf (javafp, ");\n");
    else
      fprintf (javafp, ")");
}				/*  Close call_emit().  */

int
spec_emit (AST * root)
{
    AST *assigntemp;

    /* I am reaching every case in this switch.  */
    switch (root->astnode.typeunit.specification)
      {
	  /* PARAMETER in fortran corresponds to a class
	     constant in java, that has to be declared
	     class wide outside of any method.  This is
	     currently not implemented, but the assignment
	     is made.  */
      case Parameter:
	  fprintf (javafp, "// Assignment from Fortran PARAMETER specification.\n");
	  assigntemp = root->astnode.typeunit.declist;
	  for (assigntemp; assigntemp; assigntemp = assigntemp->nextstmt)
	    {
		if (gendebug)
		    printf ("Parameter stmt.\n");
		/*  fprintf (javafp, "public static final "); */
		assign_emit (assigntemp);
		fprintf (javafp, ";\n");
	    }
	  break;

	  /*  I am reaching these next two cases. Intrinsic, for
	     example handles stuff like Math.max, etc. */
      case Intrinsic:
	  name_emit (root);
	  break;
      case External:
	  /*        printf ("External stmt.\n");   */
	  break;
      }
}


int
assign_emit (AST * root)
{
    name_emit (root->astnode.assignment.lhs);
    fprintf (javafp, " = ");
    expr_emit (root->astnode.assignment.rhs);
}

char * print_nodetype (AST *root) 
{
  static char temp[100];

  switch (root->nodetype)
  {
    case 0:
      return("print_nodetype(): Bad Node");
    case Source:
      return("print_nodetype(): Source");
    case Progunit:
      return("print_nodetype(): Progunit");
    case Subroutine:
      return("print_nodetype(): Subroutine");
    case Function:
      return("print_nodetype(): Function");
    case Specification:
      return("print_nodetype(): Specification");
    case Statement:
      return("print_nodetype(): Statement");
    case Assignment:
      return("print_nodetype(): Assignment");
    case Call:
      return("print_nodetype(): Call");
    case Forloop:
      return("print_nodetype(): Forloop");
    case Blockif:
      return("print_nodetype(): Blockif");
    case Elseif:
      return("print_nodetype(): Elseif");
    case Else:
      return("print_nodetype(): Else");
    case Identifier:
      return("print_nodetype(): Identifier");
    case Method:
      return("print_nodetype(): Method");
    case Expression:
      return("print_nodetype(): Expression");
    case Typedec:
      return("print_nodetype(): Typedec");
    case Logicalif:
      return("print_nodetype(): Logicalif");
    case Return:
      return("print_nodetype(): Return");
    case Goto:
      return("print_nodetype(): Goto");
    case Label:
      return("print_nodetype(): Label");
    case Relationalop:
      return("print_nodetype(): Relationalop");
    case Logicalop:
      return("print_nodetype(): Logicalop");
    case Binaryop:
      return("print_nodetype(): Binaryop");
    case Unaryop:
      return("print_nodetype(): Unaryop");
    case End:
      return("print_nodetype(): End");
    case Unimplemented:
      return("print_nodetype(): Unimplemented");
    case Constant:
      return("print_nodetype(): Constant");
    default:
      sprintf(temp, "print_nodetype(): Unknown Node: %d", root->nodetype);
      return(temp);
  }
}
