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
#include"list.h"

#define ONED 1
#define TWOD 0

char * strdup ( const char * );
char * print_nodetype ( AST * ); 
char * lowercase ( char * );
HASHNODE * format_lookup(SYMTABLE *, char *);
void format_name_emit(AST *);

char *progname;
char *returnname;
int gendebug = 0;
int cur_idx = 0;
EntryList *doloop = NULL;
EntryList *while_list = NULL;

FILE *javafp;

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
          fprintf(javafp,"} // End class.\n");
          fclose(javafp);
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
	  printf ("Function name: %s\n", 
              root->astnode.source.name->astnode.ident.name);
	  constructor (root);
	  break;
      case Program:
	  if (gendebug)
	      printf ("Program.\n");
	  returnname = root->astnode.source.name->astnode.ident.name;
	  printf ("Program name: %s\n", 
              root->astnode.source.name->astnode.ident.name);
          constructor(root);
          break;
      case Typedec:
	  if (gendebug)
	      printf ("Typedec.\n");
	  typedec_emit (root);
	  if (root->nextstmt != NULL)	/* End of typestmt list. */
	      emit (root->nextstmt);
	  break;
      case DataList:
	  if (gendebug)
	      printf ("Data.\n");
	  data_emit (root);
	  if (root->nextstmt != NULL)	/* End of data list. */
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
      case Write:
	  if (gendebug)
	      printf ("Write statement.\n");
	  write_emit (root);
	  if (root->nextstmt != NULL)
	      emit (root->nextstmt);
	  break;
      case Format:
          printf("skipping format statement\n");
	  if (root->nextstmt != NULL)
	      emit (root->nextstmt);
          break;
      case Stop:
	  if (gendebug)
	      printf ("Stop.\n");
	  fprintf (javafp, "   }\n");
	  break;
      case End:
	  if (gendebug)
	      printf ("End.\n");
	  fprintf (javafp, "   }\n");
	  break;
      case Unimplemented:
	  fprintf (javafp, "// WARNING: Unimplemented statement in Fortran source.\n");
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
          /* If there is a corresponding data statement for this
             variable, don't emit anything here.  Just wait and
             let the whole thing get emitted when we come across
             the DATA node.  --9/22/97,  Keith */

          if(type_lookup(data_table,temp->astnode.ident.name)) {
             printf("@@ Variable %s: Found corresponding data stmt\n",
               temp->astnode.ident.name);
             continue;
          }
          else
             printf("@@ Variable %s: Corresponding data stmt not found\n",
               temp->astnode.ident.name);

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

int
data_emit(AST *root)
{
  enum returntype returnval;
  AST *Dtemp, *Ntemp, *Ctemp;
  HASHNODE *hashtemp;
  int i, length=1, is_array=FALSE;

    /* foreach Data spec... */
  for(Dtemp = root->astnode.label.stmt; Dtemp != NULL; Dtemp = Dtemp->prevstmt) 
  {
    Ctemp = Dtemp->astnode.data.clist;

      /* foreach variable... */    
    for(Ntemp = Dtemp->astnode.data.nlist; Ntemp != NULL; Ntemp = Ntemp->nextstmt) 
    {
      if((hashtemp = type_lookup(type_table,Ntemp->astnode.ident.name)) == NULL)
      {
        fprintf(stderr,"No typedec associated with this DATA variable: %s\n",
          Ntemp->astnode.ident.name);
        continue;
      }

      returnval = hashtemp->type;

      if(hashtemp->variable == NULL)
      {
        fprintf(stderr,"Wow, hashtemp->variable is NULL!\n");
        continue;
      }

      if( hashtemp->variable->astnode.ident.arraylist != NULL )
        is_array = TRUE;
      else
        is_array = FALSE;

      if( hashtemp->variable->astnode.ident.leaddim != NULL )
      {
        if(hashtemp->variable->astnode.ident.leaddim[0] == '*')
        {
          fprintf(stderr,"Attempt to initialize dummy argument: %s\n",
            hashtemp->variable->astnode.ident.name);
          continue;
        }
        else if (type_lookup(args_table,Ntemp->astnode.ident.name))
        {
          fprintf(stderr,"Attempt to initialize argument: %s\n",
            hashtemp->variable->astnode.ident.name);
          continue;
        }

        if(is_array)
        {
          AST *temp2;

          length = 1;

 
          temp2=hashtemp->variable->astnode.ident.arraylist;
          for( ; temp2 != NULL ; temp2=temp2->nextstmt ) {
            if(temp2->nodetype != Constant) {
              fprintf(stderr,"Cant translate data statement for %s\n",
                 hashtemp->variable->astnode.ident.name);
              break;
            }
            else {
              length *= atoi(temp2->astnode.constant.number);
            }
          }
        }
      }

      fprintf(javafp,"%s ", returnstring[ hashtemp->type]);
   
      if( is_array ) {
        fprintf(javafp,"[] ");
        fprintf(javafp,"%s = {",Ntemp->astnode.ident.name);

        for(i=0;i<length;i++) {
          fprintf(javafp,"%s ",Ctemp->astnode.constant.number);
          if( (Ctemp = Ctemp->nextstmt) == NULL )
            break;
          else if(i != length -1 )
            fprintf(javafp,", ");
        }
        fprintf(javafp,"};\n");
      } 
      else {
        fprintf(javafp,"%s = %s;\n",Ntemp->astnode.ident.name,
           Ctemp->astnode.constant.number);
        Ctemp = Ctemp->nextstmt;
      }
    }
  }
}

#ifdef DATAEMIT

/*  Comments, anyone...?  */

int
data_emit(AST *root)
{
  enum returntype returnval;
  AST *Dtemp, *Ntemp, *Ctemp;
  HASHNODE *hashtemp;

  for(Dtemp = root->astnode.label.stmt; Dtemp != NULL; Dtemp = Dtemp->prevstmt) 
  {
    for(Ntemp = Dtemp->astnode.data.nlist; Ntemp != NULL; Ntemp = Ntemp->nextstmt) 
    {
      if((hashtemp = type_lookup(type_table,Ntemp->astnode.ident.name)) == NULL)
      {
        fprintf(stderr,"No typedec associated with this DATA variable: %s\n",
          Ntemp->astnode.ident.name);
        continue;
      }

      returnval = hashtemp->type;

      /*  There really is something screwy about those 
          hash tables...  -dmd 9/26/97  */
      if(hashtemp->variable == NULL)
      {
        fprintf(stderr,"Wow, hashtemp->variable is NULL!\n");
        continue;
      }

      if( hashtemp->variable->astnode.ident.leaddim != NULL )
      {
        if((hashtemp->variable->astnode.ident.leaddim[0] == '*')  ||
         (type_lookup(args_table,Ntemp->astnode.ident.name)))
        {
          fprintf(stderr,"Attempt to initialize dummy argument: %s\n",
            hashtemp->variable->astnode.ident.name);
          continue;
        }
      }

      fprintf(javafp,"%s ", returnstring[ hashtemp->type]);
   
      if( hashtemp->variable->astnode.ident.arraylist != NULL )
        fprintf(javafp,"[] ");

      fprintf(javafp,"%s = {",Ntemp->astnode.ident.name);
      for(Ctemp = Dtemp->astnode.data.clist; Ctemp != NULL; Ctemp = Ctemp->nextstmt) {
        fprintf(javafp,"%s ",Ctemp->astnode.constant.number);
        if(Ctemp->nextstmt != NULL)
          fprintf(javafp,", ");
      }
      fprintf(javafp,"};\n");
    }
  }
}

#endif /*  DATAEMIT  */


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

/*  Heh... gotta love it...  -dmd  9/26/97  */

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

printf("** nodetype = %s, token = %s\n", print_nodetype(root),
  tok2str(root->token));

    if(root->nodetype == Identifier)
      if(root->token == STRING)
        printf("** I really should emit a string literal here\n");

    hashtemp = type_lookup (external_table, root->astnode.ident.name);

    /* If the name is in the external table, then check to see if
       is an intrinsic function instead.  */

    if (hashtemp != NULL)
      {

          tempname = strdup(root->astnode.ident.name);
          uppercase(tempname);

	  javaname = (char *) methodscan (intrinsic_toks, tempname);

printf("@@ javaname = %s\n",javaname);
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
printf("@@ tempname = %s\n",tempname);
            if (!strcmp (tempname, "LSAME"))
            {
printf("@@ tempname matches LSAME\n");
              temp = root->astnode.ident.arraylist;
              fprintf (javafp, "%s", temp->astnode.ident.name);
              fprintf (javafp, "%s(", javaname);
              name_emit (temp->nextstmt);
              fprintf (javafp, ")");
              /* goto end; *//*  Hack ... */
              return;
            }
            else if (!strcmp (tempname, "LSAMEN"))
            {
printf("@@ tempname matches LSAMEN\n");
              temp = root->astnode.ident.arraylist;

              /* first, make sure there are enough args to work with */
              if(temp == NULL) {
                fprintf(stderr,"No args to LSAMEN\n");
                return;
              } 
              else if(temp->nextstmt == NULL) {
                fprintf(stderr,"Not enough args to LSAMEN\n");
                return;
              }
              else if(temp->nextstmt->nextstmt == NULL) {
                fprintf(stderr,"Not enough args to LSAMEN\n");
                return;
              }

              fprintf (javafp, "%s", temp->nextstmt->astnode.ident.name);
              fprintf (javafp, "%s(true,0,", javaname);
              name_emit (temp->nextstmt->nextstmt);
              fprintf (javafp, ",0,");
              expr_emit (temp);
              fprintf (javafp, ")");
              /* goto end; *//*  Hack ... */
              return;
            }
 else
  printf("@@ tempname matches nothing!\n");
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
	  
printf("## ok, java name = %s\n",javaname);
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
		if (!strcmp (tempname, "MAX"))
		  {
		      temp = root->astnode.ident.arraylist;
		      fprintf (javafp, "%s(", javaname);
		      expr_emit (temp);
		      fprintf (javafp, ", ");
		      expr_emit (temp->nextstmt);
		      fprintf (javafp, ")");
		      return;
		  }

		if (!strcmp (tempname, "MIN"))
		  {
		      temp = root->astnode.ident.arraylist;
		      fprintf (javafp, "%s(", javaname);
		      expr_emit (temp);
		      fprintf (javafp, ", ");
		      expr_emit (temp->nextstmt);
		      fprintf (javafp, ")");
		      return;
		  }

		if (!strcmp (tempname, "ABS"))
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
/*
		      fprintf (javafp, "%s(", javaname);
		      expr_emit (temp);
		      fprintf (javafp, ", ");
		      expr_emit (temp->nextstmt);
		      fprintf (javafp, ")");
*/
                      fprintf(javafp,"(");
                      expr_emit(temp);
                      fprintf(javafp,")%%(");
                      expr_emit(temp->nextstmt);
                      fprintf(javafp,") ");
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
      /* I think these first two cases are obsolete now.  9/23/97, Keith */

      case STRING:
          printf("** I am going to emit a String literal!\n");
 	  fprintf (javafp, "\"%s\"", root->astnode.ident.name);
	  break;
      case CHAR:
          printf("** I am going to emit a String/char literal!\n");
	  fprintf (javafp, "\"%s\"", root->astnode.ident.name);
	  break;
      case NAME:
      default:
	  /* At some point in here I will have to switch on the
	     token type check whether it is a variable or
	     string or character literal. Also have to look up whether
	     name is intrinsic or external.  */

	  if (root->astnode.ident.arraylist == NULL) {


            if(hashtemp == NULL) {
	      fprintf (javafp, "%s", root->astnode.ident.name);
            } 
            else {
              if(root->parent == NULL) {
                fprintf(stderr,"name_emit(): NO PARENT!\n");
              } 
              else {
                if (root->parent->nodetype == Call) {
                  if((type_lookup(external_table, 
                                  root->parent->astnode.ident.name) != NULL))
                  {
                    fprintf (javafp, "%s,0", root->astnode.ident.name);
                  }
                  else {
	            fprintf (javafp, "%s", root->astnode.ident.name);
                  }
                } else {
	          fprintf (javafp, "%s", root->astnode.ident.name);
                }
              }
            }
          }
	  else if (hashtemp != NULL)
	    {
               int is_arg=FALSE;

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

                  if( type_lookup(args_table,root->astnode.ident.name) != NULL )
                    is_arg = TRUE;
                  else
                    is_arg = FALSE;

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
                        fprintf (javafp, "(");
                        expr_emit (temp);
                        fprintf (javafp, "- 1)");
                        if (hashtemp->variable->astnode.ident.leaddim[0] != '*' &&
                            temp->nextstmt != NULL)
                        {
                          temp = temp->nextstmt;
                          fprintf (javafp, "+");
                          fprintf (javafp, "(");
                          expr_emit (temp);
                          fprintf (javafp, "- 1)");
                          fprintf (javafp, "*");
                          fprintf(javafp,  "%s", 
                               hashtemp->variable->astnode.ident.leaddim);
                        }  /* Multi dimension.  */
                        if(is_arg)
                          fprintf(javafp,  "+ _%s_offset",root->astnode.ident.name);

#endif
#if TWOD
                        printf("TWOD not implemented yet!\n");
#endif
                    } else {
                        /* I dont think this is an external function */
#if ONED
                       printf("NOT an EXTERNAL function!\n");
                       if( type_lookup(args_table,root->astnode.ident.name) != NULL )
                         printf("This is an ARG\n");
                       else
                         printf("This is NOT an ARG\n");

                        fprintf (javafp, "[");
                        fprintf (javafp, "(");
                        expr_emit (temp);
                        fprintf (javafp, "- 1)");
                        if (hashtemp->variable->astnode.ident.leaddim[0] != '*' &&
                            temp->nextstmt != NULL)
                        {
                          temp = temp->nextstmt;
                          fprintf (javafp, "+");
                          fprintf (javafp, "(");
                          expr_emit (temp);
                          fprintf (javafp, "- 1)");
                          fprintf (javafp, "*");
                          fprintf(javafp,  "%s", 
                               hashtemp->variable->astnode.ident.leaddim);
                        }  /* Multi dimension.  */
                        if(is_arg)
                          fprintf(javafp,  "+ _%s_offset",root->astnode.ident.name);
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
                      fprintf (javafp, "(");
                      expr_emit (temp);
                      fprintf (javafp, "- 1)");
                      if (hashtemp->variable->astnode.ident.leaddim[0] != '*' &&
                          temp->nextstmt != NULL)
                      {
                        temp = temp->nextstmt;
                        fprintf (javafp, "+");
                        fprintf (javafp, "(");
                        expr_emit (temp);
                        fprintf (javafp, "- 1)");
                        fprintf (javafp, "*");
                        fprintf(javafp,  "%s", 
                             hashtemp->variable->astnode.ident.leaddim);
                      }  /* Multi dimension.  */
                      if(is_arg)
                        fprintf(javafp,  "+ _%s_offset",root->astnode.ident.name);
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
                /* else it's not in the array table? */
                char *tempstr;

                tempstr = strdup (root->astnode.ident.name);
                *tempstr = toupper (*tempstr);

		fprintf (javafp, "%s.%s", tempstr,root->astnode.ident.name);
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
          if(root->token == STRING)
	    fprintf (javafp, "\"%s\"", root->astnode.ident.name);
          else
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
	   switch (root->token)
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
      case Substring:
          fprintf(javafp,"%s.substring((",root->astnode.ident.name);
          expr_emit(root->astnode.ident.arraylist);
          fprintf(javafp,")-1,");
          expr_emit(root->astnode.ident.arraylist->nextstmt);
          fprintf(javafp,")");
          break;
      default:
          fprintf(stderr,"Warning: Unknown nodetype in expr_emit(): %s\n",
            print_nodetype(root));
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
    char * filename;
    char * classname;

    /* In fortran, functions return a value implicitely
       associated with there own name. In java, we declare a
       variable in the constructor that shadows the class
       (function) name and returns the same type. */

    filename = lowercase(strdup(root->astnode.source.name->astnode.ident.name));
    *filename = toupper (*filename);
    classname = strdup(filename);
    strcat(filename,".java");

    printf("filename is %s\n",filename);

    if((javafp = fopen(filename,"w"))==NULL) {
      fprintf(stderr,"Cannot open output file '%s'.\n",filename);
      perror("Reason");
      exit(1);
    }

    javaheader(javafp,classname);  /* print header to output file */

    if (root->nodetype == Function)
    {
      returns = root->astnode.source.returns;

      /* Test code.... */
      fprintf (javafp, "static %s %s;\n\n", returnstring[returns],
         root->astnode.source.name->astnode.ident.name);

      /* Define the constructor for the class. */
      fprintf (javafp, "\npublic static %s %s (",
        returnstring[returns],
        lowercase(strdup(root->astnode.source.name->astnode.ident.name)));

    }
    /* Else we have a subroutine, which returns void. */
    else if(root->nodetype == Subroutine)
    {
      fprintf (javafp, "\npublic static void %s (",
        root->astnode.source.name->astnode.ident.name);
    }
    else  /* Else we have a program, create a main() function */
    {
      fprintf (javafp, "\npublic static void main (String [] args");
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
                strcat( temp2, "_offset");
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

/* push this do loop's number on the stack */
    list_push(&doloop, root->astnode.forloop.Continue->astnode.label.number);

/*  Some point I will need to test whether this is really a name
   because it will crash if not.  */
    indexname = root->astnode.forloop.start->astnode.assignment.lhs->astnode.ident.name;

/* print out a label for this for loop */
   fprintf(javafp, "forloop%d:\n",root->astnode.forloop.Continue->astnode.label.number);
   
/* This block writes out the loop parameters.  */
    fprintf (javafp, "for (");
    assign_emit (root->astnode.forloop.start);
    /* fprintf (javafp, " -1; %s<", indexname); */
    fprintf (javafp, "; %s<=", indexname);
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

/* finally pop this loop's label number off the stack and 
   emit the label (for experimental goto resolution) */
   
    fprintf(javafp,"Dummy.label(%d);\n",list_pop(&doloop));
}

/* 
   Since gotos aren't supported by java, we can't just emit a goto here.
   labeled continues and breaks are supported in java, but only in certain 
   cases.  so, if we are within a loop, and we are trying to goto the CONTINUE 
   statement of an enclosing loop, then we can just emit a labeled continue 
   statement.  --Keith       
*/

goto_emit (AST * root)
{
    /* fprintf (javafp, "goto label%d;\n", root->astnode.go_to.label); 
    fprintf(stderr,"WARNING: ignoring goto encountered in fortran source.\n");
    fprintf (javafp, " break label%d; // was \"goto label%d\"\n;\n", 
         root->astnode.go_to.label, root->astnode.go_to.label);
    */
 
/* this code isn't working under the following condition:

   10  continue
       if(i .lt. 10) then
         write(*,*) i
         i = i + 1
   20    continue
         if(j .lt. 20) then
           write(*,*) j
           j = j + 1
           goto 10
         endif
         go to 10
       endif
  
   The outer loop should be translated to a java while loop, but
   the inner loop should remain an if statement.  that is not 
   happening yet.
*/

    if(doloop != NULL)
    {
      if( list_search(&doloop, root->astnode.go_to.label ) )
      {
         /* we are inside a do loop and we are looking at a goto
            statement to the 'continue' statement of an enclosing loop.
            what we want to do here is just emit a 'labeled continue' */ 

         fprintf(javafp,"continue forloop%d;\n",root->astnode.go_to.label);
      }    
    }
    else if(while_list != NULL)
    {
      if( list_examine(&while_list) == root->astnode.go_to.label )
      {
         /* we are inside a simulated while loop and we are looking at 
            a goto statement to the 'beginning' statement of the most
            enclosing if statment.  Since we are translating this to an 
            actual while loop, we ignore this goto statement */
 
        fprintf(javafp,"// goto %d (end while)\n",root->astnode.go_to.label);
         ;
      }    
      else
        fprintf(javafp,"Dummy.go_to(%d);\n",root->astnode.go_to.label);
    }
    else 
    {
      /* otherwise, not quite sure what to do with this one, so
         we'll just emit a dummy goto */

      fprintf(javafp,"Dummy.go_to(%d);\n",root->astnode.go_to.label);
    }
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
  if (root->astnode.label.stmt != NULL) {
    if (root->astnode.label.stmt->nodetype != Format) {
      fprintf(javafp,"{\n");
      fprintf (javafp, "label%d:\n   ", root->astnode.label.number);
      fprintf(javafp,"Dummy.label(%d);\n",root->astnode.label.number);
      emit (root->astnode.label.stmt);
      fprintf(javafp,"}\n");
    }
  } 
  else {
    fprintf(javafp,"{\n");
    fprintf (javafp, "label%d:\n   ", root->astnode.label.number);
    fprintf(javafp,"Dummy.label(%d);\n",root->astnode.label.number);
    fprintf(javafp,"}\n");
  }
}

int
write_emit (AST * root)
{
  HASHNODE *hnode;
  AST *temp;
  AST *nodeptr;
  char tmp[100];

  fprintf (javafp, "System.out.println(");

  sprintf(tmp,"%d", root->astnode.io_stmt.format_num);
  printf("***Looking for format statement number: %s\n",tmp);

  if( (hnode = format_lookup(format_table,tmp)) != NULL ) {
    printf("****FOUND****\n");
    if(hnode->variable == NULL)
      printf("###temp is NULL\n");
    else
      printf("###temp is non-NULL\n");

    nodeptr = root->astnode.io_stmt.arg_list; 

    format_list_emit(hnode->variable->astnode.label.stmt,&nodeptr);
  }
  else {
    printf("****NOT FOUND****\n");

    for( temp = root->astnode.io_stmt.arg_list; 
      temp != NULL; 
      temp = temp->nextstmt) 
    {
      expr_emit (temp);
      if(temp->nextstmt != NULL)
        fprintf (javafp, " + \"\" + ");
    }
  }

  fprintf (javafp, ");\n");
}

format_list_emit(AST *node, AST **nptr)
{
  AST *temp = node;

  while(temp != NULL)
    temp = format_item_emit(temp,nptr);
}

AST *
format_item_emit(AST *temp, AST **nodeptr)
{
  int i;

  switch(temp->token) {
    case EDIT_DESC:
    case NAME:
      printf("NAme/EDIT_DESC\n");
      format_name_emit(*nodeptr);
      if(*nodeptr == NULL)
        fprintf(stderr,"ERROR, nodeptr now null (Name)\n");
      else {
        printf("** Advancing nodeptr ** \n");
        *nodeptr = (*nodeptr)->nextstmt;
      }
      if(temp->nextstmt != NULL)
        fprintf(javafp," + ");
      return(temp->nextstmt);
      break;
    case STRING:
      printf("STring: %s\n",temp->astnode.ident.name);
      fprintf(javafp,"\"%s\" ",temp->astnode.ident.name);
      if(temp->nextstmt != NULL)
        fprintf(javafp," + ");
      return(temp->nextstmt);
      break;
    case REPEAT:
      printf("Repeat %d\n",temp->astnode.label.number);
      for(i=0;i<temp->astnode.label.number;i++) {
        format_list_emit(temp->astnode.label.stmt,nodeptr);

        if((i < temp->astnode.label.number -1) || 
          ((i == temp->astnode.label.number -1) && (temp->nextstmt != NULL)))
              fprintf(javafp," + ");
      }
      return(temp->nextstmt);
      break;
    case INTEGER:
      printf("INteger %d\n",atoi(temp->astnode.constant.number));

      if(temp->nextstmt != NULL) {
        if(temp->nextstmt->token != REPEAT) {
          if(temp->nextstmt->astnode.ident.name[0] == 'X') {
            fprintf(javafp,"\"");
            for(i=0;i< atoi(temp->astnode.constant.number);i++)
              fprintf(javafp," ");
            fprintf(javafp,"\"");
            if(temp->nextstmt->nextstmt != NULL)
              fprintf(javafp," + ");
            temp=temp->nextstmt;  /* consume edit desc */
          }
        }
      }
      else {
        fprintf(stderr,"Bad format spec!\n");
        fprintf(javafp," );\n");
        return(temp->nextstmt);
      }

      return(temp->nextstmt);
      break;
    case CM:
      printf("Comma\n");
      return(temp->nextstmt);
      break;
    case DIV:
      printf("Div\n");
      fprintf(javafp,"\"\\n\" ");
      if(temp->nextstmt != NULL)
        fprintf(javafp," + ");
      return(temp->nextstmt);
      break;
    case CAT:
      printf("two divs\n");
      fprintf(javafp,"\"\\n\\n\" ");
      if(temp->nextstmt != NULL)
        fprintf(javafp," + ");
      return(temp->nextstmt);
      break;
    default:
      printf("Unknown token!!! %d (%s) - ",temp->token,
         tok2str(temp->token));
      printf("this node type %s\n",print_nodetype(temp));
      return(temp->nextstmt);
      break;
  }
}

void
format_name_emit(AST *node)
{
  if(node == NULL) {
    printf("Bad formatting!\n");
    fprintf(javafp,"\" NULL \"");
  }
  else {
      /* in the write statement is an array, with no index specified.
         so we will keep grabbing data from the array until the end
         of the format specification */
/*
    if( (node->token == NAME) && 
        (type_lookup(array_table, root->astnode.ident.name) != NULL) &&
        (root->astnode.ident.arraylist == NULL) )
    {

    
    }
    else
*/
      expr_emit(node);
  }
  fprintf(javafp," + \"\" ");
}

int
blockif_emit (AST * root)
{
  AST *prev = root->prevstmt;
  AST *temp;

  if(prev != NULL)
    if(prev->nodetype == Label)
      if(prev->astnode.label.stmt == NULL)
        if((root->astnode.blockif.elseifstmts == NULL) &&
           (root->astnode.blockif.elsestmts == NULL))
        {
          for
           (
            temp=root->astnode.blockif.stmts;
            temp->nextstmt!=NULL;
            temp = temp->nextstmt
           )
              ; /* do nothing */
          if(temp->nodetype == Goto)
            if(temp->astnode.go_to.label == prev->astnode.label.number) {
              while_emit(root);
              return;
            }
        }

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
while_emit(AST *root)
{
  /* push this while loop's number on the stack */
  list_push(&while_list, root->prevstmt->astnode.label.number);

  fprintf(javafp, "while (");
  if (root->astnode.blockif.conds != NULL)
    expr_emit (root->astnode.blockif.conds);
  fprintf (javafp, ")  {\n    ");
  emit (root->astnode.blockif.stmts);
  fprintf (javafp, "}              // Close if()\n");

  /* finally pop this while loop's label number off the stack 
     and emit the label (for experimental goto resolution) */
   
  fprintf(javafp,"Dummy.label(%d);\n",list_pop(&while_list));
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
printf("**rhs nodetype is %s\n",print_nodetype(root->astnode.assignment.rhs));
    expr_emit (root->astnode.assignment.rhs);
}

char * print_nodetype (AST *root) 
{
  static char temp[100];

  switch (root->nodetype)
  {
    case 0:
      return("Bad Node");
    case Source:
      return("Source");
    case Progunit:
      return("Progunit");
    case Subroutine:
      return("Subroutine");
    case Function:
      return("Function");
    case Specification:
      return("Specification");
    case Statement:
      return("Statement");
    case Assignment:
      return("Assignment");
    case Call:
      return("Call");
    case Forloop:
      return("Forloop");
    case Blockif:
      return("Blockif");
    case Elseif:
      return("Elseif");
    case Else:
      return("Else");
    case Identifier:
      return("Identifier");
    case Method:
      return("Method");
    case Expression:
      return("Expression");
    case Typedec:
      return("Typedec");
    case Logicalif:
      return("Logicalif");
    case Return:
      return("Return");
    case Goto:
      return("Goto");
    case Label:
      return("Label");
    case Relationalop:
      return("Relationalop");
    case Logicalop:
      return("Logicalop");
    case Binaryop:
      return("Binaryop");
    case Unaryop:
      return("Unaryop");
    case End:
      return("End");
    case Unimplemented:
      return("Unimplemented");
    case Constant:
      return("Constant");
    case Format:
      return("Format");
    default:
      sprintf(temp, "print_nodetype(): Unknown Node: %d", root->nodetype);
      return(temp);
  }
}
