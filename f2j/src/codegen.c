/*
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */


/*  
 * codegen.c
 *   Generates java source code for checking.
 */

#include<stdio.h>
#include<string.h>
#include<ctype.h>
#include"f2j.h"
#include"f2jparse.tab.h"
#include"dlist.h"

#define ONED 1
#define TWOD 0

#define NONSTATIC 0
#define STATIC_NODATA 1
#define STATIC_WITHDATA 2

char * strdup ( const char * );
char * print_nodetype ( AST * ); 
char * lowercase ( char * );
HASHNODE * format_lookup(SYMTABLE *, char *);
char * methodscan (METHODTAB * , char * );
void format_name_emit(AST *);

/*  
 *   Global variables, a necessary evil when working with
 * yacc. 
 */

char *unit_name;
char *returnname;
char *cur_filename;
int gendebug = 1;
int cur_idx = 0;
int return_label = 0;
Dlist doloop = NULL;
Dlist while_list = NULL;
Dlist adapter_list = NULL;

SUBSTITUTION global_sub = { NULL, 0 };

extern char *inputfilename;

FILE *javafp;
FILE *curfp;

SYMTABLE *cur_type_table;
SYMTABLE *cur_external_table;
SYMTABLE *cur_intrinsic_table;
SYMTABLE *cur_args_table;
SYMTABLE *cur_array_table; 
SYMTABLE *cur_format_table; 
SYMTABLE *cur_data_table; 
SYMTABLE *cur_save_table; 
SYMTABLE *cur_common_table; 
SYMTABLE *cur_param_table; 
AST *cur_dataList;

/* data types for arrays */

char *returnstring[] =
{"String", "String", "complex", "double", "float", "int", "boolean"};

/* data types for scalars */

char *wrapper_returns[] =
{"StringW", "StringW", "complexW", "doubleW", "floatW", "intW", "booleanW"};

/* initial values for above data types */

char *init_vals[] =
{"\" \"", "\" \"", "0", "0.0", "0.0", "0", "false"};

/*
 * emit()
 *
 * This is the main code generation function.  We traverse the
 * AST and call recursively call emit() on each node.  This
 * function figures out what kind of node it's looking at and
 * calls the appropriate function to handle the code generation.
 *
 */

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
        {
          AST *tmp;

	  if (gendebug)
	      printf ("Source.\n");

          /* First set up the local hash tables. */

          cur_type_table = root->astnode.source.type_table;
          cur_external_table = root->astnode.source.external_table;
          cur_intrinsic_table = root->astnode.source.intrinsic_table;
          cur_args_table = root->astnode.source.args_table;
          cur_array_table = root->astnode.source.array_table;
          cur_format_table = root->astnode.source.format_table;
          cur_data_table = root->astnode.source.data_table;
          cur_save_table = root->astnode.source.save_table;
          cur_common_table = root->astnode.source.common_table;
          cur_param_table = root->astnode.source.parameter_table;
          cur_dataList = root->astnode.source.dataStmtList;

          while_list = make_dl();
          doloop = make_dl();
          adapter_list = make_dl();

          open_output_file(root->astnode.source.progtype);
          curfp = javafp;

	  fprintf (curfp, "// Type declarations.\n");
	  emit (root->astnode.source.typedecs);
          
	  emit (root->astnode.source.progtype);
	  fprintf (curfp, "\n// Executable code.\n");
	  emit (root->astnode.source.statements);

          emit_adapters();

          fprintf(curfp,"} // End class.\n");
          fclose(curfp);
	  break;
        }
      case Subroutine:
	  if (gendebug)
	      printf ("Subroutine.\n");
	  returnname = NULL;	/* Subroutines return void. */
          unit_name = root->astnode.source.name->astnode.ident.name;
	  constructor (root);
	  break;
      case Function:
	  if (gendebug)
	      printf ("Function.\n");
	  returnname = root->astnode.source.name->astnode.ident.name;
          unit_name = root->astnode.source.name->astnode.ident.name;
          if(gendebug)
            printf ("Function name: %s\n", 
              root->astnode.source.name->astnode.ident.name);
	  constructor (root);
	  break;
      case Program:
	  if (gendebug)
	      printf ("Program.\n");
	  returnname = NULL;	/* programs return void. */
          unit_name = root->astnode.source.name->astnode.ident.name;
	  if (gendebug)
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
	  fprintf (curfp, ";\n");
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
          {
            if(returnname != NULL)
	      printf ("Return: %s.\n", returnname);
            else
	      printf ("Return.\n");
          }
            
          /*
           * According to the f77 spec, labels cannot contain more
           * than five digits, so we use six nines as the label
           * for the final return statement to avoid conflicts with
           * labels that already exist in the program.
           */

          fprintf(curfp,"Dummy.go_to(\"%s\",999999);\n",cur_filename,
            return_label);

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
      case ComputedGoto:
	  if (gendebug)
	      printf ("Goto.\n");
	  computed_goto_emit (root);
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
	  if (gendebug)
            printf("skipping format statement\n");
	  if (root->nextstmt != NULL)
	      emit (root->nextstmt);
          break;
      case Stop:
          if (gendebug)
            printf ("Stop.\n");

          fprintf (curfp, "System.exit(1);\n");

          if (root->nextstmt != NULL)
            emit (root->nextstmt);
	  break;
      case End:
	  if (gendebug)
	      printf ("End.\n");

          /*
           * We only generate one real return statement.  The
           * other return statements are emitted as gotos to
           * the end of the code.  See the tech report for the
           * reasoning behind this decision.  Anyway, here at 
           * the end, we emit the real return statement.
           * We use six nines as the label to avoid conflicts 
           * with other labels.  See comment above in the Return 
           * case.
           */

          fprintf(curfp,"Dummy.label(\"%s\",999999);\n",cur_filename); 

          if (returnname != NULL)
	      fprintf (curfp, "return %s.val;\n", returnname);
	  else
	      fprintf (curfp, "return;\n");

	  fprintf (curfp, "   }\n");
	  break;
      case Save:
	  if (gendebug)
	      printf ("Save (ignoring).\n");
          if (root->nextstmt != NULL)
            emit (root->nextstmt);
	  break;
      case Common:
	  if (gendebug)
	      printf ("Common.\n");
          common_emit(root);
          if (root->nextstmt != NULL)
            emit (root->nextstmt);
	  break;
      case Unimplemented:
	  fprintf (curfp, "// WARNING: Unimplemented statement in Fortran source.\n");
	  if (root->nextstmt != NULL)
	      emit (root->nextstmt);
	  break;
      case Constant:
      default:
          fprintf(stderr,"emit(): Error, bad nodetype (%s)\n",
            print_nodetype(root));
      }				/* switch on nodetype.  */
}

/*  
 *  This function emits common blocks as a static class containing
 *  the variables specified in the COMMON statement.  Currently,
 *  each COMMON statement must specify the same variable names for
 *  the translation to work reliably.     10/9/97   --Keith    
 *
 *  Now COMMON statements may use different variable names and
 *  f2java attempts to merge the names into one.  --Keith
 */

int
common_emit(AST *root)
{
  extern char *returnstring[];
  HASHNODE *hashtemp;
  AST *Ctemp, *Ntemp, *temp;
  char filename[100];
  FILE *commonfp;
  char * prefix = strtok(strdup(inputfilename),".");
  int needs_dec = FALSE;

  /*
   * Ctemp loops through each common block name specified
   * in the COMMON statement and Ntemp loops through each
   * variable in each common block. 
   */

  for(Ctemp=root;Ctemp!=NULL;Ctemp=Ctemp->nextstmt)
  {
    if(Ctemp->astnode.common.name != NULL) 
    {
      /* common block filename will be a concatenation of
       * the original input filename and the name of this
       * common block.
       */
      sprintf(filename,"%s_%s.java", prefix,
         Ctemp->astnode.common.name);

      if((commonfp = fopen(filename,"w"))==NULL) 
      {
        fprintf(stderr,"Cannot open output file '%s'.\n",filename);
        perror("Reason");
        exit(1);
      }
  
      curfp = commonfp;

      /* import util package for object wrapper classes */

      fprintf(curfp,"import org.netlib.util.*;\n\n");

      if(Ctemp->astnode.common.name != NULL)
        fprintf(curfp,"public class %s_%s\n{\n",prefix,
          Ctemp->astnode.common.name);
  
      for(Ntemp=Ctemp->astnode.common.nlist;Ntemp!=NULL;Ntemp=Ntemp->nextstmt)
      {
        needs_dec = FALSE;

        if(gendebug)
        {
          printf("Common block %s -- %s\n",Ctemp->astnode.common.name,
            Ntemp->astnode.ident.name);
          printf("Looking up %s in the type table\n",Ntemp->astnode.ident.name);
        }
  
        /* each variable in the common block should have a type
         * declaration associated with it.
         */
        if((hashtemp = type_lookup(cur_type_table,Ntemp->astnode.ident.name)) == NULL)
        {
          fprintf(stderr,"Error: can't find type for common %s\n",
            Ntemp->astnode.ident.name);
          if(gendebug)
            printf("Not Found\n");
          continue;
        }

        if(gendebug)
          printf("Found\n");

        temp = hashtemp->variable;

        if(temp->astnode.ident.needs_declaration)
          needs_dec = TRUE;

        /* now emit the variable declaration as with any
         * other variable.
         */
        if(type_lookup(cur_data_table,Ntemp->astnode.ident.name) && !needs_dec)
          /* vardec_emit(temp, temp->vartype, STATIC_WITHDATA); */
          vardec_emit(temp, temp->vartype);
        else
          /* vardec_emit(temp, temp->vartype, STATIC_NODATA); */
          vardec_emit(temp, temp->vartype);
      }
      if(Ctemp->astnode.common.name != NULL)
        fprintf(curfp,"}\n");
  
      fclose(curfp);
    }
  }
  curfp = javafp;
}

/* Emit all the type declarations.  This procedure checks
   whether variables are typed in the argument list, and
   does not redeclare those arguments. */
int
typedec_emit (AST * root)
{
  extern METHODTAB intrinsic_toks[];
  AST *temp;
  AST *temp2;
  HASHNODE *hashtemp, *ht;
  enum returntype returns;
  char *tempname;

  temp = root->astnode.typeunit.declist;

  /* 
   *  This may have to be moved into the looop also.  Could be
   *  why I have had problems with this stuff.  
   */
  hashtemp = type_lookup (cur_external_table, temp->astnode.ident.name);
  if (hashtemp)
    return 1;

  returns = root->astnode.typeunit.returns;

  /*  
   * Somewhere in here I need to do a table lookup
   * to see whether the variable is in the argument
   * list for the method.  If so, it takes the type
   * in the argument list and is not retyped here. 
   */
  for (temp; temp != NULL; temp = temp->nextstmt)
  {
    /* 
     * If there is a corresponding data statement for this
     * variable, don't emit anything here.  Just wait and
     * let the whole thing get emitted when we come across
     * the DATA node.  --9/22/97,  Keith 
     */

    if(type_lookup(cur_data_table,temp->astnode.ident.name)) {
      if(gendebug)
        printf("@@ Variable %s: Found corresponding data stmt\n",
          temp->astnode.ident.name);

      ht = type_lookup(cur_type_table,temp->astnode.ident.name);

      if(ht == NULL)
        continue;
      
      if( ! ht->variable->astnode.ident.needs_declaration)
        continue;
    }
    else
      if(gendebug)
       printf("@@ Variable %s: Corresponding data stmt not found\n",
         temp->astnode.ident.name);

      /* 
       * we already emitted this variable as a static variable 
       * (aka 'class variable'), so we don't emit it here. 
       */

    /*
     *  dont worry about checking the save table now since we're 
     *  going to emit everything as static variables.  --keith
     *
     *    if(type_lookup(cur_save_table,temp->astnode.ident.name))
     *      continue;
     */
 
    /* 
     * also do not try to redefine a 'common' variable since
     * they are placed in their own classes.  10-8-97 -- Keith 
     */

    if(type_lookup(cur_common_table,temp->astnode.ident.name))
      continue;

    /* 
     * Dont emit anything for intrinsic functions.
     */

    tempname = strdup(temp->astnode.ident.name);
    uppercase(tempname);

    if(( methodscan (intrinsic_toks, tempname) != NULL)
     && (type_lookup(cur_intrinsic_table,temp->astnode.ident.name) != NULL)) 
      continue;

     /* 
      * Let's do the argument lookup first. No need to retype variables
      * that are already declared in the argument list, or declared
      * as externals.  So if it is already declared, loop again.   
      */

    hashtemp = type_lookup (cur_args_table, temp->astnode.ident.name);
    if (hashtemp)
      continue;

    /* vardec_emit(temp, returns, NONSTATIC); */
    vardec_emit(temp, returns);
  }
}				/* Close typedec_emit(). */

/* 
 * the body of this function used to be in typedec_emit, but
 * I moved it so that I could use the same code to emit static
 * or nonstatic variables.   10/3/97  -- Keith 
 *
 * This could probably be simplified somewhat now that all
 * variables are emitted 'static'.   1/27/98 -- Keith
 */

/*
int vardec_emit(AST *root, enum returntype returns, int only_static)
*/
int
vardec_emit(AST *root, enum returntype returns)
{
  HASHNODE *hashtemp;
  AST *temp2;
  char *prefix;
  int count;
  int only_static = STATIC_NODATA;

  if(only_static)         /* true if only_static is either  */
    prefix = "static ";   /*   STATIC_WITHDATA or STATIC_NODATA */
  else
    prefix = "";

  if(gendebug)
    printf("ident = %s, prefix = %s\n",root->astnode.ident.name,prefix);

  /* 
   * check to see if this is an array declaration or not. 
   * if so, we must generate the appropriate "new" statement.
   * otherwise, just declare & initialize in one statement. --keith 
   */

  if(root->astnode.ident.arraylist != NULL) {
    fprintf (curfp, "%s%s [] ",prefix, returnstring[returns]);

    if (gendebug)
      printf ("%s\n", returnstring[returns]);
    name_emit (root);

    if(only_static == STATIC_WITHDATA) {
      fprintf (curfp, ";\n");
    }
    else {
      if (returns == Integer)
        fprintf (curfp, "= new int[");
      else if (returns == Double)
        fprintf (curfp, "= new double[");
      else if (returns == Logical)
        fprintf (curfp, "= new boolean[");
      else if ((returns == String) || (returns == Character))
        fprintf (curfp, "= new String[");
      else
        fprintf(stderr,"vardec_emit():  Unknown type (%d)!\n",returns);
         
      /* make sure this variable is in the array table */

      hashtemp = type_lookup(cur_array_table,root->astnode.ident.name);
      if(hashtemp != NULL) 
      {
        temp2=root->astnode.ident.arraylist;
        for(count=0 ; temp2!=NULL ; temp2=temp2->nextstmt, count++) 
        {
          if(temp2 != root->astnode.ident.arraylist)
            fprintf(curfp, " * ");   /* if not the first iteration */
          fprintf(curfp,"(");
          expr_emit(temp2);
          fprintf(curfp,")");
        }
      }
      else
        fprintf(stderr,"vardec_emit: Can't find %s in array table!\n",
           root->astnode.ident.name);

      fprintf (curfp, "];\n");
    }
  } else {    /* this is not an array declaration */

    HASHNODE *p;

    fprintf (curfp, "%s%s ", prefix, wrapper_returns[returns]);

    if (gendebug)
      printf ("%s\n", returnstring[returns]);

    name_emit (root);

    printf("looking for %s in parameter table\n",root->astnode.ident.name);

    p = type_lookup(cur_param_table, root->astnode.ident.name);

    /*  expr_emit(p->variable);  */

    if(only_static == STATIC_WITHDATA) {
      if (returns == Integer)
        fprintf (curfp, "= new intW(%s)", init_vals[returns]);
      else if (returns == Double)
        fprintf (curfp, "= new doubleW(%s)", init_vals[returns]);
      else if (returns == Logical)
        fprintf (curfp, "= new booleanW(%s)", init_vals[returns]);
      else if ((returns == String) || (returns == Character))
        print_string_initializer(root);
      fprintf (curfp, ";\n");
    } else {
      /*  
       * initialize local variables to zero or
       * false to keep the java compiler from
       * squawking.  
       */

      if(p != NULL)
      {
        /* this variable is declared as a parameter, so 
         * initialize it with the value from the PARAMETER
         * statement.
         */
        fprintf(curfp,"= new %s(",wrapper_returns[returns]);
        expr_emit(p->variable);
        fprintf(curfp,");\n");
      }
      else
      {
        /* this variable is not declared as a parameter, so
         * initialize it with an initial value depending on
         * its data type.
         */
        if ((returns == String) || (returns == Character))
        {
          print_string_initializer(root);
          fprintf(curfp,";\n");
        }
        else
          fprintf(curfp,"= new %s(%s);\n",wrapper_returns[returns],
            init_vals[returns]);
      }
    }
  }
}

/* 
 * This function prints the initialization code for a 
 * String object. 
 */

int
print_string_initializer(AST *root)
{
  HASHNODE *ht;

  ht = type_lookup(cur_type_table,root->astnode.ident.name);
  if(ht == NULL)
  {
    fprintf(stderr,"Weird...can't find %s in type_table\n",
      root->astnode.ident.name);
    fprintf (curfp, "= new StringW(%s)", init_vals[String]);
  }
  else
  {
    char buf[ ht->variable->astnode.ident.len ];

    sprintf(buf,"\"%*s\"",ht->variable->astnode.ident.len," ");
    fprintf(curfp,"= new StringW(%s)", buf);
  }
}

/* 
 * This function handles emitting DATA statements, which consist of a
 * list of names and a list of data items.  We start with the first name
 * and assign as many data items from the list as the size allows.  for
 * example if the first name is a 5 element array, we assign the first 5
 * data items to the first name.  then we go to the second name, third 
 * name, etc. and assign values in the same way.     10/3/97  --Keith
 */

int
data_emit(AST *root)
{
  enum returntype returnval;
  AST * Dtemp, *Ntemp, *Ctemp;
  AST * data_var_emit(AST *, AST *, HASHNODE *);
  AST * data_implied_loop_emit(AST * , AST *);
  HASHNODE *hashtemp;

  /* foreach Data spec... */
  for(Dtemp = root->astnode.label.stmt; Dtemp != NULL; Dtemp = Dtemp->prevstmt) 
  {
    Ctemp = Dtemp->astnode.data.clist;

    /* foreach variable... */    
    for(Ntemp = Dtemp->astnode.data.nlist;Ntemp != NULL;Ntemp=Ntemp->nextstmt) 
    {
      /* check to see if we're looking at an implied do loop */

      if(Ntemp->nodetype == Forloop) 
      {
        data_implied_loop_emit(Ntemp, Ctemp);
        continue;
      }

      /* This variable should have a type declaration associated with it */

      hashtemp = type_lookup(cur_type_table,Ntemp->astnode.ident.name);

      if(hashtemp == NULL)
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

      /* check to see if this variable is also part of a common block */

      if(type_lookup(cur_common_table, Ntemp->astnode.ident.name))
      {
        fprintf(stderr,"Warning: can't handle COMMON vars w/DATA statements.\n");
        continue;
      }

      Ctemp = data_var_emit(Ntemp,Ctemp,hashtemp);
    }
  }
}

/*
 * This function generates the code for implied do loops in DATA
 * statements.  The initialization is done in Java within a static
 * block.  For example, the following fortran statements:
 * 
 *    integer x
 *    data (x(j),j=1,4)/5,6,7,8/
 * 
 * would be emitted in Java as:
 * 
 *    static int [] x= new int[(4)];
 *    static {
 *    x[( 1 )- 1] = 5;
 *    x[( 2 )- 1] = 6;
 *    x[( 3 )- 1] = 7;
 *    x[( 4 )- 1] = 8;
 *    }
 *
 */

AST *
data_implied_loop_emit(AST * root, AST *Clist)
{
  AST * loop_var, * lhs;
  int start, stop, incr, i;
 
  printf("/* \n");
  printf("* looking at an implied data loop...\n");
  printf("*\n");

  start = atoi(root->astnode.forloop.start->astnode.constant.number);
  printf("* the start is: %d\n",start);

  stop = atoi(root->astnode.forloop.stop->astnode.constant.number);
  printf("* the stop is: %d\n",stop);

  if(root->astnode.forloop.incr != NULL)
    incr = atoi(root->astnode.forloop.incr->astnode.constant.number);
  else
    incr = 1;

  printf("* the increment is: %d\n",incr);

  loop_var = root->astnode.forloop.counter;
  printf("* the name for the loop var is: %s\n", 
    loop_var->astnode.ident.name);

  lhs = root->astnode.forloop.Label;
  printf("* the Lhs for this data stmt is: %s\n", 
    lhs->astnode.ident.name);
  
  global_sub.name = loop_var->astnode.ident.name;

  fprintf(curfp,"static {\n");
  for(i = start; i <= stop; i += incr)
  {
    global_sub.val = i;
    name_emit(lhs);
    fprintf(curfp, " = ");
    expr_emit(Clist);
    fprintf(curfp, ";\n");
    Clist = Clist->nextstmt;
  }
  fprintf(curfp,"}\n");

  printf("*/ \n");

  global_sub.name = NULL;

  return Clist;
}

/* 
 * This function emits variable declarations for those variables
 * originally contained in DATA statements in the fortran source.
 */

AST *
data_var_emit(AST *Ntemp, AST *Ctemp, HASHNODE *hashtemp)
{
  int i, length=1, is_array=FALSE, needs_dec = FALSE;
  AST * data_array_emit(int , AST *, AST *, int );

  /* check to see whether we're going to be assigning to
   * an array element.  If so, the declaration for the array
   * would have already been emitted, so we dont need a
   * declaration here - just assign the value.  Otherwise,
   * we do need a declaration. 
   */
  if(Ntemp->astnode.ident.arraylist == NULL)
    needs_dec = FALSE;
  else
    needs_dec = TRUE;

  /* here we determine whether this variable was declared as
   * an array or not.  hashtemp points to the symtable info.
   */
  if((hashtemp->variable->astnode.ident.arraylist != NULL ) && !needs_dec)
    is_array = TRUE;
  else
    is_array = FALSE;

  if( hashtemp->variable->astnode.ident.leaddim != NULL )
  {
    /* Check for attempts to initialize dummy argument.  we can't
     * determine the number of elements in a dummy arg. 
     */
    if(hashtemp->variable->astnode.ident.leaddim[0] == '*')
    {
      fprintf(stderr,"Attempt to initialize dummy argument: %s\n",
        hashtemp->variable->astnode.ident.name);
      return;
    }
    else if (type_lookup(cur_args_table,Ntemp->astnode.ident.name))
    {
      fprintf(stderr,"Attempt to initialize argument: %s\n",
        hashtemp->variable->astnode.ident.name);
      return;
    }

    /* determine how many elements are in this array so that
     * we know how many items from the DATA statement to assign
     * to this variable.
     */
    if(is_array)
      length = determine_var_length(hashtemp);
  }

  if(is_array)
  {
    fprintf(curfp,"static %s ", returnstring[ hashtemp->type]);
    Ctemp = data_array_emit(length, Ctemp, Ntemp, needs_dec);
  }
  else 
  {
    if(!needs_dec)
    {
         /*  can't remember why this code was here....
          *
          * if(type_lookup(cur_save_table,Ntemp->astnode.ident.name))
          *   fprintf(curfp,"static %s ", returnstring[ hashtemp->type]);
          * else
          */
      fprintf(curfp,"static %s ", wrapper_returns[ hashtemp->type]);
      data_scalar_emit(hashtemp->type, Ctemp, Ntemp, needs_dec);
    }
    else 
    {
      fprintf(curfp,"static {\n");
      data_scalar_emit(hashtemp->type, Ctemp, Ntemp, needs_dec);
      fprintf(curfp,"}\n");
    }

    Ctemp = Ctemp->nextstmt;
  }

  return Ctemp;
}

/* 
 * Determine the number of elements in this array variable 
 */

int
determine_var_length(HASHNODE *var)
{
  AST *temp2;

  int length = 1;

 
  temp2=var->variable->astnode.ident.arraylist;
  for( ; temp2 != NULL ; temp2=temp2->nextstmt ) {
    if(temp2->nodetype != Constant) {

      /*
       * fprintf(stderr,"Cant translate data statement for %s\n",
       *   var->variable->astnode.ident.name);
       */

      length = -1;
      break;
    }
    else {
      length *= atoi(temp2->astnode.constant.number);
    }
  }

  return length;
}

/* 
 * This function generates array declarations which are contained in
 * DATA statements.
 * 
 */

AST *
data_array_emit(int length, AST *Ctemp, AST *Ntemp, int needs_dec)
{
  int i, count =1;

  fprintf(curfp,"[] ");

  /* 
   * if this variable is static, we can't declare it here 
   * because it has been declared already as a class variable.
   * so we use the "_temp_" prefix and emit the initialization.
   * later we assign the temp variable to the class variable.
   * 10/3/97  --Keith
   */

/*
 * dont worry about SAVE statements now.  11/11/97 --keith
 *
 *  if(type_lookup(cur_save_table,Ntemp->astnode.ident.name) && !needs_dec)
 *    fprintf(curfp,"_temp_%s = {",Ntemp->astnode.ident.name);
 *  else
 */
    fprintf(curfp,"%s = {",Ntemp->astnode.ident.name);

  for(i=0,count=0;(length==-1)?(Ctemp != NULL):(i< length);i++,count++) {
    if(Ctemp->token == STRING)
      fprintf(curfp,"\"%s\" ",Ctemp->astnode.ident.name);
    else {
      fprintf(curfp,"%s%s ",  
        Ctemp->astnode.constant.sign == 1 ? "-" : "",
        Ctemp->astnode.constant.number);
    }

    /* 
     * Every now and then, emit a newline for readability.
     * I have run across some lines that end up so long that
     * they screw up 'vi'.   9/30/97  --Keith 
     */
    if( count % 5 == 0 )
      fprintf(curfp,"\n");

    if( (Ctemp = Ctemp->nextstmt) == NULL )
      break;
    else {
      if(length == -1)
      {
        if (Ctemp != NULL)
          fprintf(curfp,", ");
      }
      else 
        if(i != length -1 )
          fprintf(curfp,", ");
    }
  }
   
  fprintf(curfp,"};\n");
/*
 * dont worry about SAVE statements now.  11/11/97 --keith
 *
 *  if(type_lookup(cur_save_table,Ntemp->astnode.ident.name) && !needs_dec)
 *    fprintf(curfp,"%s = _temp_%s;\n",Ntemp->astnode.ident.name,
 *      Ntemp->astnode.ident.name);
 */

  return Ctemp;
}

/* 
 * This function generates declarations of scalar items which are
 * contained in DATA statements.
 * 
 */

int
data_scalar_emit(enum returntype type, AST *Ctemp, AST *Ntemp, int needs_dec)
{
  if(Ctemp->token == STRING) 
  {
    HASHNODE *ht;
    int len;

    /* find this string in the symbol table */
    ht = type_lookup(cur_type_table,Ntemp->astnode.ident.name);

    /* determine the length of the string (as declared in the fortran source) */
    if(ht == NULL)
      len = 1;
    else
      len = Ntemp->astnode.ident.len;

    /* now initialize the string to all blanks.  but we try to keep the length
     * of the string constant, otherwise some subscript operations get screwed
     * up.  so we initialize the string to n blanks, where n is the original 
     * string length.
     */

    if(!needs_dec)
    {
      fprintf(curfp,"%s = new StringW(\"%*s\");\n",
        Ntemp->astnode.ident.name, len,
        Ctemp->astnode.ident.name);
    }
    else
    {
      expr_emit(Ntemp);
      fprintf(curfp," = \"%*s\";\n", len, Ctemp->astnode.ident.name);
    }
  }
  else 
  {
    if(!needs_dec)
    {
      fprintf(curfp,"%s = new %s(%s%s);\n",Ntemp->astnode.ident.name,
        wrapper_returns[ type],
        Ctemp->astnode.constant.sign == 1 ? "-" : "",
        Ctemp->astnode.constant.number);
    }
    else
    {
      expr_emit(Ntemp);
      fprintf(curfp," = %s%s;\n",
        Ctemp->astnode.constant.sign == 1 ? "-" : "",
        Ctemp->astnode.constant.number);
    }
  }
}

/* 
 * A name will either fly solo or lead off
 * a named array.  So far, this code will emit
 * a name or an array with integer indices.  The
 * procedure also needs to check all relevant tables
 * to determine whether the name is an array or
 * a procedure (i.e. Class.method) call, and whether
 * the name is a STRING, CHAR, etc.  Frankly, this is
 * a hideous procedure and really needs to
 * be rewritten. 
 *
 * ...and it's getting worse by the day  --Keith
 *
 *  Heh... gotta love it...  -dmd  9/26/97
 *
 *  Started cleaning up name_emit  10/10/97  --Keith
 */

int
name_emit (AST * root)
{
  AST *temp;
  HASHNODE *hashtemp;
  char *javaname, * tempname;
  extern METHODTAB intrinsic_toks[];

  printf("entering name_emit\n");
  /*  
   *  Check to see whether name is in external table.  Names are
   *  loaded into the external table from the parser.   
   */

  if(root->nodetype == Identifier)
    if(root->token == STRING)
      printf("** maybe I should emit a string literal here\n");

  tempname = strdup(root->astnode.ident.name);
  uppercase(tempname);

  /* 
   * If the name is in the external table, then check to see if
   * it is an intrinsic function instead (e.g. SQRT, ABS, etc).  
   */

  if (type_lookup (cur_external_table, root->astnode.ident.name) != NULL)
    external_emit(root);  /* handles LSAME, LSAMEN */
  else if(( methodscan (intrinsic_toks, tempname) != NULL) 
     && ( (type_lookup(cur_intrinsic_table, root->astnode.ident.name) != NULL)
       || (type_lookup(cur_type_table, root->astnode.ident.name) == NULL)))
    intrinsic_emit(root);
  else
    switch (root->token)
    {
      /* 
       * I think the first case (STRING/CHAR) is obsolete now since string 
       * and char constants were moved to the Constant production.  
       * 9/23/97, Keith 
       */

      case STRING:
      case CHAR:
        if(gendebug)
          printf("** I am going to emit a String/char literal!\n");
        fprintf (curfp, "\"%s\"", root->astnode.ident.name);
        break;
      case INTRINSIC: 
        /* do nothing */
        break;
      case NAME:
      default:

        hashtemp = type_lookup (cur_array_table, root->astnode.ident.name);

        if (root->astnode.ident.arraylist == NULL)
          scalar_emit(root, hashtemp);
        else if (hashtemp != NULL)
          array_emit(root, hashtemp);
        else
          subcall_emit(root);
        break;
    }
  printf("leaving name_emit\n");
}

/*  This function emits a function call */

int 
subcall_emit(AST *root)
{
  AST *temp;
  char *tempstr;

  tempstr = strdup (root->astnode.ident.name);
  *tempstr = toupper (*tempstr);

  fprintf (curfp, "%s.%s", tempstr,root->astnode.ident.name);
  temp = root->astnode.ident.arraylist;

  fprintf (curfp, "(");
  if(temp->nodetype != EmptyArgList)
    for (temp; temp != NULL; temp = temp->nextstmt)
    {
      if(temp != root->astnode.ident.arraylist)
        fprintf (curfp, ",");  /* if not first iteration */
                        
      if (*temp->astnode.ident.name != '*')
        expr_emit (temp);
    }
  fprintf (curfp, ")");
}

/*  
 *  This function emits the index to an array.  The boolean argument
 *  is_arg represents whether the array is an argument to the current 
 *  function or subroutine and the boolean is_ext represents whether
 *  the array is being passed to an external function.
 */    

int
func_array_emit(AST *root, HASHNODE *hashtemp, char *arrayname, int is_arg, 
  int is_ext)
{
  HASHNODE *ht;

#if ONED
  if(is_ext)
    fprintf (curfp, ",");
  else
    fprintf (curfp, "[");

  printf("~looking up %s in the array table\n", arrayname);

  /* find this variable in the array table */
  ht = type_lookup(cur_array_table, arrayname);

  if(ht == NULL)
  {
      printf("~Could not find!\n");
  }
  else if(ht->variable->astnode.ident.dim == 3)
  {
    AST *temp;
    int i, offset;

    /* This section handles 3 dimensional array access.  we should already
     * know the dimensions of this array.
     */
    printf("~found %s, has dim %d\n",ht->variable->astnode.ident.name,
       ht->variable->astnode.ident.dim);

    printf("Ok, the dims are %d,%d,%d\n",
      ht->variable->astnode.ident.D[0],
      ht->variable->astnode.ident.D[1],
      ht->variable->astnode.ident.D[2]);

    offset = 1 + ( (1 + ht->variable->astnode.ident.D[1]) * 
        ht->variable->astnode.ident.D[0]);

    fprintf (curfp, "(");
    expr_emit(root);
    fprintf (curfp, ")");
    
    fprintf (curfp, "+((");

    fprintf (curfp, "(");
    expr_emit(root->nextstmt);
    fprintf (curfp, ")");
    
    fprintf (curfp, "+(");

    fprintf (curfp, "(");
    expr_emit(root->nextstmt->nextstmt);
    fprintf (curfp, ")");
    
    fprintf (curfp, " * %d)) *%d) - %d",
      ht->variable->astnode.ident.D[1],
      ht->variable->astnode.ident.D[0],offset);
  }
  else 
  {
    /* if this isn't a 3 dimensional array, it is handled here */

    fprintf (curfp, "(");
    expr_emit (root);
    fprintf (curfp, ")- 1");

    if((hashtemp->variable->astnode.ident.lead_expr != NULL)
         && root->nextstmt != NULL)
    {
      root = root->nextstmt;
      fprintf (curfp, "+");
      fprintf (curfp, "(");
      expr_emit (root);
      fprintf (curfp, "- 1)");
      fprintf (curfp, "* (");
      expr_emit(hashtemp->variable->astnode.ident.lead_expr);
      fprintf (curfp, ")");
    }
    else if((hashtemp->variable->astnode.ident.leaddim != NULL)
         && (hashtemp->variable->astnode.ident.leaddim[0] != '*')
         && (root->nextstmt != NULL))
    {
      root = root->nextstmt;
      fprintf (curfp, "+");
      fprintf (curfp, "(");
      expr_emit (root);
      fprintf (curfp, "- 1)");
      fprintf (curfp, "*");
      printf("leaddim = %s\n",hashtemp->variable->astnode.ident.leaddim);
      if(isalpha(hashtemp->variable->astnode.ident.leaddim[0]))
        fprintf(curfp,  "%s.val", hashtemp->variable->astnode.ident.leaddim);
      else
        fprintf(curfp,  "%s", hashtemp->variable->astnode.ident.leaddim);
    }  /* Multi dimension.  */
  }

  if(is_arg)
    fprintf(curfp,  "+ _%s_offset",arrayname);

  if(! is_ext)
    fprintf(curfp, "]");
#endif

#if TWOD
  fprintf(stderr,"TWOD not implemented yet!\n");
#endif
}

/* 
 * Here we emit array variables.  actually we first determine 
 * the context in which the array access is found and then call
 * func_array_emit() to emit the array index.
 * 10/10/97 --Keith
 */

int
array_emit(AST *root, HASHNODE *hashtemp)
{
  extern METHODTAB intrinsic_toks[];
  AST *temp;
  int is_arg=FALSE;
  char *get_common_prefix(char *);
  char *com_prefix;
  char *name;

  if (gendebug)
    printf ("Array... %s, My node type is %s\n", 
      root->astnode.ident.name,
      print_nodetype(root));

  com_prefix = get_common_prefix(root->astnode.ident.name);

  name = root->astnode.ident.name;

  if(com_prefix[0] != '\0')
  {
    HASHNODE *ht;

    ht = type_lookup(cur_type_table,root->astnode.ident.name);
    if (ht == NULL)
      fprintf(stderr,"array_emit:Cant find %s in type_table\n",
          root->astnode.ident.name);

    if(ht->variable->astnode.ident.merged_name != NULL)
      name = ht->variable->astnode.ident.merged_name;
  }

  if (name == NULL)
  {
    fprintf(stderr,"array_emit: setting name to NULL!\n");
    name = root->astnode.ident.name;
  }

  if(gendebug)
    printf("### #in array_emit, setting name = %s\n",name);

  /* 
   * Now, what needs to happen here is the context of the
   * array needs to be determined.  If the array is being
   * passed as a parameter to a method, then the array index
   * needs to be passed separately and the array passed as
   * itself.  If not, then an array value is being set,
   * so dereference with index arithmetic.  
   */

  if((root->parent != NULL) && (root->parent->nodetype == Typedec))
    fprintf (curfp, "%s", name);
  else
    fprintf (curfp, "%s%s", com_prefix, name);

  temp = root->astnode.ident.arraylist;

  if(root->parent == NULL) {
    /* Under normal circumstances, I dont think this should 
       be reached */
    fprintf (stderr,"Array... %s, NO PARENT - ", name);
    fprintf (stderr,"This is not good!\n");
  } else {
    if(gendebug)
      printf ("Array... %s, Parent node type... %s\n", 
        name,
        print_nodetype(root->parent));

    if( type_lookup(cur_args_table,root->astnode.ident.name) != NULL )
      is_arg = TRUE;
    else
      is_arg = FALSE;

    if((root->parent->nodetype == Call)) 
    {
      if((type_lookup(cur_external_table, root->parent->astnode.ident.name) != NULL))
        func_array_emit(temp, hashtemp, root->astnode.ident.name, is_arg, TRUE);
      else 
        func_array_emit(temp, hashtemp, root->astnode.ident.name, is_arg, FALSE);
    } 
    else if((root->parent->nodetype == Typedec)) 
    {
      /*  Just a declaration, don't emit index. */
      if(gendebug)
        printf("I guess this is just an array declaration\n");
    }
    else 
      func_array_emit(temp, hashtemp, root->astnode.ident.name, is_arg, FALSE);
  }
}

/* 
 * If the variable is in a common block, this function returns the name of
 * the class file in which it is declared.  Otherwise, it returns a blank
 * string.
 */

char *
get_common_prefix(char *varname)
{
  HASHNODE *ht;
  char * prefix = strtok(strdup(inputfilename),".");
  static char * cprefix;

  ht = type_lookup(cur_common_table, varname);

  if(ht == NULL)
    cprefix = "";
  else {
    cprefix = (char *) malloc(
       strlen(ht->variable->astnode.ident.commonBlockName) +
       strlen(prefix) + 3);

    sprintf(cprefix,"%s_%s.", prefix,
      ht->variable->astnode.ident.commonBlockName);
  }

  return(cprefix);
}

/*  
 *  This function emits a scalar variable.  The first thing that needs
 *  to be checked here is whether the variable is part of a common block.
 *  If so, we need to emit the common block name followed by a dot and
 *  the variable name.  Otherwise, just emit the variable name.   If using
 *  object wrappers, the nodetype of the parent node must be checked.  If the 
 *  parent node is a 'call' to an external function then the variables must 
 *  be passed as objects.  Otherwise, the value from the wrapper should be 
 *  obtained by appending .val to the variable name.   10/10/97  -- Keith
 * 
 *  (note: this function also emits array variables which do not have
 *   indices since they look like scalars to the parser)
 */

int
scalar_emit(AST *root, HASHNODE *hashtemp)
{
  extern METHODTAB intrinsic_toks[];
  char *com_prefix;
  char *name;

  /* get the name of the common block class file, if applicable */

  com_prefix = get_common_prefix(root->astnode.ident.name);

  name = root->astnode.ident.name;

  if(com_prefix[0] != '\0')
  {
    HASHNODE *ht;

    ht = type_lookup(cur_type_table,root->astnode.ident.name);
    if (ht == NULL)
      fprintf(stderr,"scalar_emit:Cant find %s in type_table\n",
          root->astnode.ident.name);

    if(ht->variable->astnode.ident.merged_name != NULL)
      name = ht->variable->astnode.ident.merged_name;
  }

  if (name == NULL)
  {
    fprintf(stderr,"scalar_emit: setting name to NULL!\n");
    name = root->astnode.ident.name;
  }

  if(hashtemp == NULL) {
    /* if hashtemp is NULL, then this variable is not in the
     * array table.
     */
    if(gendebug) {
      printf("here we are emitting a scalar: %s, len = %d",
        root->astnode.ident.name, root->astnode.ident.len);
      printf("The parent node is : %s\n",print_nodetype(root->parent));
    }
 
    if(gendebug)
      printf("### #in scalar_emit, setting name = %s\n",name);

    if(root->parent == NULL) {
      fprintf(stderr,"name_emit(): NO PARENT! (%s)\n", name);
    } else {
      if (root->parent->nodetype == Call) {
        char *tempname;

        if(gendebug)
          printf("in CALL, '%s' <- '%s'\n", 
            root->parent->astnode.ident.name,
            name);

        tempname = strdup(root->parent->astnode.ident.name);
        uppercase(tempname);

        /* determine whether the parent (a call) is an intrinsic or an 
         * array access.  If neither, we pass the scalar as is - wrapped
         * in an object.  This provides the ability to simulate pass by
         * reference in Java.  If the parent is either an intrinsic 
         * function call or an array access, we must pass the actual value.
         * Fortran intrinsics are implemented using functions from the core
         * Java API which only take primitive types as arguments.  And arrays
         * must always be indexed using primitive integers.  Therefore, in
         * those two cases, we must emit the primitive value obtained by
         * appending ".val" to the wrapper object.
         */
        if((methodscan (intrinsic_toks, tempname) == NULL) &&
           (type_lookup(cur_array_table, root->parent->astnode.ident.name) == NULL))
        {
          if(gendebug)
            printf("did not find %s in intrinsics table\n",
               root->parent->astnode.ident.name);
          fprintf (curfp, "%s%s", com_prefix, name);
        }
        else
        {
          if(gendebug)
            printf("found %s in intrinsics or array table\n",
               root->parent->astnode.ident.name);
          fprintf (curfp, "%s%s.val", com_prefix,name);
        }
      }
      else if(root->parent->nodetype == Typedec) {
        if(gendebug)
          printf("Emitting typedec name: %s\n", name);
        fprintf (curfp, "%s", name);
      }
      else if(root->parent->nodetype == ArrayDec) {
        fprintf (curfp, "%s%s.val", com_prefix, name);
      }
      else {
        if( (global_sub.name != NULL) && 
            !strcmp(global_sub.name, name))
          fprintf (curfp, " %d ", global_sub.val);
        else
          fprintf (curfp, "%s%s.val", com_prefix, name);
      }
    }
  }
  else 
  {
    /* 
     * if we reach this case, we are emitting an array, but there
     * is no index specified.  Normally, we would just emit the variable
     * name, but we must also check the parent nodetype.  If it is a
     * call to an external function, then we have to emit the variable
     * name followed by ",0" to signify that the offset into this array
     * is 0.   10/10/97  --Keith 
     */

    if(root->parent == NULL) 
    {
      fprintf(stderr,"name_emit(): NO PARENT!\n");
    } 
    else 
    {
      printf("CRAP here we are emitting a scalar: %s,",name);
      printf("The parent node is : %s\n",print_nodetype(root->parent));

      if((root->parent->nodetype == Call) && 
         (type_lookup(cur_external_table, root->parent->astnode.ident.name) != NULL))
      {
        if( type_lookup(cur_args_table,root->astnode.ident.name) != NULL )
          fprintf (curfp, "%s,_%s_offset", name,
             name);
        else
          fprintf (curfp, "%s,0", name);
      }
      else 
        fprintf (curfp, "%s", name);
    }
  }
}

/*  
 *  This function translates calls to external functions.  First, 
 *  check whether we are translating a call to LSAME or LSAMEN.
 *  LSAME is from BLAS and LSAMEN is from LAPACK.  Instead of translating
 *  the actual files lsame.f and lsamen.f to java, we just translate
 *  the calls to equivalent java method calls (String.equalsIgnoreCase
 *  and String.regionMatches respectively).   If we're not translating
 *  a call to LSAME or LSAMEN, use the function call_emit().  --Keith 
 *
 *  changing equalsIgnoreCase() to a character comparison since the
 *  LAPACK routine only compares the first character.  12/4/97 --Keith
 *
 */

int
external_emit(AST *root)
{
  extern METHODTAB intrinsic_toks[];
  char *tempname, *javaname;
  AST *temp;

  tempname = strdup(root->astnode.ident.name);
  uppercase(tempname);

  javaname = (char *) methodscan (intrinsic_toks, tempname);

  /*  
   *  This block of code is only called if the identifier
   *  absolutely does not have an entry in any table,
   *  and corresponds to a method invocation of
   *  something in the blas or lapack packages.  
   */

  if (javaname == NULL)
  {
    if (root->astnode.ident.arraylist != NULL)
      call_emit (root);
    return;
  }

  if (root->astnode.ident.arraylist != NULL)
  {
    if (!strcmp (tempname, "LSAME"))
    {
      temp = root->astnode.ident.arraylist;

      printf("emitting a call to LSAME...first nodetype = %s, next = %s\n",
          print_nodetype(temp), print_nodetype(temp->nextstmt));

      fprintf(curfp, "(");
      expr_emit(temp);
      fprintf(curfp, ".charAt(0) == ");
      expr_emit(temp->nextstmt);
      fprintf(curfp, ".charAt(0))");

      return;
    }
    else if (!strcmp (tempname, "LSAMEN"))
    {
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

      expr_emit(temp->nextstmt);
      fprintf (curfp, "%s(true,0,", javaname);
      expr_emit (temp->nextstmt->nextstmt);
      fprintf (curfp, ",0,");
      expr_emit (temp);
      fprintf (curfp, ")");
      return;
    }
  }
}

/*
 * This function generates calls to intrinsic functions.  Basically we just
 * map fortran intrinsics to equivalent functions in the core Java API.
 * It might be a good idea to write separate handlers for each intrinsic.
 * Many intrinsics can be handled with a generic handler, so we could have
 * a generic one-argument handler, a generic two-argument handler, etc.
 * Intrinsics that need more specialized handling, such as LOG10, would need
 * their own handler.  Because of the need for specialized handlers, the
 * commented-out loop below may not ever really work.
 */

int
intrinsic_emit(AST *root)
{
  extern METHODTAB intrinsic_toks[];
  AST *temp;
  char *tempname, *javaname;

  printf("entering intrinsic_emit\n");
  tempname = strdup(root->astnode.ident.name);
  uppercase(tempname);

  javaname = (char *)methodscan (intrinsic_toks, tempname);

  /* 
   * This goes into an infinite loop. I don't
   * know why.  Might be a result from parsing.
   * It will replace the entire lower block
   * when I get it to work.  
   */

#ifdef  KJGKJKJKKJH
  temp = root->astnode.ident.arraylist;
  assert(temp != NULL);
  fprintf (curfp, "%s(", javaname);

  for (temp; temp != NULL; temp->nextstmt)
  {
    printf("Yoikes\n");
    expr_emit (temp);
    if(temp->nextstmt)
      fprintf (curfp, ", ");
  }
  fprintf (curfp, ")");
  return;
  /*put end brace here when KJGKJKJKKJH defined*/ 
#endif	  

  if (!strcmp (tempname, "MAX"))
  {
    temp = root->astnode.ident.arraylist;
    fprintf (curfp, "%s(", javaname);
    expr_emit (temp);
    fprintf (curfp, ", ");
    expr_emit (temp->nextstmt);
    fprintf (curfp, ")");
    return;
  }

  if (!strcmp (tempname, "MIN"))
  {
    temp = root->astnode.ident.arraylist;
    fprintf (curfp, "%s(", javaname);
    expr_emit (temp);
    fprintf (curfp, ", ");
    expr_emit (temp->nextstmt);
    fprintf (curfp, ")");
    return;
  }

  if ( (!strcmp (tempname, "DSQRT")) ||
       (!strcmp (tempname, "SQRT"))  ||
       (!strcmp (tempname, "DABS"))  ||
       (!strcmp (tempname, "DBLE"))  ||
       (!strcmp (tempname, "LOG"))  ||
       (!strcmp (tempname, "SIN"))  ||
       (!strcmp (tempname, "COS"))  ||
       (!strcmp (tempname, "EXP"))  ||
       (!strcmp (tempname, "ABS")))
  {
    temp = root->astnode.ident.arraylist;
    fprintf (curfp, "%s(", javaname);
    expr_emit (temp);
    fprintf (curfp, ")");
    return;
  }

  if(!strcmp(tempname,"LOG10"))
  {
    temp = root->astnode.ident.arraylist;
    fprintf (curfp, "(%s(", javaname);
    expr_emit (temp);
    fprintf (curfp, ") / 2.30258509)");
    return;
  }

  if(!strcmp(tempname,"LEN"))
  {
    HASHNODE *ht;

    temp = root->astnode.ident.arraylist;

    if(temp != NULL)
      if( (ht=type_lookup(cur_type_table,temp->astnode.ident.name)) != NULL)
      {
        fprintf (curfp, " %d ", ht->variable->astnode.ident.len);
        printf("LEN(%s) = %d\n",temp->astnode.ident.name,
          ht->variable->astnode.ident.len);
      }
      else
      {
        fprintf (curfp, " 1 ");
        printf("LEN(%s) = 1\n");
      }
    return;
  }

  if (!strcmp (tempname, "MOD"))
  {
    temp = root->astnode.ident.arraylist;
    fprintf(curfp,"(");
    expr_emit(temp);
    fprintf(curfp,")%%(");
    expr_emit(temp->nextstmt);
    fprintf(curfp,") ");

    /*  
     *  this chunk of code will emit a call to MOD as a call to
     *  Math.IEEERemainder().  usually that is not appropriate
     *  since IEEERemainder returns double, whereas in FOTRAN,
     *  the expected type is int.   -- keith
     *
     * fprintf (curfp, "%s(", javaname);
     * expr_emit (temp);
     * fprintf (curfp, ", ");
     * expr_emit (temp->nextstmt);
     * fprintf (curfp, ")");
     */

    return;
  }

  if (!strcmp (tempname, "ICHAR"))
  {
    temp = root->astnode.ident.arraylist;
    fprintf (curfp, "%s(", javaname);
    expr_emit (temp);
    fprintf (curfp, ".charAt(0))");
    return;
  }

  if (!strcmp (tempname, "CHAR"))
  {
    temp = root->astnode.ident.arraylist;
    fprintf (curfp, "%s(", javaname);
    expr_emit (temp);
    fprintf (curfp, ")");
    return;
  }

  if((!strcmp (tempname, "INT")) ||
     (!strcmp (tempname, "REAL")))
  {
    temp = root->astnode.ident.arraylist;
    fprintf (curfp, "%s(", javaname);
    expr_emit (temp);
    fprintf (curfp, ")");
    return;
  }

  if(!strcmp(tempname,"NINT"))
  {
    temp = root->astnode.ident.arraylist;

    fprintf(curfp,"%s((",javaname);
    expr_emit(temp);
    fprintf(curfp,") >= 0 ? (");
    expr_emit(temp);
    fprintf(curfp,") + .5 : (");
    expr_emit(temp);
    fprintf(curfp,") - .5)");
    return;
  }

  if(!strcmp(tempname,"SIGN"))
  {
    temp = root->astnode.ident.arraylist;

    fprintf(curfp,"((");
    expr_emit(temp->nextstmt);
    fprintf(curfp,") >= 0 ? Math.abs(");
    expr_emit(temp);
    fprintf(curfp,") : -Math.abs(");
    expr_emit(temp);
    fprintf(curfp,"))");
    return;
  }
  printf("leaving intrinsic_emit\n");
}

/*
 * This function tries to guess the type of a value contained
 * in a string.  If we find a '.' in the string, we guess that
 * it's a floating point number.  If the string contains 'true'
 * or 'false', we guess that it's a boolean value.  Otherwise
 * we guess that it's an integer value.  Not very sophisticated,
 * but it works most of the time.
 */

enum returntype
get_type(char *num)
{
  int isfloat = 0;
  int isbool = 0;
  int idx;

  for(idx = 0;idx < strlen(num);idx++) 
    if(num[idx] == '.') {
      return Double;
      break;
    }

  if( !strcmp(num,"false") || !strcmp(num,"true"))
    return Logical;

  return Integer;
}

/* 
 * All this will do is emit a number if there is one.
 * Needs to be extended for arrays, etc.  Consider using
 * a switch/case structure for this.
 */

int
expr_emit (AST * root)
{
  extern METHODTAB intrinsic_toks[];
  char *tempname;

  if(root == NULL)
  {
    fprintf(stderr,"Warning: NULL root in expr_emit\n");
    return 0;
  }

  switch (root->nodetype)
  {
    case Identifier:
      name_emit (root);
      break;
    case Expression:
      if (root->astnode.expression.parens)
        fprintf (curfp, "(");

      if (root->astnode.expression.lhs != NULL)
        expr_emit (root->astnode.expression.lhs);

      expr_emit (root->astnode.expression.rhs);

      if (root->astnode.expression.parens)
        fprintf (curfp, ")");
      break;
    case Power:
      /* hack alert: */
      if((root->parent != NULL) && (root->parent->nodetype == ArrayDec))
        fprintf (curfp, "(int) Math.pow(");
      else
        fprintf (curfp, "Math.pow(");
      expr_emit (root->astnode.expression.lhs);
      fprintf (curfp, ", ");
      expr_emit (root->astnode.expression.rhs);
      fprintf (curfp, ")");
      break;
    case Binaryop:
      expr_emit (root->astnode.expression.lhs);
      fprintf (curfp, "%c", root->astnode.expression.optype);
      expr_emit (root->astnode.expression.rhs);
      break;
    case Unaryop:
      fprintf (curfp, "%c", root->astnode.expression.minus);
      expr_emit (root->astnode.expression.rhs);
      break;
    case Constant:

     /* 
      * here we need to determine if this is a parameter to a function
      * or subroutine.  if so, and we are using wrappers, then we need
      * to create a temporary wrapper and pass that in instead of the
      * constant.   10/9/97  -- Keith 
      */

     if(root->parent != NULL)
     {
       tempname = strdup(root->parent->astnode.ident.name);
       uppercase(tempname);
     }

     if( (root->parent != NULL) &&
         (root->parent->nodetype == Call) &&
         (type_lookup(cur_array_table,root->parent->astnode.ident.name) == NULL) &&
         (methodscan(intrinsic_toks, tempname) == NULL))
     {
       if(root->token == STRING)
         fprintf (curfp, "new StringW(\"%s\")", root->astnode.ident.name);
       else {
         fprintf (curfp, "new %s(%s)", 
           wrapper_returns[get_type(root->astnode.constant.number)],
           root->astnode.constant.number);
         }
       }
       else 
       {
         if(root->token == STRING)
           fprintf (curfp, "\"%s\"", root->astnode.ident.name);
         else
           fprintf (curfp, "%s", root->astnode.constant.number);
       }
       break;
    case Logicalop:
      /* 
       * Change all of this code to switch on the tokens.
       * The parser code will have to store the NOT token.
       */
      if (root->astnode.expression.lhs != NULL)
        expr_emit (root->astnode.expression.lhs);
      else
        fprintf (curfp, "!");
      if (root->token == AND)
        fprintf (curfp, " && ");
      if (root->token == OR)
        fprintf (curfp, " || ");
      expr_emit (root->astnode.expression.rhs);
      break;
    case Relationalop:

      switch (root->token)
      {
        case rel_eq:

if(root->astnode.expression.lhs->nodetype == Identifier)
  printf("##@@ lhs ident %s has type %s\n", 
    root->astnode.expression.lhs->astnode.ident.name,
    returnstring[root->astnode.expression.lhs->vartype]);

if(root->astnode.expression.rhs->nodetype == Identifier)
  printf("##@@ rhs ident %s has type %s\n", 
    root->astnode.expression.rhs->astnode.ident.name,
    returnstring[root->astnode.expression.rhs->vartype]);

          if(((root->astnode.expression.lhs->vartype == String) ||
              (root->astnode.expression.lhs->vartype == Character)) &&
             ((root->astnode.expression.rhs->vartype == String) ||
              (root->astnode.expression.rhs->vartype == Character)))
          {
            expr_emit (root->astnode.expression.lhs);
            fprintf(curfp,".equalsIgnoreCase(");
            expr_emit (root->astnode.expression.rhs);
            fprintf(curfp,")");
          }
          else
          {
            expr_emit (root->astnode.expression.lhs);
            fprintf (curfp, " == ");
            expr_emit (root->astnode.expression.rhs);
          }
          break;
        case rel_ne:
          if(((root->astnode.expression.lhs->vartype == String) ||
              (root->astnode.expression.lhs->vartype == Character)) &&
             ((root->astnode.expression.rhs->vartype == String) ||
              (root->astnode.expression.rhs->vartype == Character)))
          {
            fprintf(curfp,"!");
            expr_emit (root->astnode.expression.lhs);
            fprintf(curfp,".equalsIgnoreCase(");
            expr_emit (root->astnode.expression.rhs);
            fprintf(curfp,")");
          }
          else
          {
            expr_emit (root->astnode.expression.lhs);
            fprintf (curfp, " != ");
            expr_emit (root->astnode.expression.rhs);
          }
          break;
        case rel_lt:
          expr_emit (root->astnode.expression.lhs);
          fprintf (curfp, " < ");
          expr_emit (root->astnode.expression.rhs);
          break;
        case rel_le:
          expr_emit (root->astnode.expression.lhs);
          fprintf (curfp, " <= ");
          expr_emit (root->astnode.expression.rhs);
          break;
        case rel_gt:
          expr_emit (root->astnode.expression.lhs);
          fprintf (curfp, " > ");
          expr_emit (root->astnode.expression.rhs);
          break;
        case rel_ge:
          expr_emit (root->astnode.expression.lhs);
          fprintf (curfp, " >= ");
          expr_emit (root->astnode.expression.rhs);
          break;
      }
      break;
    case Substring:
      fprintf(curfp,"%s.val.substring((",root->astnode.ident.name);
      expr_emit(root->astnode.ident.arraylist);
      fprintf(curfp,")-1,");
      expr_emit(root->astnode.ident.arraylist->nextstmt);
      fprintf(curfp,")");
      break;
    default:
      fprintf(stderr,"Warning: Unknown nodetype in expr_emit(): %s\n",
        print_nodetype(root));
  }
}

/*
 * This function attempts to open the output file.
 *
 */

int
open_output_file(AST *root)
{
  char * filename;
  char * classname;
  
  filename = (char *)
     malloc(strlen(root->astnode.source.name->astnode.ident.name) + 10);
  if(filename == NULL)
  {
    perror("Unsuccessful malloc()");
    exit(1);
  }

  strcpy(filename,lowercase(strdup(root->astnode.source.name->astnode.ident.name)));
  *filename = toupper (*filename);
  classname = strdup(filename);
  cur_filename = classname;
  strcat(filename,".java");

  if(gendebug)
    printf("filename is %s\n",filename);

  if((javafp = fopen(filename,"w"))==NULL) {
    fprintf(stderr,"Cannot open output file '%s'.\n",filename);
    perror("Reason");
    exit(1);
  }

  javaheader(javafp,classname);  /* print header to output file */
}

/*
 * This function generates the method header for the current 
 * function or subroutine.
 */

int
constructor (AST * root)
{
    enum returntype returns;
    extern char *returnstring[];
    AST *tempnode, *temp;
    char *tempstring;
    HASHNODE *hashtemp;

    /* 
     * In fortran, functions return a value implicitly
     * associated with there own name. In java, we declare a
     * variable in the constructor that shadows the class
     * (function) name and returns the same type. 
     */

    if (root->nodetype == Function)
    {
      returns = root->astnode.source.returns;

      /* Test code.... */
      if ((returns == String) || (returns == Character))
      {
         print_string_initializer(root);
         fprintf(curfp, ";\n\n");
      }
      else
         fprintf (curfp, "static %s %s = new %s(%s);\n\n", 
           wrapper_returns[returns],
           root->astnode.source.name->astnode.ident.name,
           wrapper_returns[returns],
           init_vals[returns]);

      /* Define the constructor for the class. */
/*
 *    fprintf (curfp, "\npublic static %s %s (",
 *      returnstring[returns],
 *      lowercase(strdup(root->astnode.source.name->astnode.ident.name)));
 */

      fprintf (curfp, "\npublic static %s %s (",
        returnstring[returns],
        root->astnode.source.name->astnode.ident.name);
    }
    /* Else we have a subroutine, which returns void. */
    else if(root->nodetype == Subroutine)
    {
      fprintf (curfp, "\npublic static void %s (",
        root->astnode.source.name->astnode.ident.name);
    }
    else  /* Else we have a program, create a main() function */
    {
      fprintf (curfp, "\npublic static void main (String [] args");
    }

    /*
     *  Now traverse the list of constructor arguments for either
     *  functions or subroutines.   This is where I will
     *  have to check what the variable type is in the
     *  symbol table. 
     */

    tempnode = root->astnode.source.args;

    for (tempnode; tempnode != NULL; tempnode = tempnode->nextstmt)
      {
	  hashtemp = type_lookup (cur_type_table, tempnode->astnode.ident.name);
	  if (hashtemp == NULL)
          {
		fprintf (stderr,"Type table is screwed (codegen.c).\n");
		fprintf (stderr,"  (looked up: %s)\n", tempnode->astnode.ident.name);
		exit (-1);
          }

	  /* 
           * Since all of fortran is call-by-reference, we have to pass
	   * in the arguments as java Objects.  
           */

	  returns = hashtemp->type;
	  /* 
           * Check the numerical value returns.  It should not 
	   * exceed the value of the enum returntypes.  
           */
	  if (returns > 6)
	    {
		printf ("Bad return value, check types.\n");
	    }

	  if (hashtemp->variable->astnode.ident.arraylist == NULL)
	    tempstring = wrapper_returns[returns];
          else
	    tempstring = returnstring[returns];

	  /* 
           * I haven't yet decided how the pass-by-reference
	   * pass-by-value problem will be resolved.  It may
	   * not be an issue at all in a java calling java
	   * situation.  The next line, when used, will list
	   * all the arguments to the method as references.
	   * This means that primitives such as int and
	   * double are wrapped as objects.
           *
	   * *tempstring = toupper (*tempstring);
           *
           * To save storage space, I'm wrapping the primitives with
           * special-purpose wrappers (intW, doubleW, etc.).
           * 10/8/97  --Keith 
           */

	  fprintf (curfp, "%s ", tempstring);

	  if (hashtemp->variable->astnode.ident.arraylist == NULL)
	      fprintf (curfp, "%s", tempnode->astnode.ident.name);
	  else
	      /* Declare as array variables.  */
	    {
               char temp2[100];
#if ONED
	fprintf (curfp, "[]");
#endif      
#if TWOD
		temp = hashtemp->variable->astnode.ident.arraylist;
		for (temp; temp != NULL; temp = temp->nextstmt)
		  {
		      fprintf (curfp, "[]");
		  }		/* Close for() loop. */
#endif
		fprintf (curfp, " %s", tempnode->astnode.ident.name);

                /* 
                 * for arrays, add a parameter representing the base 
                 * index.   -- Keith 
                 */
                strcpy( temp2, "_");
                strcat( temp2, tempnode->astnode.ident.name);
                strcat( temp2, "_offset");
                fprintf(curfp, ", int %s",temp2);
	    }
	  /* Don't emit a comma on the last iteration. */
	  if (tempnode->nextstmt)
	      fprintf (curfp, ",\n");
      }

    fprintf (curfp, ")  {\n\n");
    if(type_lookup(cur_external_table,"etime") != NULL)
      fprintf(curfp, "  Etime.etime();\n");
}				/*  Close  constructor(). */

/*
 * This function generates code to implement the fortran DO loop.
 * naturally, we use Java's 'for' loop for this purpose.
 *
 * We also keep track of the nesting of for loops so that if we
 * encounter a goto statement within a loop, we can generate a
 * java 'break' or 'continue' statement.
 */

int
forloop_emit (AST * root)
{
  char *indexname;
  int *tmp_int;

  tmp_int = (int*)malloc(sizeof(int));

  if(!tmp_int) { perror("malloc"); exit(1); }

  *tmp_int = root->astnode.forloop.Continue->astnode.label.number;

  /* push this do loop's number on the stack */
  dl_insert_b(doloop, tmp_int);

   /*  
    *  Some point I will need to test whether this is really a name
    *  because it will crash if not.  
    */
  indexname = 
      root->astnode.forloop.start->astnode.assignment.lhs->astnode.ident.name;

  fprintf(curfp, "{\n");

  if(root->astnode.forloop.incr != NULL)
  {
    fprintf(curfp,"int _%s_inc = ", indexname);
    expr_emit (root->astnode.forloop.incr);
    fprintf(curfp, ";\n");
  }

   /* print out a label for this for loop */

  fprintf(curfp, "forloop%d:\n",
     root->astnode.forloop.Continue->astnode.label.number);
   
   /* This block writes out the loop parameters.  */

  fprintf (curfp, "for (");

  assign_emit (root->astnode.forloop.start);

  fprintf(curfp, "; ");

  if(root->astnode.forloop.incr == NULL)
  {
    name_emit(root->astnode.forloop.start->astnode.assignment.lhs);

    fprintf(curfp, " <= ");
    expr_emit (root->astnode.forloop.stop);

    fprintf (curfp, "; ");

    name_emit(root->astnode.forloop.start->astnode.assignment.lhs);

    fprintf (curfp, "++");
  }
  else
  {
    fprintf(curfp,"(_%s_inc < 0) ? ",indexname);
    name_emit(root->astnode.forloop.start->astnode.assignment.lhs);
    fprintf(curfp," >= ");
    expr_emit (root->astnode.forloop.stop);
    fprintf(curfp," : ");
    name_emit(root->astnode.forloop.start->astnode.assignment.lhs);
    fprintf(curfp," <= ",indexname);
    expr_emit (root->astnode.forloop.stop);
    fprintf (curfp, "; ");
    
    name_emit(root->astnode.forloop.start->astnode.assignment.lhs);
    fprintf (curfp, " += _%s_inc",indexname);
  }

  fprintf (curfp, ") {\n");
   /*  Done with loop parameters.  */

   /* Statements in the body of the for() loop. */
  emit (root->astnode.forloop.stmts);

   /* 
    * finally pop this loop's label number off the stack and 
    * emit the label (for experimental goto resolution) 
    */
   
  fprintf(curfp,"Dummy.label(\"%s\",%d);\n",cur_filename,
     *((int *) dl_pop(doloop)));

  fprintf (curfp, "}              //  Close for() loop. \n");
  fprintf(curfp, "}\n");
}

/* 
 * Since gotos aren't supported by java, we can't just emit a goto here.
 * labeled continues and breaks are supported in java, but only in certain 
 * cases.  so, if we are within a loop, and we are trying to goto the CONTINUE 
 * statement of an enclosing loop, then we can just emit a labeled continue 
 * statement.  --Keith       
 *
 * I think I fixed a previous problem emitting gotos within nested 
 * simulated while loops by keeping track of all if statements rather than
 * just the ones identified as while statements.   10/3/97 -- Keith
 */

goto_emit (AST * root)
{
  if( (!dl_empty(doloop)) && dl_int_search(doloop, root->astnode.go_to.label) )
  {
     /*
      *  we are inside a do loop and we are looking at a goto
      *  statement to the 'continue' statement of an enclosing loop.
      *  what we want to do here is just emit a 'labeled continue' 
      */ 

    fprintf(curfp,"continue forloop%d;\n",root->astnode.go_to.label);
  }
  else if((!dl_empty(while_list)) && 
     (dl_int_examine(while_list) == root->astnode.go_to.label ))
  {
       /* 
        *  we are inside a simulated while loop and we are looking at 
        *  a goto statement to the 'beginning' statement of the most
        *  enclosing if statment.  Since we are translating this to an 
        *  actual while loop, we ignore this goto statement 
        */

    fprintf(curfp,"// goto %d (end while)\n",root->astnode.go_to.label);
  }
  else 
  {
       /*  
        *  otherwise, not quite sure what to do with this one, so
        *  we'll just emit a dummy goto 
        */

    fprintf(curfp,"Dummy.go_to(\"%s\",%d);\n",cur_filename,
        root->astnode.go_to.label);
  }
}

/*
 * This function generates code to implement fortran's computed
 * GOTO statement.   we simply use a series of if-else statements
 * to implement the computed goto.
 */

int
computed_goto_emit (AST *root)
{
  AST *temp;
  int count = 1;

  for(temp = root->astnode.computed_goto.intlist;temp!=NULL;temp=temp->nextstmt)
  {
    if(temp != root->astnode.computed_goto.intlist)
      fprintf(curfp,"else ");
    fprintf(curfp,"if (");
    expr_emit(root->astnode.computed_goto.name);
    fprintf(curfp," == %d) \n", count);
    fprintf(curfp,"  Dummy.go_to(\"%s\",%s);\n", cur_filename, 
      temp->astnode.constant.number);
    count++;
  }
}

/*
 * This function generates code for IF statements.  Java and Fortran have
 * pretty similar if statements, so this one is simple.
 */

logicalif_emit (AST * root)
{
  fprintf (curfp, "if (");
  if (root->astnode.logicalif.conds != NULL)
    expr_emit (root->astnode.logicalif.conds);
  fprintf (curfp, ")  \n    ");
  emit (root->astnode.logicalif.stmts);
}

/*
 * This function generates labels.  We generate both a java label
 * and a call to the Dummy.label() method for goto translation.
 */

int
label_emit (AST * root)
{
  if (root->astnode.label.stmt != NULL) {
    if (root->astnode.label.stmt->nodetype != Format) {
      fprintf(curfp,"{\n");
      fprintf (curfp, "label%d:\n   ", root->astnode.label.number);
      fprintf(curfp,"Dummy.label(\"%s\",%d);\n",cur_filename,
        root->astnode.label.number);
      emit (root->astnode.label.stmt);
      fprintf(curfp,"}\n");
    }
  } 
  else {
    fprintf(curfp,"{\n");
    fprintf (curfp, "label%d:\n   ", root->astnode.label.number);
    fprintf(curfp,"Dummy.label(\"%s\",%d);\n",cur_filename,
      root->astnode.label.number);
    fprintf(curfp,"}\n");
  }
}

/*
 * This function handles WRITE statements.  It is FAR from complete,
 * but it is usually good enough to test the numerical routines.
 */

int
write_emit (AST * root)
{
  HASHNODE *hnode;
  AST *temp;
  AST *nodeptr;
  char tmp[100];

  fprintf (curfp, "System.out.println(");

  /* 
   * The following is a cheesy workaround to handle the following type of
   * statement:
   *         write(*, FMT = '( '' Matrix types:'' )' ) 
   * eventually, we should handle any kind of format spec within the
   * quotes, but for now we just treat the whole thing as a string.  
   * 12/4/97 --Keith
   */

  if(root->astnode.io_stmt.fmt_list != NULL)
  {
    fprintf(curfp, "\"%s\"", root->astnode.io_stmt.fmt_list->astnode.ident.name);
    if(root->astnode.io_stmt.arg_list != NULL)
      fprintf(curfp, " + ");
  }

  sprintf(tmp,"%d", root->astnode.io_stmt.format_num);
  if(gendebug)
    printf("***Looking for format statement number: %s\n",tmp);

  /* if there's formatting information for this write statement, use it */

  if( (hnode = format_lookup(cur_format_table,tmp)) != NULL ) {
    if(gendebug)
      printf("****FOUND****\n");

    nodeptr = root->astnode.io_stmt.arg_list; 

    format_list_emit(hnode->variable->astnode.label.stmt,&nodeptr);
  }
  else {
    if(gendebug)
      printf("****NOT FOUND****\n");

    for( temp = root->astnode.io_stmt.arg_list; 
      temp != NULL; 
      temp = temp->nextstmt) 
    {
      fprintf(curfp,"(");
      expr_emit (temp);
      fprintf(curfp,")");
      if(temp->nextstmt != NULL)
        fprintf (curfp, " + \"\" + ");
    }
  }

  fprintf (curfp, ");\n");
}

/*
 * This function loops through each format item and generates the
 * code to print the appropriate value(s).
 */

format_list_emit(AST *node, AST **nptr)
{
  AST *temp = node;

  while(temp != NULL)
    temp = format_item_emit(temp,nptr);
}

/*
 * This function generates the code to print item(s) from the 
 * format list.
 */

AST *
format_item_emit(AST *temp, AST **nodeptr)
{
  int i;

  switch(temp->token) {
    case EDIT_DESC:
    case NAME:
      if(gendebug)
        printf("NAme/EDIT_DESC\n");
      format_name_emit(*nodeptr);
      if(*nodeptr == NULL)
        fprintf(stderr,"ERROR, nodeptr now null (Name)\n");
      else {
        if(gendebug)
          printf("** Advancing nodeptr ** \n");
            *nodeptr = (*nodeptr)->nextstmt;
      }
      if(temp->nextstmt != NULL)
        fprintf(curfp," + ");
      return(temp->nextstmt);
      break;
    case STRING:
      if(gendebug)
        printf("STring: %s\n",temp->astnode.ident.name);
      fprintf(curfp,"\"%s\" ",temp->astnode.ident.name);
      if(temp->nextstmt != NULL)
        fprintf(curfp," + ");
      return(temp->nextstmt);
      break;
    case REPEAT:
      if(gendebug)
        printf("Repeat %d\n",temp->astnode.label.number);
      for(i=0;i<temp->astnode.label.number;i++) {
        format_list_emit(temp->astnode.label.stmt,nodeptr);

        if((i < temp->astnode.label.number -1) || 
          ((i == temp->astnode.label.number -1) && (temp->nextstmt != NULL)))
              fprintf(curfp," + ");
      }
      return(temp->nextstmt);
      break;
    case INTEGER:
      if(gendebug)
        printf("INteger %d\n",atoi(temp->astnode.constant.number));

      if(temp->nextstmt != NULL) {
        if(temp->nextstmt->token != REPEAT) {
          if(temp->nextstmt->astnode.ident.name[0] == 'X') {
            fprintf(curfp,"\"");
            for(i=0;i< atoi(temp->astnode.constant.number);i++)
              fprintf(curfp," ");
            fprintf(curfp,"\"");
            if(temp->nextstmt->nextstmt != NULL)
              fprintf(curfp," + ");
            temp=temp->nextstmt;  /* consume edit desc */
          }
          else if(temp->nextstmt->astnode.ident.name[0] == 'P') {
            temp=temp->nextstmt;  /* consume edit desc */
          }
        }
      }
      else {
        fprintf(stderr,"Bad format spec!\n");
        fprintf(curfp," );\n");
        return(temp->nextstmt);
      }

      return(temp->nextstmt);
      break;
    case CM:
      if(gendebug)
        printf("Comma\n");
      return(temp->nextstmt);
      break;
    case DIV:
      if(gendebug)
        printf("Div\n");
      fprintf(curfp,"\"\\n\" ");
      if(temp->nextstmt != NULL)
        fprintf(curfp," + ");
      return(temp->nextstmt);
      break;
    case CAT:
      if(gendebug)
        printf("two divs\n");
      fprintf(curfp,"\"\\n\\n\" ");
      if(temp->nextstmt != NULL)
        fprintf(curfp," + ");
      return(temp->nextstmt);
      break;
    default:
      fprintf(stderr,"format_item_emit: Unknown token!!! %d (%s) - ",
         temp->token, tok2str(temp->token));
      if(gendebug)
        printf("this node type %s\n",print_nodetype(temp));
      return(temp->nextstmt);
      break;
  }
}

/*
 * This function generates the code to print a Name from the 
 * format list.
 */

void
format_name_emit(AST *node)
{
  if(node == NULL) {
    printf("*** BAD FORMATTING\n");
    fprintf(stderr,"Bad formatting!\n");
    fprintf(curfp,"\" NULL \"");
  }
  else {
      /* 
       * in the write statement is an array, with no index specified.
       * so we will keep grabbing data from the array until the end
       * of the format specification 
       */

/*  gotta get this part finished someday   10/3/97 -- Keith
     ... still not written - 
            relatively low on the priority list... 12/8/97 -- Keith

    if( (node->token == NAME) && 
        (type_lookup(cur_array_table, root->astnode.ident.name) != NULL) &&
        (root->astnode.ident.arraylist == NULL) )
    {

    
    }
    else
*/

  fprintf(curfp,"(");
      expr_emit(node);
  fprintf(curfp,")");
  }
  fprintf(curfp," + \" \" ");
}

/*
 * This function generates the code which implements fortran's
 * block if.  This could also be a simulated while loop, which
 * is why we push this loop's number on the while_list.  This
 * way we can generate a java 'while' loop instead of the
 * simulated while loop using gotos.
 */

int
blockif_emit (AST * root)
{
  AST *prev = root->prevstmt;
  AST *temp;
  int *tmp_int;

  tmp_int = (int*)malloc(sizeof(int));

  if(!tmp_int) { perror("malloc"); exit(1); }

  /* if the previous node was a label, this could be a simulated
   * while loop.
   */
  if(prev != NULL)
    if(prev->nodetype == Label)
    {
      *tmp_int = root->prevstmt->astnode.label.number;

      /* push this while loop's number on the stack */
  
      dl_insert_b(while_list, tmp_int);

      if(prev->astnode.label.stmt == NULL)
        if((root->astnode.blockif.elseifstmts == NULL) &&
           (root->astnode.blockif.elsestmts == NULL))
        {
           /* it appears that we are looking at a simulated while loop.
            * bypass all the statements in the body of this if block 
            * and look at the last one.  if it is a goto and the
            * target is the label of the current if statement, then
            * we generate a Java while loop.  otherwise, we generate
            * an if statement.
            */
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

      /* pop this while loop's label number off the stack */
      dl_pop(while_list);
    }

  fprintf (curfp, "if (");
  if (root->astnode.blockif.conds != NULL)
    expr_emit (root->astnode.blockif.conds);

  fprintf (curfp, ")  {\n    ");
  emit (root->astnode.blockif.stmts);
  fprintf (curfp, "}              // Close if()\n");

  if (root->astnode.blockif.elseifstmts != NULL)
    emit (root->astnode.blockif.elseifstmts);

  if (root->astnode.blockif.elsestmts != NULL)
    emit (root->astnode.blockif.elsestmts);
}

/* 
 *   while_emit() is called when an if statement has been identified
 *   as a simulated while loop, e.g.:
 *
 *     10 continue
 *        if(x < 10) then
 *           do something
 *           x = x+1
 *        goto 10
 *
 *   this can be translated into java as:
 *
 *     while(x<10) {
 *       do something
 *       x = x+1
 *     }
 *
 *   that just gives us one less goto statement to worry about.  --Keith
 */

void 
while_emit(AST *root)
{

  fprintf(curfp, "while (");
  if (root->astnode.blockif.conds != NULL)
    expr_emit (root->astnode.blockif.conds);
  fprintf (curfp, ")  {\n    ");
  emit (root->astnode.blockif.stmts);
  fprintf (curfp, "}              // Close if()\n");

}

/* 
 * This function generates the code for the fortran 'else if'
 * construct.
 */

void
elseif_emit (AST * root)
{
    fprintf (curfp, "else if (");
    if (root->astnode.blockif.conds != NULL)
	expr_emit (root->astnode.blockif.conds);
    fprintf (curfp, ")  {\n    ");
    emit (root->astnode.blockif.stmts);
    fprintf (curfp, "}              // Close else if()\n");
}

/* 
 * This function generates the code for the fortran 'else'
 * construct.
 */

void
else_emit (AST * root)
{
    fprintf (curfp, "else  {\n  ");
    emit (root->astnode.blockif.stmts);
    fprintf (curfp, "}              //  Close else.\n");
}

/* 
 *  This procedure implements Lapack and Blas type methods.
 *  They are translated to static method invocations.
 *  This is not a portable solution, it is specific to
 *  the Blas and Lapack. 
 */

int
call_emit (AST * root)
{
  AST *temp;
  char *tempname;
  HASHNODE *hashtemp;
  HASHNODE *ht;
  HASHNODE *ht2;

  assert (root != NULL);

  /* shouldn't be necessary to lowercase the name
   *   lowercase (root->astnode.ident.name);
   */

  tempname = strdup (root->astnode.ident.name);
  *tempname = toupper (*tempname);

  /* first analyze this function call to determine if we need to generate
   * an 'adapter' which will simulate passing array elements by reference.
   */

  if( needs_adapter(root) )
  {
    printf("wow, guess we need an adapter for %s.\n", root->astnode.ident.name);
    insert_adapter(root);

    /* Assume all methods that are invoked are static.  */
    fprintf (curfp, "%s_adapter", root->astnode.ident.name);
  }
  else
    fprintf (curfp, "%s.%s", tempname, root->astnode.ident.name);

  if((root->astnode.ident.arraylist->nodetype == EmptyArgList) ||
     (root->astnode.ident.arraylist == NULL))
  {
    if(root->nodetype == Call)
      fprintf (curfp, "();\n");
    else
      fprintf (curfp, "()");
    return;
  }

  fprintf (curfp, "(");

  /* look up the function name so that we may compare the parameters */

  printf("Looking up function name %s, ", root->astnode.ident.name);

  if((hashtemp=type_lookup(function_table, root->astnode.ident.name)) != NULL)
  {
    AST *t2;

    printf("Found!!\n");

    temp = root->astnode.ident.arraylist;
    t2=hashtemp->variable->astnode.source.args;

    for( ; temp != NULL; temp = temp->nextstmt)
    {
         /* 
          * if the arg is an identifier  AND
          *    it looks like an array access AND
          *    it is in the array table
          */
       if((temp->nodetype == Identifier) && (temp->astnode.ident.arraylist != NULL)
          && (ht=type_lookup(cur_array_table, temp->astnode.ident.name)) )
       {
         ht2 = type_lookup(cur_args_table, temp->astnode.ident.name);

         printf("CAlling func-array_emit\n");

         if(t2->astnode.ident.arraylist)     /* it is expecting an array */
         {
           fprintf(curfp,"%s",temp->astnode.ident.name);
           func_array_emit(temp->astnode.ident.arraylist,ht,
              temp->astnode.ident.name, ht2!=NULL, TRUE);
         }
         else                                /* it is not expecting an array */
         {
           /* In this case we are passing the array element to the
            * adapter, so we dont wrap it in an object.
            */

/* fprintf(curfp,"new %s(", wrapper_returns[t2->vartype]); */

           fprintf(curfp,"%s",temp->astnode.ident.name);

           func_array_emit(temp->astnode.ident.arraylist,ht,
              temp->astnode.ident.name, ht2!=NULL, TRUE);
/*
           func_array_emit(temp->astnode.ident.arraylist,ht,
              temp->astnode.ident.name, ht2!=NULL, FALSE);
*/

/* fprintf(curfp,")"); */
         }
       }
         /* 
          * else if the arg is an identifier AND
          *      it does not look like an array access AND
          *      it is in the array table
          */
       else if((temp->nodetype == Identifier) &&
               (temp->astnode.ident.arraylist == NULL) && 
               type_lookup(cur_array_table, temp->astnode.ident.name) )
       {
         if(t2->astnode.ident.arraylist)     /* it is expecting an array */
         {
           printf("expecting array\n");
           expr_emit(temp);
         }
         else
         {
           printf("NOT expecting array\n");
           fprintf(curfp,"new %s(", wrapper_returns[t2->vartype]);
           fprintf(curfp,"%s[0]", temp->astnode.ident.name);
           fprintf(curfp,")");
         }
       }
       else if(
         ((temp->nodetype == Identifier) &&
          (temp->astnode.ident.arraylist == NULL) )
          || (temp->nodetype == Constant) )
       {
         expr_emit(temp);
       }
       else if(temp->nodetype == EmptyArgList)
       {
          ;  /* do nothing */
       }
         /* 
          * Otherwise, use wrappers.
          */
       else 
       {
         fprintf(curfp,"new %s(", wrapper_returns[t2->vartype]);

         expr_emit(temp);

         fprintf(curfp,")");
       }
       if(t2 != NULL)
         t2 = t2->nextstmt;
       if(temp->nextstmt != NULL)
         fprintf(curfp, ",");
    }
  }
  else
  {
    temp = root->astnode.ident.arraylist;

    for( ; temp != NULL; temp = temp->nextstmt)
    {
      if(((temp->nodetype == Identifier) && (temp->astnode.ident.arraylist == NULL)) ||
          (temp->nodetype == Constant))
      {
        expr_emit (temp);
      }
      else
      {
        fprintf(curfp,"new %s(", wrapper_returns[temp->vartype]);
        expr_emit (temp);
        fprintf(curfp,")");
      }

      if(temp->nextstmt != NULL)
        fprintf(curfp, ",");
    }
  }

  /*  
   *  Problem here, depends on who called this procedure.
   *  When this is used by the CALL keyword, it works as
   *  written.  When used to create an external function call,
   *  it adds an extra ; and \n to the output.  Might be
   *  able to fix this by checking the nodetype. 
   */

  if(root->nodetype == Call)
    fprintf (curfp, ");\n");
  else
    fprintf (curfp, ")");
}				/*  Close call_emit().  */

/*
 * This function compares the expressions in the function call with
 * the arguments of the function to find one specific case: attempting
 * to pass an array element to a function that expects a scalar.  If
 * we find such a case, we must generate an adapter that allows
 * pass by reference of the array element.  Returns 1 if this function
 * call needs an adapter.  If no adapter is needed or if we dont have
 * enough info to determine whether one is needed, this function 
 * returns 0.
 */

int
needs_adapter(AST *root)
{
  AST *temp;
  HASHNODE *hashtemp;

  /* first, check for a null parameter list.  if there are no parameters, 
   * we certainly wont need an adapter.
   */
  if((root->astnode.ident.arraylist->nodetype == EmptyArgList) ||
     (root->astnode.ident.arraylist == NULL))
    return 0;

  printf("in needs_adapter: Looking up function name %s, ", 
    root->astnode.ident.name);

  if((hashtemp=type_lookup(function_table, root->astnode.ident.name)) != NULL)
  {
    AST *t2;

    temp = root->astnode.ident.arraylist;
    t2=hashtemp->variable->astnode.source.args;

    for( ; temp != NULL; temp = temp->nextstmt)
    {
         /*
          * if the arg is an identifier  AND
          *    it is in the array table  AND
          *    the function is not expecting an array
          */
       if((temp->nodetype == Identifier) && 
           type_lookup(cur_array_table, temp->astnode.ident.name) &&
           !t2->astnode.ident.arraylist)
              return 1;

       if(t2 != NULL)
         t2 = t2->nextstmt;
    }
  }

  return 0;
}

/* 
 * This function handles code generation for specification statements.
 * Actually, there isn't a whole lot to do for spec statements.
 */

int
spec_emit (AST * root)
{
  AST *assigntemp;

  /* I am reaching every case in this switch.  */

  switch (root->astnode.typeunit.specification)
  {
    /* 
     * PARAMETER in fortran corresponds to a class
     * constant in java, that has to be declared
     * class wide outside of any method.  This is
     * currently not implemented, but the assignment
     * is made.  
     */
    case Parameter:

/*************************************************************
  now handling parameters as part of vardec_emit.  
   11/3/97 --Keith

      fprintf (curfp, "// Assignment from Fortran PARAMETER specification.\n");
      assigntemp = root->astnode.typeunit.declist;
      for (assigntemp; assigntemp; assigntemp = assigntemp->nextstmt)
      {
        if (gendebug)
          printf ("Parameter stmt.\n");
        assign_emit (assigntemp);
        fprintf (curfp, ";\n");
      }
*************************************************************/

      break;

     /*  
      * I am reaching these next two cases. Intrinsic, for
      * example handles stuff like Math.max, etc. 
      */
    case Intrinsic:
      name_emit (root);
      break;
    case External:
      /*        printf ("External stmt.\n");   */
      break;
  }
}

/* 
 * This function generates the code for assignment statements.
 * If it looks like the lhs and rhs have different types, we
 * try to provide the appropriate cast, but in some cases the
 * resulting code may need to be modified slightly.
 */

int
assign_emit (AST * root)
{
  enum returntype ltype, rtype;

  ltype = root->astnode.assignment.lhs->vartype;
  rtype = root->astnode.assignment.rhs->vartype;

  printf("## ## codegen: ltype = %s (%d)\n",returnstring[ltype], ltype);
  printf("## ## codegen: rtype = %s (%d)\n",returnstring[rtype], rtype);

  /* handle lhs substring operations elsewhere */
  if(root->astnode.assignment.lhs->nodetype == Substring)
  {
    substring_assign_emit(root);
    return;
  }

  name_emit (root->astnode.assignment.lhs);
  fprintf (curfp, " = ");

  if(ltype != rtype)
  {
    /* lhs and rhs have different types */

    if((ltype != String) && (ltype != Logical) && (rtype == String))
    {
      fprintf(curfp,"(%s)(",returnstring[ltype]);
      expr_emit (root->astnode.assignment.rhs);
      fprintf(curfp,".charAt(0)");
      fprintf(curfp,")");
    }
    else if((ltype != String) && (ltype != Logical) && (rtype == Character))
    {
      fprintf(curfp,"(%s)(",returnstring[ltype]);
      expr_emit (root->astnode.assignment.rhs);
      fprintf(curfp,")");
    }
    else if( (ltype == Logical) && (rtype == String) )
    {
      fprintf(curfp,"(");
      expr_emit (root->astnode.assignment.rhs);
      fprintf(curfp,").charAt(0)");
      fprintf(curfp," == 0 ? false : true");
    }
    else if( (ltype == Logical) && (rtype == Character) )
    {
      fprintf(curfp,"(int)(");
      expr_emit (root->astnode.assignment.rhs);
      fprintf(curfp,") == 0 ? false : true");
    }
    else if( (ltype == Logical) && (rtype != String) )
    {
      expr_emit (root->astnode.assignment.rhs);
      fprintf(curfp," == 0 ? false : true");
    }
    else
    {
      fprintf(curfp,"(%s)(",returnstring[ltype]);
      expr_emit (root->astnode.assignment.rhs);
      fprintf(curfp,")");
    }
  }
  else   /* lhs and rhs have same types, everything is cool */
    expr_emit (root->astnode.assignment.rhs);
}

/*
 * This function handles situations in which the lhs of an
 * assignment statement is a substring operation.  For example:
 *   a(3:4) = 'hi'
 * We haven't figured out an elegant way to handle this in Java,
 * but we do handle it, as follows:
 *
 *  int E1, E2;
 *  E1 = 3;
 *  E2 = 4;
 *  a = new StringW(
 *        a.val.substring(0,E1-1) + 
 *        "hi".substring(0,E2-E1+1) + 
 *        a.val.substring(E2,a.val.length())
 *      );
 *
 * The resulting code looks pretty bad because we have to be
 * prepared to handle rhs strings that are too big to fit in
 * the lhs substring.
 *
 * luckily, java provides the String.substring() method, which
 * helps out a lot.
 */

int
substring_assign_emit(AST *root)
{
  AST *lhs = root->astnode.assignment.lhs;
  AST *rhs = root->astnode.assignment.rhs;
  char *lname = lhs->astnode.ident.name;

  printf("substring_assign_emit\n");

  fprintf(curfp,"{\n  int E1, E2;\n");
  fprintf(curfp,"  E1 = ");
  expr_emit(lhs->astnode.ident.arraylist);
  fprintf(curfp,";\n");

  fprintf(curfp,"  E2 = ");
  expr_emit(lhs->astnode.ident.arraylist->nextstmt);
  fprintf(curfp,";\n");

  fprintf(curfp,"%s = new StringW(",lname);
  fprintf(curfp,"%s.val.substring(0,E1-1) + ", lname);

  if(rhs->vartype == Character)
  {
    /* 
     * Java's Character class doesn't have a static toString
     * method, so we have to create a new character object first.
     */

    fprintf(curfp,"new Character(");
    expr_emit(rhs);
    fprintf(curfp,").toString().substring(0,E2-E1+1) + ");
  }
  else if(rhs->vartype == String)
  {
    expr_emit(rhs);
    fprintf(curfp,".substring(0,E2-E1+1) + ");
  }
  else
  {
    char *tempstring = strdup(returnstring[rhs->vartype]);

    *tempstring = toupper (*tempstring);

    fprintf(curfp,"%s.toString(", tempstring);
    expr_emit(rhs);
    fprintf(curfp,").substring(0,E2-E1+1) + ");
  }

  fprintf(curfp,"%s.val.substring(E2,%s.val.length())",lname,lname);

  fprintf(curfp,");\n");

  fprintf(curfp,"}\n");
}

/*
 * This function returns the last item in a dlist
 * of integers.
 */

int
dl_int_examine(Dlist l)
{
  return ( *( (int *) dl_val(dl_last(l)) ) );
}

/*
 * This function searches for a value in a dlist of
 * integers.  Returns TRUE if the value is found, FALSE
 * otherwise.
 */

int
dl_int_search(Dlist l, int val)
{
  Dlist p;

  dl_traverse(p,l)
    if( *((int *)p->val) == val )
      return TRUE;

  return FALSE;
}

/*
 * This function searches for a value in a dlist of
 * AST nodes.  Returns the node if it is found, NULL
 * otherwise.
 */

AST *
dl_name_search(Dlist l, char *name)
{
  Dlist p;

  dl_traverse(p,l)
    if( !strcmp(((AST *)p->val)->astnode.ident.name,name) )
      return p->val;

  return NULL;
}

int
insert_adapter(AST *node)
{
  HASHNODE *hashtemp;
  AST *ptr, *t2, *this_call, *other_call;
  int i, found = FALSE, diff = FALSE;
  int this_arg_is_arrayacc, other_arg_is_arrayacc;
  Dlist p;

  /* if there is not an adapter for this function call already in the list,
   * insert it now 
   */

  printf("** here we are in insert_adapter\n");
  printf("** \n");

  dl_traverse(p, adapter_list )
  {
    ptr = (AST *) dl_val(p);

    if( !strcmp(ptr->astnode.ident.name, node->astnode.ident.name) )
    {
      found = TRUE;

      /* this function call is already in the list.  now we must determine whether
       * the prototypes of the adapters would be the same.  If so, there's no need
       * to insert this node in the adapter list.  If the prototypes would be 
       * different, then we must insert this node.
       */
  
      printf("** %s is already in adapter_list.  now checking args.\n",
        node->astnode.ident.name);

      if((hashtemp=type_lookup(function_table, node->astnode.ident.name)) != NULL)
      {
        printf("** \n");
        printf("** found prototype.\n");
  
        this_call = node->astnode.ident.arraylist;
        other_call = ptr->astnode.ident.arraylist;
  
        t2 = hashtemp->variable->astnode.source.args;
  
        diff = FALSE;

        for(i=0 ; this_call != NULL; this_call = this_call->nextstmt, i++)
        {
          printf("** arg %d\n",i);
  
          if( other_call == NULL )
          {
            fprintf(stderr,"2:Function calls to %s in unit %s ", 
              node->astnode.ident.name, unit_name);
            fprintf(stderr,"don't have same number of params\n");
            return -1;
          }

          this_arg_is_arrayacc = (this_call->nodetype == Identifier) &&
                (this_call->astnode.ident.arraylist != NULL) &&
                type_lookup(cur_array_table, this_call->astnode.ident.name);
          printf("** this_arg_is_arrayacc = %d\n",this_arg_is_arrayacc);

          other_arg_is_arrayacc = (other_call->nodetype == Identifier) &&
                (other_call->astnode.ident.arraylist != NULL) &&
                type_lookup(cur_array_table, other_call->astnode.ident.name);
          printf("** other_arg_is_arrayacc = %d\n",other_arg_is_arrayacc);

          if( (! t2->astnode.ident.arraylist) &&
              (this_arg_is_arrayacc != other_arg_is_arrayacc ))
          {
            printf("** setting diff = TRUE\n");
            diff = TRUE;
          }
  
          printf("** blah\n");

          other_call = other_call->nextstmt;
        }
  
        if(!diff) {
          printf("** found an equivalent adapter.  no need to insert.\n");
          return 1;
        }
      }
      else {
        printf("** cant find prototype...returning.\n");  
  
                      /* cant find the prototype.  normally, I dont think */
        return -1;    /* this case will be reached.                       */
      }
    }
  }

/*
  if( ! found ) {
    printf("** Could not find %s in adapter_list... inserting now.\n",
      node->astnode.ident.name);
*/
    printf("** inserting '%s' into adapter_list now.\n",
      node->astnode.ident.name);
    dl_insert_b(adapter_list,node);
/*  } */
}

/*
 * This function generates any adapters necessary to
 * allow functions to pass array elements by reference.
 */

int
emit_adapters()
{
  HASHNODE *hashtemp;
  AST * arg, * temp;
  char *tempname;
  Dlist p;
  int i;


  dl_traverse(p,adapter_list)
  {
    hashtemp = type_lookup(function_table, ((AST *)dl_val(p))->astnode.ident.name);

    if(hashtemp == NULL) {
      fprintf(stderr,"Error: cant generate adapter for %s\n",
         ( (AST *) dl_val(p) )->astnode.ident.name);
      continue;
    }

    fprintf(curfp,"// adapter for %s\n", 
      ( (AST *) dl_val(p) )->astnode.ident.name);
  
    /* first generate the method header */

    if(hashtemp->variable->nodetype == Function)
      fprintf(curfp,"private static %s %s_adapter(", 
          returnstring[hashtemp->variable->astnode.source.returns],
          hashtemp->variable->astnode.source.name->astnode.ident.name);
    else
      fprintf(curfp,"private static void %s_adapter(", 
          hashtemp->variable->astnode.source.name->astnode.ident.name);

    temp = hashtemp->variable->astnode.source.args;
    arg = ((AST *)dl_val(p))->astnode.ident.arraylist;
    
    for(i = 0; arg != NULL ; arg = arg->nextstmt, i++)
    {
      if(temp == NULL) {
        fprintf(stderr,"Error: mismatch between call to %s and prototype\n",
           ( (AST *) dl_val(p) )->astnode.ident.name);
        break;
      }

      if( (temp->astnode.ident.arraylist) ||
          ( (arg->nodetype == Identifier) && (arg->astnode.ident.arraylist != NULL) &&
            type_lookup(cur_array_table,arg->astnode.ident.name)) )
        fprintf(curfp,"%s [] arg%d , int arg%d_offset ", returnstring[temp->vartype], i, i);
      else
        fprintf(curfp,"%s arg%d ", wrapper_returns[temp->vartype], i);

      if(temp != NULL)
        temp = temp->nextstmt;
      if(arg->nextstmt != NULL)
        fprintf(curfp,",");
    }

    fprintf(curfp,")\n{\n");

    temp = hashtemp->variable->astnode.source.args;
    arg = ((AST *)dl_val(p))->astnode.ident.arraylist;
    
    for(i = 0; arg != NULL ; arg = arg->nextstmt, i++)
    {
      if(temp == NULL)
        break;

      if((arg->nodetype == Identifier) && (arg->astnode.ident.arraylist != NULL) &&
            type_lookup(cur_array_table,arg->astnode.ident.name) &&
            !temp->astnode.ident.arraylist)
         fprintf(curfp,"%s _f2j_tmp%d = new %s(arg%d[arg%d_offset]);\n", 
           wrapper_returns[temp->vartype], i, wrapper_returns[temp->vartype], i, i);

      if(temp != NULL)
        temp = temp->nextstmt;
    }

    /*  now emit the call here */

    tempname = strdup( ((AST *) dl_val(p))->astnode.ident.name );
    *tempname = toupper(*tempname);

    if(hashtemp->variable->nodetype == Function)
    {
      fprintf(curfp,"%s %s_retval;\n\n", 
          returnstring[hashtemp->variable->astnode.source.returns],
          hashtemp->variable->astnode.source.name->astnode.ident.name);

      fprintf(curfp,"%s_retval = %s.%s(", ((AST *) dl_val(p))->astnode.ident.name,
         tempname,  ((AST *) dl_val(p))->astnode.ident.name );
    }
    else
      fprintf(curfp,"\n%s.%s(",tempname,  ((AST *) dl_val(p))->astnode.ident.name );
    
    temp = hashtemp->variable->astnode.source.args;
    arg = ((AST *)dl_val(p))->astnode.ident.arraylist;
    
    for(i = 0; arg != NULL ; arg = arg->nextstmt, i++)
    {
      if(temp == NULL)
        break;

      if((arg->nodetype == Identifier) && (arg->astnode.ident.arraylist != NULL) &&
            type_lookup(cur_array_table,arg->astnode.ident.name) &&
            !temp->astnode.ident.arraylist)
         fprintf(curfp,"_f2j_tmp%d",i);
      else if((arg->nodetype == Identifier) &&
            type_lookup(cur_array_table,arg->astnode.ident.name) &&
            temp->astnode.ident.arraylist)
         fprintf(curfp,"arg%d, arg%d_offset",i,i);
      else
         fprintf(curfp,"arg%d",i);

      if(temp != NULL)
        temp = temp->nextstmt;
      if(arg->nextstmt != NULL)
        fprintf(curfp,",");
    }

    fprintf(curfp,");\n\n");

    /*  assign the temp variable to the array element */

    temp = hashtemp->variable->astnode.source.args;
    arg = ((AST *)dl_val(p))->astnode.ident.arraylist;
    
    for(i = 0; arg != NULL ; arg = arg->nextstmt, i++)
    {
      if(temp == NULL)
        break;

      if((arg->nodetype == Identifier) && (arg->astnode.ident.arraylist != NULL) &&
            type_lookup(cur_array_table,arg->astnode.ident.name) &&
            !temp->astnode.ident.arraylist)
         fprintf(curfp,"arg%d[arg%d_offset] = _f2j_tmp%d.val;\n",i,i,i);

      if(temp != NULL)
        temp = temp->nextstmt;
    }

    if(hashtemp->variable->nodetype == Function)
    {
      fprintf(curfp,"\nreturn %s_retval;\n", 
          hashtemp->variable->astnode.source.name->astnode.ident.name);
    }

    fprintf(curfp,"}\n\n");
  }
}

/*
 * This is primarily a debugging tool.  Given a node, it returns a
 * string containing the node type.
 */

char * 
print_nodetype (AST *root) 
{
  static char temp[100];

  if(root == NULL) {
    return("print_nodetpe: NULL root");
  }

  switch (root->nodetype)
  {
    case Source:
      return("Source");
    case Progunit:
      return("Progunit");
    case Subroutine:
      return("Subroutine");
    case Function:
      return("Function");
    case Program:
      return("Program");
    case Blockif:
      return("Blockif");
    case Common:
      return("Common");
    case DataStmt:
      return("DataStmt");
    case DataList:
      return("DataList");
    case Elseif:
      return("Elseif");
    case Else:
      return("Else");
    case Forloop:
      return("Forloop");
    case Format:
      return("Format");
    case Constant:
      return("Constant");
    case Method:
      return("Method");
    case Identifier:
      return("Identifier");
    case Label:
      return("Label");
    case Logicalif:
      return("Logicalif");
    case Typedec:
      return("Typedec");
    case Assignment:
      return("Assignment");
    case Expression:
      return("Expression");
    case Return:
      return("Return");
    case Goto:
      return("Goto");
    case Call:
      return("Call");
    case Statement:
      return("Statement");
    case Relationalop:
      return("Relationalop");
    case Logicalop:
      return("Logicalop");
    case Binaryop:
      return("Binaryop");
    case Power:
      return("Power");
    case Unaryop:
      return("Unaryop");
    case Save:
      return("Save");
    case Specification:
      return("Specification");
    case Substring:
      return("Substring");
    case End:
      return("End");
    case Write:
      return("Write");
    case Stop:
      return("Stop");
    case ComputedGoto:
      return("ComputedGoto");
    case ArrayAccess:
      return("ArrayAccess");
    case ArrayDec:
      return("ArrayDec");
    case EmptyArgList:
      return("EmptyArgList");
    case Unimplemented:
      return("Unimplemented");
    default:
      sprintf(temp, "print_nodetype(): Unknown Node: %d", root->nodetype);
      return(temp);
  }
}
