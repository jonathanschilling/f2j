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
#include"list.h"

#define ONED 1
#define TWOD 0

#define NONSTATIC 0
#define STATIC_NODATA 1
#define STATIC_WITHDATA 2

#define WRAPPER

char * strdup ( const char * );
char * print_nodetype ( AST * ); 
char * lowercase ( char * );
HASHNODE * format_lookup(SYMTABLE *, char *);
char * methodscan (METHODTAB * , char * );
void format_name_emit(AST *);

char *progname;
char *returnname;
char *cur_filename;
int gendebug = 1;
int cur_idx = 0;
EntryList *doloop = NULL;
EntryList *while_list = NULL;

extern char *inputfilename;

FILE *javafp;

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

/*  
 *   Global variables, a necessary evil when working with
 * yacc. 
 */

char *returnstring[] =
{"String", "complex", "double", "float", "int", "boolean"};

#ifdef WRAPPER

char *wrapper_returns[] =
{"StringW", "complexW", "doubleW", "floatW", "intW", "booleanW"};

char *init_vals[] =
{"\"\"", "0", "0.0", "0.0", "0", "false"};

#endif

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

          open_output_file(root->astnode.source.progtype);
          
          /*  
           * At the beginning of each program unit, we look for
           * any variables that are listed in a SAVE statement.
           * These variables will be emitted as static class
           * variables, so we must do that now, before the
           * method header has been emitted.   10/3/97 -- Keith 
           */

          if(tmp != NULL)
            fprintf(javafp,"// Static variables (fortran SAVE stmt)\n");

          tmp = root->astnode.source.typedecs;
          while(tmp != NULL)
          {
            if(tmp->nodetype == Typedec)
              static_var_emit(tmp);
            tmp = tmp->nextstmt;
          }

	  emit (root->astnode.source.progtype);
	  fprintf (javafp, "// Type declarations.\n");
	  emit (root->astnode.source.typedecs);
	  fprintf (javafp, "\n// Executable code.\n");
	  emit (root->astnode.source.statements);
          fprintf(javafp,"} // End class.\n");
          fclose(javafp);
	  break;
        }
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
          if(gendebug)
            printf ("Function name: %s\n", 
              root->astnode.source.name->astnode.ident.name);
	  constructor (root);
	  break;
      case Program:
	  if (gendebug)
	      printf ("Program.\n");
	  returnname = NULL;	/* programs return void. */
/*	  returnname = root->astnode.source.name->astnode.ident.name; */
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
#ifdef WRAPPER
	      fprintf (javafp, "return %s.val;\n", returnname);
#else
	      fprintf (javafp, "return %s;\n", returnname);
#endif
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

          fprintf (javafp, "System.exit(1);\n");

          if (root->nextstmt != NULL)
            emit (root->nextstmt);
	  break;
      case End:
	  if (gendebug)
	      printf ("End.\n");
	  fprintf (javafp, "   }\n");
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
	  fprintf (javafp, "// WARNING: Unimplemented statement in Fortran source.\n");
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
 */

int
common_emit(AST *root)
{
  extern char *returnstring[];
  HASHNODE *hashtemp;
  AST *Ctemp;
  AST *Ntemp;
  char filename[100];
  FILE *commonfp;
  char * prefix = strtok(strdup(inputfilename),".");

  for(Ctemp=root;Ctemp!=NULL;Ctemp=Ctemp->nextstmt)
  {
    if(Ctemp->astnode.common.name != NULL) {
      sprintf(filename,"%s_%s.java", prefix, Ctemp->astnode.common.name);
      if((commonfp = fopen(filename,"w"))==NULL) {
        fprintf(stderr,"Cannot open output file '%s'.\n",filename);
        perror("Reason");
        exit(1);
      }

      if(Ctemp->astnode.common.name != NULL)
        fprintf(commonfp,"public class %s_%s\n{\n",prefix,
           Ctemp->astnode.common.name);
  
      for(Ntemp=Ctemp->astnode.common.nlist;Ntemp!=NULL;Ntemp=Ntemp->nextstmt)
      {
        if(gendebug)
          printf("Common block %s -- %s\n",Ctemp->astnode.common.name,
            Ntemp->astnode.ident.name);
  
        if((hashtemp = type_lookup(cur_type_table,Ntemp->astnode.ident.name)) == NULL)
        {
          fprintf(stderr,"Error: can't find type for common %s\n",
            Ntemp->astnode.ident.name);
          continue;
        }
#ifdef WRAPPER
        fprintf(commonfp,"static %s %s = new %s(%s);\n",
          wrapper_returns[hashtemp->type], Ntemp->astnode.ident.name,
          wrapper_returns[hashtemp->type], init_vals[hashtemp->type]);
#else
        fprintf(commonfp,"static %s %s;\n",returnstring[hashtemp->type], 
          Ntemp->astnode.ident.name);
#endif
      }
  
      if(Ctemp->astnode.common.name != NULL)
        fprintf(commonfp,"}\n",prefix);
  
      fclose(commonfp);
    }
  }
}

/* 
 * This function emits declarations for static class variables. 
 * First, loop through each variable for which there exists a
 * declaration and look in the SAVE table to see if that variable
 * should be static.  If so, emit it here.  If the variable also
 * has a DATA statement associated with it, the declaration should
 * not need to initialize the variable here since we'll worry about
 * that when emitting the DATA statement.   10/3/97 -- Keith 
 */

int
static_var_emit(AST *root)
{
  AST *temp;

  for(temp=root->astnode.typeunit.declist;temp!=NULL;temp=temp->nextstmt)
  {
    if(type_lookup(cur_save_table,temp->astnode.ident.name))
    {
      if(type_lookup (cur_args_table, temp->astnode.ident.name))
      {
        fprintf(stderr,"Can't save non-static argument %s\n",
           temp->astnode.ident.name);
        exit(1);
      }

      if(type_lookup(cur_data_table, temp->astnode.ident.name))
        vardec_emit(temp, root->astnode.typeunit.returns, STATIC_WITHDATA);
      else
        vardec_emit(temp, root->astnode.typeunit.returns, STATIC_NODATA);
    }
  }
}

/* Emit all the type declarations.  This procedure checks
   whether variables are typed in the argument list, and
   does not redeclare those arguments. */
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
             continue;
          }
          else
             if(gendebug)
               printf("@@ Variable %s: Corresponding data stmt not found\n",
                 temp->astnode.ident.name);

          if(type_lookup(cur_save_table,temp->astnode.ident.name)) {
             /* 
              * we already emitted this variable as a static variable 
              * (aka 'class variable'), so we don't emit it here. 
              */
             continue;
          }

          if(type_lookup(cur_common_table,temp->astnode.ident.name)) {
             /* 
              * also do not try to redefine a 'common' variable since
              * they are placed in their own classes.  10-8-97 -- Keith 
              */
             continue;
          }

          /* 
           * Let's do the argument lookup first. No need to retype variables
           * that are already declared in the argument list, or declared
           * as externals.  So if it is already declared, loop again.   
           */

	  hashtemp = type_lookup (cur_args_table, temp->astnode.ident.name);
	  if (hashtemp)
	      continue;

          vardec_emit(temp, returns, NONSTATIC);

      }
}				/* Close typedec_emit(). */

/* 
 * the body of this function used to be in typedec_emit, but
 * I moved it so that I could use the same code to emit static
 * or nonstatic variables.   10/3/97  -- Keith 
 */

int
vardec_emit(AST *root, enum returntype returns, int only_static)
{
  AST *temp2;
  char *prefix;

  if(only_static)         /* true if only_static is either  */
    prefix = "static ";   /*   STATIC_WITHDATA or STATIC_NODATA */
  else
    prefix = "";

  /* 
   * check to see if this is an array declaration or not. 
   * if so, we must generate the appropriate "new" statement.
   * otherwise, just declare & initialize in one statement. --keith 
   */

  if(root->astnode.ident.arraylist != NULL) {
    fprintf (javafp, "%s%s [] ",prefix, returnstring[returns]);

    if (gendebug)
      printf ("%s\n", returnstring[returns]);
    name_emit (root);

    if(only_static == STATIC_WITHDATA) {
      fprintf (javafp, ";\n");
    }
    else {
      if (returns == Integer)
        fprintf (javafp, "= new int[");
      else if (returns == Double)
        fprintf (javafp, "= new double[");
      else if (returns == Logical)
        fprintf (javafp, "= new boolean[");
      else
        fprintf(stderr,"typdec_emit():  Unknown type!\n");
         
      for(temp2=root->astnode.ident.arraylist;temp2!=NULL;temp2=temp2->nextstmt) {
        if(temp2 != root->astnode.ident.arraylist)
          fprintf(javafp, " * ");   /* if not the first iteration */
        expr_emit(temp2);
      }

      fprintf (javafp, "];\n");
    }
  } else {
#ifdef WRAPPER
    fprintf (javafp, "%s%s ", prefix, wrapper_returns[returns]);
#else
    fprintf (javafp, "%s%s ", prefix, returnstring[returns]);
#endif
    if (gendebug)
      printf ("%s\n", returnstring[returns]);
    name_emit (root);

    if(only_static == STATIC_WITHDATA) {
#ifdef WRAPPER
      if (returns == Integer)
        fprintf (javafp, "= new intW(%s)", init_vals[returns]);
      else if (returns == Double)
        fprintf (javafp, "= new doubleW(%s)", init_vals[returns]);
      else if (returns == Logical)
        fprintf (javafp, "= new booleanW(%s)", init_vals[returns]);
      else if (returns == Character)
        fprintf (javafp, "= new StringW(%s)", init_vals[returns]);
      fprintf (javafp, ";\n");
#else
      fprintf (javafp, ";\n");
#endif
    } else {
      /*  
       * initialize local variables to zero or
       * false to keep the java compiler from
       * squawking.  
       */

#ifdef WRAPPER
      if (returns == Integer)
        fprintf (javafp, "= new intW(%s)", init_vals[returns]);
      else if (returns == Double)
        fprintf (javafp, "= new doubleW(%s)", init_vals[returns]);
      else if (returns == Logical)
        fprintf (javafp, "= new booleanW(%s)", init_vals[returns]);
      else if (returns == Character)
        fprintf (javafp, "= new StringW(%s)", init_vals[returns]);
      fprintf (javafp, ";\n");
#else
      if (returns == Integer || returns == Double)
        fprintf (javafp, "= 0");
      else if (returns == Logical)
        fprintf (javafp, "= false");
      else if (returns == Character)
        fprintf (javafp, "= null");
      fprintf (javafp, ";\n");
#endif
    }
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
  AST *Dtemp, *Ntemp, *Ctemp;
  HASHNODE *hashtemp;
  int i, length=1, is_array=FALSE, count=1;

  /* foreach Data spec... */
  for(Dtemp = root->astnode.label.stmt; Dtemp != NULL; Dtemp = Dtemp->prevstmt) 
  {
    Ctemp = Dtemp->astnode.data.clist;

    /* foreach variable... */    
    for(Ntemp = Dtemp->astnode.data.nlist; Ntemp != NULL; Ntemp = Ntemp->nextstmt) 
    {
      /* This variable should have a type declaration associated with it */
      if((hashtemp = type_lookup(cur_type_table,Ntemp->astnode.ident.name)) == NULL)
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
        /* Check for attempts to initialize dummy argument: */

        if(hashtemp->variable->astnode.ident.leaddim[0] == '*')
        {
          fprintf(stderr,"Attempt to initialize dummy argument: %s\n",
            hashtemp->variable->astnode.ident.name);
          continue;
        }
        else if (type_lookup(cur_args_table,Ntemp->astnode.ident.name))
        {
          fprintf(stderr,"Attempt to initialize argument: %s\n",
            hashtemp->variable->astnode.ident.name);
          continue;
        }

        if(is_array)
        {
          AST *temp2;

          length = 1;

          /* determine the number of elements in this variable */
 
          temp2=hashtemp->variable->astnode.ident.arraylist;
          for( ; temp2 != NULL ; temp2=temp2->nextstmt ) {
            if(temp2->nodetype != Constant) {

              /*
               * fprintf(stderr,"Cant translate data statement for %s\n",
               *   hashtemp->variable->astnode.ident.name);
               */

              length = -1;
              break;
            }
            else {
              length *= atoi(temp2->astnode.constant.number);
            }
          }
        }
      }

#ifdef WRAPPER
      if(is_array)
          fprintf(javafp,"%s ", returnstring[ hashtemp->type]);
      else {
        if(type_lookup(cur_save_table,Ntemp->astnode.ident.name))
          fprintf(javafp,"%s ", returnstring[ hashtemp->type]);
        else
          fprintf(javafp,"%s ", wrapper_returns[ hashtemp->type]);
      }
#else
      fprintf(javafp,"%s ", returnstring[ hashtemp->type]);
#endif
   
      if( is_array ) {
        fprintf(javafp,"[] ");

        /* 
         * if this variable is static, we can't declare it here 
         * because it has been declared already as a class variable.
         * so we use the "_temp_" prefix and emit the initialization.
         * later we assign the temp variable to the class variable.
         * 10/3/97  --Keith
         */

        if(type_lookup(cur_save_table,Ntemp->astnode.ident.name))
          fprintf(javafp,"_temp_%s = {",Ntemp->astnode.ident.name);
        else
          fprintf(javafp,"%s = {",Ntemp->astnode.ident.name);

        for(i=0, count=0; (length == -1)?(Ctemp != NULL):(i< length);i++,count++) {
          if(Ctemp->token == STRING)
            fprintf(javafp,"\"%s\" ",Ctemp->astnode.ident.name);
          else {
            fprintf(javafp,"%s%s ",  
              Ctemp->astnode.constant.sign == 1 ? "-" : "",
              Ctemp->astnode.constant.number);
          }

          /* 
           * Every now and then, emit a newline for readability.
           * I have run across some lines that end up so long that
           * they screw up 'vi'.   9/30/97  --Keith 
           */
          if( count % 5 == 0 )
            fprintf(javafp,"\n");

          if( (Ctemp = Ctemp->nextstmt) == NULL )
            break;
          else {
            if(length == -1)
            {
              if (Ctemp != NULL)
                fprintf(javafp,", ");
            }
            else 
              if(i != length -1 )
                fprintf(javafp,", ");
          }
        }
   
        fprintf(javafp,"};\n");
        if(type_lookup(cur_save_table,Ntemp->astnode.ident.name))
          fprintf(javafp,"%s = _temp_%s;\n",Ntemp->astnode.ident.name,
             Ntemp->astnode.ident.name);
      }
      else {
        /* this case is for initialization of scalar items */

        if(Ctemp->token == STRING) {
          if(type_lookup(cur_save_table,Ntemp->astnode.ident.name))
            fprintf(javafp,"_temp_%s = \"%s\";\n",Ntemp->astnode.ident.name,
              Ctemp->astnode.ident.name);
          else
#ifdef WRAPPER
            fprintf(javafp,"%s = new StringW(\"%s\");\n",Ntemp->astnode.ident.name,
              Ctemp->astnode.ident.name);
#else
            fprintf(javafp,"%s = \"%s\";\n",Ntemp->astnode.ident.name,
              Ctemp->astnode.ident.name);
#endif
        }
        else {
          if(type_lookup(cur_save_table,Ntemp->astnode.ident.name))
            fprintf(javafp,"_temp_%s = %s%s;\n",Ntemp->astnode.ident.name,
              Ctemp->astnode.constant.sign == 1 ? "-" : "",
              Ctemp->astnode.constant.number);
          else
#ifdef WRAPPER
            fprintf(javafp,"%s = new %s(%s%s);\n",Ntemp->astnode.ident.name,
              wrapper_returns[ hashtemp->type],
              Ctemp->astnode.constant.sign == 1 ? "-" : "",
              Ctemp->astnode.constant.number);
#else
            fprintf(javafp,"%s = %s%s;\n",
              Ntemp->astnode.ident.name,
              Ctemp->astnode.constant.sign == 1 ? "-" : "",
              Ctemp->astnode.constant.number);
#endif
        }

#ifdef WRAPPER
        if(type_lookup(cur_save_table,Ntemp->astnode.ident.name))
          fprintf(javafp,"%s.val = _temp_%s;\n",Ntemp->astnode.ident.name,
             Ntemp->astnode.ident.name);
#else
        if(type_lookup(cur_save_table,Ntemp->astnode.ident.name))
          fprintf(javafp,"%s = _temp_%s;\n",Ntemp->astnode.ident.name,
             Ntemp->astnode.ident.name);
#endif

        Ctemp = Ctemp->nextstmt;
      }
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
  else if( methodscan (intrinsic_toks, tempname) != NULL) 
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
        fprintf (javafp, "\"%s\"", root->astnode.ident.name);
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
}

/*  This function emits a subroutine call */

int 
subcall_emit(AST *root)
{
  AST *temp;
  char *tempstr;

  tempstr = strdup (root->astnode.ident.name);
  *tempstr = toupper (*tempstr);

  fprintf (javafp, "%s.%s", tempstr,root->astnode.ident.name);
  temp = root->astnode.ident.arraylist;

  fprintf (javafp, "(");
  for (temp; temp != NULL; temp = temp->nextstmt)
  {
    if(temp != root->astnode.ident.arraylist)
      fprintf (javafp, ",");  /* if not first iteration */
                        
    if (*temp->astnode.ident.name != '*')
      expr_emit (temp);
  }
  fprintf (javafp, ")");
}

/*  
 *  This function emits the index to an array.  The boolean argument
 *  is_arg represents whether the array is an argument to the current 
 *  function or subroutine and the boolean is_ext represents whether
 *  the array is being passed to an external function.
 */    
int
func_array_emit(AST *root, HASHNODE *hashtemp, char *arrayname, int is_arg, int is_ext)
{
  HASHNODE *ht;

#if ONED
  if(is_ext)
    fprintf (javafp, ",");
  else
    fprintf (javafp, "[");

printf("~looking up %s in the array table\n", arrayname);

  ht = type_lookup(cur_array_table, arrayname);
  if(ht == NULL)
  {
      printf("~Could not find!\n");
  }
  else if(ht->variable->astnode.ident.dim == 3)
  {
    HASHNODE *p;
    AST *temp;
    int D[3], i, offset;

    printf("~found %s, has dim %d\n",ht->variable->astnode.ident.name,
       ht->variable->astnode.ident.dim);

    for(temp = ht->variable->astnode.ident.arraylist, i = 0;
        temp != NULL;
        temp = temp->nextstmt, i++)
    {
      if(temp->nodetype == Identifier)
      {
printf("looking for %s in parameter table\n",temp->astnode.ident.name);

        p = type_lookup(cur_param_table, temp->astnode.ident.name);

        if(p == NULL)
          fprintf(stderr,"Cant find %s in parameter table!\n",
            temp->astnode.ident.name);
        else 
        {
printf("FOUND %s in parameter table\n",temp->astnode.ident.name);
           
           if(p->variable->nodetype == Constant)
             D[i]=atoi(p->variable->astnode.constant.number); 
           else
             fprintf(stderr,"Cant determine array dimensions!\n");        
        }
      }
      else if(temp->nodetype == Constant)
      {
        D[i] = atoi(temp->astnode.constant.number);
      }
      else
        fprintf(stderr,"Error: unsupported nodetype in 3D array dec.\n");
    }
    printf("Ok, the dims are %d,%d,%d\n",D[0],D[1],D[2]);

    offset = 1 + ( (1 + D[1]) * D[0]);

    fprintf (javafp, "(");
    expr_emit(root);
    fprintf (javafp, ")");
    
    fprintf (javafp, "+((");

    fprintf (javafp, "(");
    expr_emit(root->nextstmt);
    fprintf (javafp, ")");
    
    fprintf (javafp, "+(");

    fprintf (javafp, "(");
    expr_emit(root->nextstmt->nextstmt);
    fprintf (javafp, ")");
    
    fprintf (javafp, " * %d)) *%d) - %d",D[1],D[0],offset);
  }
  else 
  {
    fprintf (javafp, "(");
    expr_emit (root);
    fprintf (javafp, ")- 1");

    if (hashtemp->variable->astnode.ident.leaddim[0] != '*' && root->nextstmt != NULL)
    {
      root = root->nextstmt;
      fprintf (javafp, "+");
      fprintf (javafp, "(");
      expr_emit (root);
      fprintf (javafp, "- 1)");
      fprintf (javafp, "*");
#ifdef WRAPPER
      if(isalpha(hashtemp->variable->astnode.ident.leaddim[0]))
        fprintf(javafp,  "%s.val", hashtemp->variable->astnode.ident.leaddim);
      else
        fprintf(javafp,  "%s", hashtemp->variable->astnode.ident.leaddim);
#else
      fprintf(javafp,  "%s", hashtemp->variable->astnode.ident.leaddim);
#endif
    }  /* Multi dimension.  */
  }

  if(is_arg)
    fprintf(javafp,  "+ _%s_offset",arrayname);

  if(! is_ext)
    fprintf(javafp, "]");
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

  if (gendebug)
    printf ("Array... %s, My node type is %s\n", 
      root->astnode.ident.name,
      print_nodetype(root));

  /* 
   * Now, what needs to happen here is the context of the
   * array needs to be determined.  If the array is being
   * passed as a parameter to a method, then the array index
   * needs to be passed separately and the array passed as
   * itself.  If not, then an array value is being set,
   * so dereference with index arithmetic.  
   */

  fprintf (javafp, "%s", root->astnode.ident.name);
  temp = root->astnode.ident.arraylist;

  if(root->parent == NULL) {
    /* Under normal circumstances, I dont think this should 
       be reached */
    fprintf (stderr,"Array... %s, NO PARENT - ", root->astnode.ident.name);
    fprintf (stderr,"This is not good!\n");
  } else {
    if(gendebug)
      printf ("Array... %s, Parent node type... %s\n", 
        root->astnode.ident.name,
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
 *   indices)
 */

int
scalar_emit(AST *root, HASHNODE *hashtemp)
{
  extern METHODTAB intrinsic_toks[];
  char *com_prefix;

  if(hashtemp == NULL) {
    char * prefix = strtok(strdup(inputfilename),".");
    HASHNODE *ht;

    ht = type_lookup(cur_common_table, root->astnode.ident.name);

#ifdef WRAPPER

 printf("here we are emitting a scalar: %s,",root->astnode.ident.name);
 printf("The parent node is : %s\n",print_nodetype(root->parent));


    if(ht == NULL)
      com_prefix = "";
    else {
      com_prefix = (char *) malloc(
         strlen(ht->variable->astnode.ident.commonBlockName) +
         strlen(prefix) + 3);

      sprintf(com_prefix,"%s_%s.", prefix,
        ht->variable->astnode.ident.commonBlockName);
    }

    if(root->parent == NULL) {
      fprintf(stderr,"name_emit(): NO PARENT! (%s)\n",
        root->astnode.ident.name);
    } else {
      if (root->parent->nodetype == Call) {
        char *tempname;

        if(gendebug)
          printf("in CALL, '%s' <- '%s'\n", 
            root->parent->astnode.ident.name,
            root->astnode.ident.name);

        tempname = strdup(root->parent->astnode.ident.name);
        uppercase(tempname);

        if((methodscan (intrinsic_toks, tempname) == NULL) &&
           (type_lookup(cur_array_table, root->parent->astnode.ident.name) == NULL))
        {
          printf("did not find %s in intrinsics table\n",
               root->parent->astnode.ident.name);
          fprintf (javafp, "%s%s", com_prefix, root->astnode.ident.name);
        }
        else
        {
          printf("found %s in intrinsics\n",
               root->parent->astnode.ident.name);
          fprintf (javafp, "%s%s.val", com_prefix,root->astnode.ident.name);
        }
      }
      else if(root->parent->nodetype == Typedec) {
        fprintf (javafp, "%s%s", com_prefix, root->astnode.ident.name);
      }
      else if(root->parent->nodetype == ArrayDec) {
#ifdef WRAPPER
        fprintf (javafp, "%s%s.val", com_prefix, root->astnode.ident.name);
#else
        fprintf (javafp, "%s%s", com_prefix, root->astnode.ident.name);
#endif
      }
      else {
        fprintf (javafp, "%s%s.val", com_prefix, root->astnode.ident.name);
      }
    }
#else
    if(ht == NULL) {
      fprintf (javafp, "%s", root->astnode.ident.name);
    }
    else {
      fprintf (javafp, "%s_%s.%s", prefix, 
        ht->variable->astnode.ident.commonBlockName,
        root->astnode.ident.name);
    }
#endif
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
 printf("CRAP here we are emitting a scalar: %s,",root->astnode.ident.name);
 printf("The parent node is : %s\n",print_nodetype(root->parent));

      if((root->parent->nodetype == Call) && 
         (type_lookup(cur_external_table, root->parent->astnode.ident.name) != NULL))
      {
        if( type_lookup(cur_args_table,root->astnode.ident.name) != NULL )
          fprintf (javafp, "%s,_%s_offset", root->astnode.ident.name,
             root->astnode.ident.name);
        else
          fprintf (javafp, "%s,0", root->astnode.ident.name);
      }
      else 
        fprintf (javafp, "%s", root->astnode.ident.name);
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
#ifdef WRAPPER
      fprintf (javafp, "%s.val", temp->astnode.ident.name);
#else
      fprintf (javafp, "%s", temp->astnode.ident.name);
#endif
      fprintf (javafp, "%s(", javaname);
      name_emit (temp->nextstmt);
      fprintf (javafp, ")");
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

#ifdef WRAPPER
      fprintf (javafp, "%s.val", temp->nextstmt->astnode.ident.name);
#else
      fprintf (javafp, "%s", temp->nextstmt->astnode.ident.name);
#endif
      fprintf (javafp, "%s(true,0,", javaname);
      name_emit (temp->nextstmt->nextstmt);
      fprintf (javafp, ",0,");
      expr_emit (temp);
      fprintf (javafp, ")");
      return;
    }
  }
}

int
intrinsic_emit(AST *root)
{
  extern METHODTAB intrinsic_toks[];
  AST *temp;
  char *tempname, *javaname;

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
  /*put end brace here when KJGKJKJKKJH defined*/ 
#endif	  

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

  if ( (!strcmp (tempname, "DSQRT")) ||
       (!strcmp (tempname, "SQRT"))  ||
       (!strcmp (tempname, "DABS"))  ||
       (!strcmp (tempname, "DBLE"))  ||
       (!strcmp (tempname, "ABS")))
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
    fprintf(javafp,"(");
    expr_emit(temp);
    fprintf(javafp,")%%(");
    expr_emit(temp->nextstmt);
    fprintf(javafp,") ");

    /*  
     *  this chunk of code will emit a call to MOD as a call to
     *  Math.IEEERemainder().  usually that is not appropriate
     *  since IEEERemainder returns double, whereas the expected
     *  type is int.   -- keith
     *
     * fprintf (javafp, "%s(", javaname);
     * expr_emit (temp);
     * fprintf (javafp, ", ");
     * expr_emit (temp->nextstmt);
     * fprintf (javafp, ")");
     */

    return;
  }
}

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

     /* 
      * here we need to determine if this is a parameter to a function
      * or subroutine.  if so, and we are using wrappers, then we need
      * to create a temporary wrapper and pass that in instead of the
      * constant.   10/9/97  -- Keith 
      */

#ifdef WRAPPER
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
         fprintf (javafp, "new StringW(\"%s\")", root->astnode.ident.name);
       else {
         fprintf (javafp, "new %s(%s)", 
           wrapper_returns[get_type(root->astnode.constant.number)],
           root->astnode.constant.number);
         }
       }
       else 
       {
         if(root->token == STRING)
           fprintf (javafp, "\"%s\"", root->astnode.ident.name);
         else
           fprintf (javafp, "%s", root->astnode.constant.number);
       }
#else
       if(root->token == STRING)
         fprintf (javafp, "\"%s\"", root->astnode.ident.name);
       else
         fprintf (javafp, "%s", root->astnode.constant.number);
#endif
       break;
    case Logicalop:
      /* 
       * Change all of this code to switch on the tokens.
       * The parser code will have to store the NOT token.
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

          if((root->astnode.expression.lhs->vartype == Character) &&
             (root->astnode.expression.rhs->vartype == Character))
          {
            expr_emit (root->astnode.expression.lhs);
            fprintf(javafp,".equalsIgnoreCase(");
            expr_emit (root->astnode.expression.rhs);
            fprintf(javafp,")");
          }
          else
          {
            expr_emit (root->astnode.expression.lhs);
            fprintf (javafp, " == ");
            expr_emit (root->astnode.expression.rhs);
          }
          break;
        case rel_ne:
          if((root->astnode.expression.lhs->vartype == Character) &&
             (root->astnode.expression.rhs->vartype == Character))
          {
            fprintf(javafp,"!");
            expr_emit (root->astnode.expression.lhs);
            fprintf(javafp,".equalsIgnoreCase(");
            expr_emit (root->astnode.expression.rhs);
            fprintf(javafp,")");
          }
          else
          {
            expr_emit (root->astnode.expression.lhs);
            fprintf (javafp, " != ");
            expr_emit (root->astnode.expression.rhs);
          }
          break;
        case rel_lt:
          expr_emit (root->astnode.expression.lhs);
          fprintf (javafp, " < ");
          expr_emit (root->astnode.expression.rhs);
          break;
        case rel_le:
          expr_emit (root->astnode.expression.lhs);
          fprintf (javafp, " <= ");
          expr_emit (root->astnode.expression.rhs);
          break;
        case rel_gt:
          expr_emit (root->astnode.expression.lhs);
          fprintf (javafp, " > ");
          expr_emit (root->astnode.expression.rhs);
          break;
        case rel_ge:
          expr_emit (root->astnode.expression.lhs);
          fprintf (javafp, " >= ");
          expr_emit (root->astnode.expression.rhs);
          break;
      }
      break;
    case Substring:
#ifdef WRAPPER
      fprintf(javafp,"%s.val.substring((",root->astnode.ident.name);
#else
      fprintf(javafp,"%s.substring((",root->astnode.ident.name);
#endif
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
open_output_file(AST *root)
{
  char * filename;
  char * classname;
  
  filename = lowercase(strdup(root->astnode.source.name->astnode.ident.name));
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
#ifdef WRAPPER
      fprintf (javafp, "static %s %s = new %s(%s);\n\n", 
         wrapper_returns[returns],
         root->astnode.source.name->astnode.ident.name,
         wrapper_returns[returns],
         init_vals[returns]);
#else
      fprintf (javafp, "static %s %s;\n\n", returnstring[returns],
         root->astnode.source.name->astnode.ident.name);
#endif

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

#ifdef WRAPPER
	  if (hashtemp->variable->astnode.ident.arraylist == NULL)
	    tempstring = wrapper_returns[returns];
          else
	    tempstring = returnstring[returns];
#else
	  tempstring = returnstring[returns];
#endif

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

                /* 
                 * for arrays, add a parameter representing the base 
                 * index.   -- Keith 
                 */
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

   /*  
    *  Some point I will need to test whether this is really a name
    *  because it will crash if not.  
    */
  indexname = 
      root->astnode.forloop.start->astnode.assignment.lhs->astnode.ident.name;

   /* print out a label for this for loop */
  fprintf(javafp, "forloop%d:\n",
     root->astnode.forloop.Continue->astnode.label.number);
   
  if(root->astnode.forloop.incr != NULL)
  {
    fprintf(javafp,"int _%s_inc = ", indexname);
    expr_emit (root->astnode.forloop.incr);
    fprintf(javafp, ";\n");
  }

   /* This block writes out the loop parameters.  */

  fprintf (javafp, "for (");

  assign_emit (root->astnode.forloop.start);

  fprintf(javafp, "; ");

  if(root->astnode.forloop.incr == NULL)
  {
    name_emit(root->astnode.forloop.start->astnode.assignment.lhs);

    fprintf(javafp, " <= ");
    expr_emit (root->astnode.forloop.stop);

    fprintf (javafp, "; ");

    name_emit(root->astnode.forloop.start->astnode.assignment.lhs);

    fprintf (javafp, "++");
  }
  else
  {
    fprintf(javafp,"(_%s_inc < 0) ? ",indexname);
    name_emit(root->astnode.forloop.start->astnode.assignment.lhs);
    fprintf(javafp," >= ");
    expr_emit (root->astnode.forloop.stop);
    fprintf(javafp," : ");
    name_emit(root->astnode.forloop.start->astnode.assignment.lhs);
    fprintf(javafp," <= ",indexname);
    expr_emit (root->astnode.forloop.stop);
    fprintf (javafp, "; ");
    
    name_emit(root->astnode.forloop.start->astnode.assignment.lhs);
    fprintf (javafp, " += _%s_inc",indexname);
  }

  fprintf (javafp, ") {\n");
   /*  Done with loop parameters.  */

   /* Statements in the body of the for() loop. */
  emit (root->astnode.forloop.stmts);

  fprintf (javafp, "}              //  Close for() loop. \n");

   /* 
    * finally pop this loop's label number off the stack and 
    * emit the label (for experimental goto resolution) 
    */
   
  fprintf(javafp,"Dummy.label(\"%s\",%d);\n",cur_filename,
     list_pop(&doloop));
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
  if( (doloop != NULL) && list_search(&doloop, root->astnode.go_to.label) )
  {
     /*
      *  we are inside a do loop and we are looking at a goto
      *  statement to the 'continue' statement of an enclosing loop.
      *  what we want to do here is just emit a 'labeled continue' 
      */ 

    fprintf(javafp,"continue forloop%d;\n",root->astnode.go_to.label);
  }
  else if((while_list != NULL) && 
     (list_examine(&while_list) == root->astnode.go_to.label ))
  {
       /* 
        *  we are inside a simulated while loop and we are looking at 
        *  a goto statement to the 'beginning' statement of the most
        *  enclosing if statment.  Since we are translating this to an 
        *  actual while loop, we ignore this goto statement 
        */

    fprintf(javafp,"// goto %d (end while)\n",root->astnode.go_to.label);
  }
  else 
  {
       /*  
        *  otherwise, not quite sure what to do with this one, so
        *  we'll just emit a dummy goto 
        */

    fprintf(javafp,"Dummy.go_to(\"%s\",%d);\n",cur_filename,
        root->astnode.go_to.label);
  }
}

int
computed_goto_emit (AST *root)
{
  AST *temp;
  int count = 1;

  for(temp = root->astnode.computed_goto.intlist;temp!=NULL;temp=temp->nextstmt)
  {
    if(temp != root->astnode.computed_goto.intlist)
      fprintf(javafp,"else ");
    fprintf(javafp,"if (");
    expr_emit(root->astnode.computed_goto.name);
    fprintf(javafp," == %d) \n", count);
    fprintf(javafp,"  Dummy.go_to(\"%s\",%s);\n", cur_filename, 
      temp->astnode.constant.number);
    count++;
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
      fprintf(javafp,"Dummy.label(\"%s\",%d);\n",cur_filename,
        root->astnode.label.number);
      emit (root->astnode.label.stmt);
      fprintf(javafp,"}\n");
    }
  } 
  else {
    fprintf(javafp,"{\n");
    fprintf (javafp, "label%d:\n   ", root->astnode.label.number);
    fprintf(javafp,"Dummy.label(\"%s\",%d);\n",cur_filename,
      root->astnode.label.number);
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
  if(gendebug)
    printf("***Looking for format statement number: %s\n",tmp);

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
        fprintf(javafp," + ");
      return(temp->nextstmt);
      break;
    case STRING:
      if(gendebug)
        printf("STring: %s\n",temp->astnode.ident.name);
      fprintf(javafp,"\"%s\" ",temp->astnode.ident.name);
      if(temp->nextstmt != NULL)
        fprintf(javafp," + ");
      return(temp->nextstmt);
      break;
    case REPEAT:
      if(gendebug)
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
      if(gendebug)
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
          else if(temp->nextstmt->astnode.ident.name[0] == 'P') {
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
      if(gendebug)
        printf("Comma\n");
      return(temp->nextstmt);
      break;
    case DIV:
      if(gendebug)
        printf("Div\n");
      fprintf(javafp,"\"\\n\" ");
      if(temp->nextstmt != NULL)
        fprintf(javafp," + ");
      return(temp->nextstmt);
      break;
    case CAT:
      if(gendebug)
        printf("two divs\n");
      fprintf(javafp,"\"\\n\\n\" ");
      if(temp->nextstmt != NULL)
        fprintf(javafp," + ");
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

void
format_name_emit(AST *node)
{
  if(node == NULL) {
printf("*** BAD FORMATTING\n");
    fprintf(stderr,"Bad formatting!\n");
    fprintf(javafp,"\" NULL \"");
  }
  else {
      /* 
       * in the write statement is an array, with no index specified.
       * so we will keep grabbing data from the array until the end
       * of the format specification 
       */

/*  gotta get this part finished someday   10/3/97 -- Keith

    if( (node->token == NAME) && 
        (type_lookup(cur_array_table, root->astnode.ident.name) != NULL) &&
        (root->astnode.ident.arraylist == NULL) )
    {

    
    }
    else
*/
      expr_emit(node);
  }
  fprintf(javafp," + \" \" ");
}

int
blockif_emit (AST * root)
{
  AST *prev = root->prevstmt;
  AST *temp;

  if(prev != NULL)
    if(prev->nodetype == Label)
    {
      /* push this while loop's number on the stack */
      list_push(&while_list, root->prevstmt->astnode.label.number);

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

      /* pop this while loop's label number off the stack */
      list_pop(&while_list);
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

  fprintf(javafp, "while (");
  if (root->astnode.blockif.conds != NULL)
    expr_emit (root->astnode.blockif.conds);
  fprintf (javafp, ")  {\n    ");
  emit (root->astnode.blockif.stmts);
  fprintf (javafp, "}              // Close if()\n");

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

  lowercase (root->astnode.ident.name);
  tempname = strdup (root->astnode.ident.name);
  *tempname = toupper (*tempname);

  /* Assume all methods that are invoked are static.  */
  fprintf (javafp, "%s.%s", tempname, root->astnode.ident.name);

  assert (root->astnode.ident.arraylist != NULL);

  fprintf (javafp, "(");

  printf("Looking up function name %s, ", root->astnode.ident.name);

  if( (hashtemp = type_lookup(function_table, root->astnode.ident.name)) != NULL )
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
           fprintf(javafp,"%s",temp->astnode.ident.name);
           func_array_emit(temp->astnode.ident.arraylist,ht,
              temp->astnode.ident.name, ht2!=NULL, TRUE);
         }
         else                                /* it is not expecting an array */
         {
#ifdef WRAPPER
           fprintf(javafp,"new %s(", wrapper_returns[t2->vartype]);
#endif
           fprintf(javafp,"%s",temp->astnode.ident.name);

           func_array_emit(temp->astnode.ident.arraylist,ht,
              temp->astnode.ident.name, ht2!=NULL, FALSE);

#ifdef WRAPPER
           fprintf(javafp,")");
#endif
         }
       }
         /* 
          * else (if the arg is an identifier AND
          *       it does not look like an array access AND
          *       it is in the array table) OR
          */
       else if((temp->nodetype == Identifier)&&(temp->astnode.ident.arraylist == NULL)
          && type_lookup(cur_array_table, temp->astnode.ident.name) )
       {
         if(t2->astnode.ident.arraylist)     /* it is expecting an array */
         {
printf("expecting array\n");
           expr_emit(temp);
         }
         else
         {
printf("NOT expecting array\n");
#ifdef WRAPPER
         fprintf(javafp,"new %s(", wrapper_returns[t2->vartype]);
#endif
         fprintf(javafp,"%s[0]", temp->astnode.ident.name);
#ifdef WRAPPER
         fprintf(javafp,")");
#endif
         }
       }
       else if(
         ( (temp->nodetype == Identifier)&&(temp->astnode.ident.arraylist == NULL) )
         || (temp->nodetype == Constant) )
       {
         expr_emit(temp);
       }
         /* 
          * Otherwise, use wrappers.
          */
       else 
       {
#ifdef WRAPPER
         fprintf(javafp,"new %s(", wrapper_returns[t2->vartype]);
#endif

         expr_emit(temp);

#ifdef WRAPPER
         fprintf(javafp,")");
#endif
       }
       t2 = t2->nextstmt;
       if(temp->nextstmt != NULL)
         fprintf(javafp, ",");
    }
  }
  else
  {
    temp = root->astnode.ident.arraylist;

    for( ; temp != NULL; temp = temp->nextstmt)
    {
#ifdef WRAPPER
      if(((temp->nodetype == Identifier) && (temp->astnode.ident.arraylist == NULL)) ||
          (temp->nodetype == Constant))
      {
        expr_emit (temp);
      }
      else
      {
        fprintf(javafp,"new %s(", wrapper_returns[temp->vartype]);
        expr_emit (temp);
        fprintf(javafp,")");
      }
#else
      expr_emit (temp);
#endif

      if(temp->nextstmt != NULL)
        fprintf(javafp, ",");
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
	  /* 
           * PARAMETER in fortran corresponds to a class
	   * constant in java, that has to be declared
	   * class wide outside of any method.  This is
	   * currently not implemented, but the assignment
	   * is made.  
           */
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

  if(root == NULL) {
    return("print_nodetpe: NULL root");
  }

  switch (root->nodetype)
  {
    case 0:
      return("Bad Node");
    case Source:
      return("Source");
    case Progunit:
      return("Progunit");
    case Program:
      return("Program");
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
    case Write:
      return("Write");
    case Format:
      return("Format");
    case Save:
      return("Save");
    case DataList:
      return("DataList");
    case Common:
      return("Common");
    case ComputedGoto:
      return("Computed goto");
    default:
      sprintf(temp, "print_nodetype(): Unknown Node: %d", root->nodetype);
      return(temp);
  }
}
