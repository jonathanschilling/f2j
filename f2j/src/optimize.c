/*
 * $Source$
 * $Revision$
 * $Author$
 */

/*  
 * optimize.c
 *   Determines which scalars really need to be wrapped
 * in objects for emulation of pass-by-reference.
 */

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>
#include"f2j.h"
#include"f2jparse.tab.h"
#include"dlist.h"

int optdebug = TRUE;

char * strdup ( const char * );
char * print_nodetype ( AST * ); 
char * lowercase ( char * );
char * methodscan (METHODTAB * , char * );

AST *ASTroot;

/*
 * optimize
 */

void
optimize (AST * root)
{
  void assign_optimize(AST *);
  void call_optimize(AST *);
  void forloop_optimize(AST *);
  void blockif_optimize(AST *);
  void elseif_optimize(AST *);
  void else_optimize(AST *);
  void logicalif_optimize(AST *);
  void read_optimize(AST *);

    switch (root->nodetype)
      {
      case 0:
	  if (optdebug)
	      printf ("Bad node\n");
	  optimize (root->nextstmt);
      case Progunit:
        {
	  if (optdebug)
	      printf ("Source.\n");

          ASTroot = root;
          optimize(root->astnode.source.typedecs);
          optimize(root->astnode.source.progtype);
          optimize(root->astnode.source.statements);

	  break;
        }
      case Subroutine:
	  if (optdebug)
	      printf ("Subroutine.\n");

	  break;
      case Function:
	  if (optdebug)
	      printf ("Function.\n");

          if(optdebug)
            printf ("Function name: %s\n", 
              root->astnode.source.name->astnode.ident.name);

	  break;
      case Program:
	  if (optdebug)
	      printf ("Program.\n");

	  if (optdebug)
	    printf ("Program name: %s\n", 
              root->astnode.source.name->astnode.ident.name);

          break;
      case Typedec:
	  if (optdebug)
	      printf ("Typedec.\n");

	  if (root->nextstmt != NULL)	/* End of typestmt list. */
	      optimize (root->nextstmt);
	  break;
      case DataList:
	  if (optdebug)
	      printf ("Data.\n");

	  if (root->nextstmt != NULL)	/* End of data list. */
	      optimize (root->nextstmt);
	  break;
      case Specification:
	  if (optdebug)
	      printf ("Specification.\n");

	  if (root->nextstmt != NULL)	/* End of typestmt list. */
	      optimize (root->nextstmt);
	  break;
      case Equivalence:
	  if (optdebug)
	      printf ("Equivalence.\n");

	  if (root->nextstmt != NULL)
	      optimize (root->nextstmt);
	  break;
      case Statement:
	  if (optdebug)
	      printf ("Statement.\n");

	  if (root->nextstmt != NULL)	/* End of typestmt list. */
	      optimize (root->nextstmt);
	  break;

      case Assignment:
	  if (optdebug)
	      printf ("Assignment.\n");

	  assign_optimize (root);

	  if (root->nextstmt != NULL)
	      optimize (root->nextstmt);
	  break;
      case Call:
	  if (optdebug)
	      printf ("Call.\n");

	  call_optimize (root);

	  if (root->nextstmt != NULL)	/* End of typestmt list. */
	      optimize (root->nextstmt);
	  break;
      case Forloop:
	  if (optdebug)
	      printf ("Forloop.\n");

	  forloop_optimize (root);

	  if (root->nextstmt != NULL)	/* End of typestmt list. */
	      optimize (root->nextstmt);
	  break;

      case Blockif:
	  if (optdebug)
	      printf ("Blockif.\n");

	  blockif_optimize (root);

	  if (root->nextstmt != NULL)	/* End of typestmt list. */
	      optimize (root->nextstmt);
	  break;
      case Elseif:
	  if (optdebug)
	      printf ("Elseif.\n");

	  elseif_optimize (root);

	  if (root->nextstmt != NULL)	/* End of typestmt list. */
	      optimize (root->nextstmt);
	  break;
      case Else:
	  if (optdebug)
	      printf ("Else.\n");

	  else_optimize (root);

	  if (root->nextstmt != NULL)	/* End of typestmt list. */
	      optimize (root->nextstmt);
	  break;
      case Logicalif:
	  if (optdebug)
	      printf ("Logicalif.\n");

	  logicalif_optimize (root);

	  if (root->nextstmt != NULL)	/* End of typestmt list. */
	      optimize (root->nextstmt);
	  break;
      case Return:
	  if (optdebug)
	    printf ("Return.\n");
            
	  if (root->nextstmt != NULL)	/* End of typestmt list. */
	      optimize (root->nextstmt);
	  break;
      case Goto:
	  if (optdebug)
	      printf ("Goto.\n");

	  if (root->nextstmt != NULL)
	      optimize (root->nextstmt);
	  break;
      case ComputedGoto:
	  if (optdebug)
	      printf ("Goto.\n");

	  if (root->nextstmt != NULL)
	      optimize (root->nextstmt);
	  break;
      case Label:
	  if (optdebug)
	      printf ("Label.\n");

	  if (root->nextstmt != NULL)	/* End of typestmt list. */
	      optimize (root->nextstmt);
	  break;
      case Write:
	  if (optdebug)
	      printf ("Write statement.\n");

	  if (root->nextstmt != NULL)
	      optimize (root->nextstmt);
	  break;
      case Read:
	  if (optdebug)
	      printf ("Read statement.\n");

	  read_optimize (root);

	  if (root->nextstmt != NULL)
	      optimize (root->nextstmt);
	  break;
      case Format:
	  if (optdebug)
            printf("skipping format statement\n");

	  if (root->nextstmt != NULL)
	      optimize (root->nextstmt);
          break;
      case Stop:
          if (optdebug)
            printf ("Stop.\n");

          if (root->nextstmt != NULL)
            optimize (root->nextstmt);
	  break;
      case End:
	  if (optdebug)
	      printf ("End.\n");
	  break;
      case Save:
	  if (optdebug)
	      printf ("Save (ignoring).\n");

          if (root->nextstmt != NULL)
            optimize (root->nextstmt);
	  break;
      case Common:
	  if (optdebug)
	      printf ("Common.\n");

          if (root->nextstmt != NULL)
            optimize (root->nextstmt);
	  break;
      case Unimplemented:
	  if (root->nextstmt != NULL)
	      optimize (root->nextstmt);
	  break;
      case Constant:
      default:
          fprintf(stderr,"optimize(): Error, bad nodetype (%s)\n",
            print_nodetype(root));
      }				/* switch on nodetype.  */
}

/* 
 * A name will either fly solo or lead off
 * a named array.  So far, this code will optimize
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
 *  Started cleaning up name_optimize  10/10/97  --Keith
 */

void
name_optimize (AST * root)
{
  HASHNODE *hashtemp;
  char * tempname;
  extern METHODTAB intrinsic_toks[];
  SYMTABLE *opt_external_table = ASTroot->astnode.source.external_table;
  SYMTABLE *opt_intrinsic_table = ASTroot->astnode.source.intrinsic_table;
  SYMTABLE *opt_type_table = ASTroot->astnode.source.type_table;
  SYMTABLE *opt_array_table = ASTroot->astnode.source.type_table;

  void subcall_optimize(AST *);

  printf("entering name_optimize\n");

  /*  
   *  Check to see whether name is in external table.  Names are
   *  loaded into the external table from the parser.   
   */

  if(root->nodetype == Identifier)
    if(root->token == STRING)
      printf("** maybe I should optimize a string literal here\n");

  tempname = strdup(root->astnode.ident.name);
  uppercase(tempname);

  /* 
   * If the name is in the external table, then check to see if
   * it is an intrinsic function instead (e.g. SQRT, ABS, etc).  
   */

  if (type_lookup (opt_external_table, root->astnode.ident.name) != NULL)
  {
  }
  else if(( methodscan (intrinsic_toks, tempname) != NULL) 
     && ( (type_lookup(opt_intrinsic_table, root->astnode.ident.name) != NULL)
       || (type_lookup(opt_type_table, root->astnode.ident.name) == NULL)))
  {
  }
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
        if(optdebug)
          printf("** I am going to optimize a String/char literal!\n");
        break;
      case INTRINSIC: 
        /* do nothing */
        break;
      case NAME:
      default:

        hashtemp = type_lookup (opt_array_table, root->astnode.ident.name);

        if (root->astnode.ident.arraylist == NULL)
        {
        }
        else if (hashtemp != NULL)
        {
        }
        else
          subcall_optimize(root);
        break;
    }
  printf("leaving name_optimize\n");
}

/*  This function optimize a function call.  I think this function
 * is only called in cases where the function or subroutine is
 * not declared external or intrinsic and we dont know what
 * else to do with it.
 */

void 
subcall_optimize(AST *root)
{
  AST *temp;
  char *tempstr;
  void expr_optimize (AST *);

  tempstr = strdup (root->astnode.ident.name);
  *tempstr = toupper (*tempstr);

  temp = root->astnode.ident.arraylist;

  if(temp->nodetype != EmptyArgList)
    for (; temp != NULL; temp = temp->nextstmt)
    {
                        
      if (*temp->astnode.ident.name != '*')
        expr_optimize (temp);
    }
}

/* 
 * All this will do is optimize a number if there is one.
 * Needs to be extended for arrays, etc.  Consider using
 * a switch/case structure for this.
 */

void
expr_optimize (AST * root)
{
  char *tempname;
  void name_optimize (AST *);

  if(root == NULL)
  {
    fprintf(stderr,"Warning: NULL root in expr_optimize\n");
    return;
  }

  switch (root->nodetype)
  {
    case Identifier:
      name_optimize (root);
      break;
    case Expression:

      if (root->astnode.expression.lhs != NULL)
        expr_optimize (root->astnode.expression.lhs);

      expr_optimize (root->astnode.expression.rhs);

      break;
    case Power:
      /* hack alert: */
      expr_optimize (root->astnode.expression.lhs);
      expr_optimize (root->astnode.expression.rhs);
      break;
    case Binaryop:
      expr_optimize (root->astnode.expression.lhs);
      expr_optimize (root->astnode.expression.rhs);
      break;
    case Unaryop:
      expr_optimize (root->astnode.expression.rhs);
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
       break;
    case Logicalop:
      /* 
       * Change all of this code to switch on the tokens.
       * The parser code will have to store the NOT token.
       */
      if (root->astnode.expression.lhs != NULL)
        expr_optimize (root->astnode.expression.lhs);
      expr_optimize (root->astnode.expression.rhs);
      break;
    case Relationalop:

      switch (root->token)
      {
        case rel_eq:

          if(((root->astnode.expression.lhs->vartype == String) ||
              (root->astnode.expression.lhs->vartype == Character)) &&
             ((root->astnode.expression.rhs->vartype == String) ||
              (root->astnode.expression.rhs->vartype == Character)))
          {
            expr_optimize (root->astnode.expression.lhs);
            expr_optimize (root->astnode.expression.rhs);
          }
          else
          {
            expr_optimize (root->astnode.expression.lhs);
            expr_optimize (root->astnode.expression.rhs);
          }
          break;
        case rel_ne:
          if(((root->astnode.expression.lhs->vartype == String) ||
              (root->astnode.expression.lhs->vartype == Character)) &&
             ((root->astnode.expression.rhs->vartype == String) ||
              (root->astnode.expression.rhs->vartype == Character)))
          {
            expr_optimize (root->astnode.expression.lhs);
            expr_optimize (root->astnode.expression.rhs);
          }
          else
          {
            expr_optimize (root->astnode.expression.lhs);
            expr_optimize (root->astnode.expression.rhs);
          }
          break;
        case rel_lt:
          expr_optimize (root->astnode.expression.lhs);
          expr_optimize (root->astnode.expression.rhs);
          break;
        case rel_le:
          expr_optimize (root->astnode.expression.lhs);
          expr_optimize (root->astnode.expression.rhs);
          break;
        case rel_gt:
          expr_optimize (root->astnode.expression.lhs);
          expr_optimize (root->astnode.expression.rhs);
          break;
        case rel_ge:
          expr_optimize (root->astnode.expression.lhs);
          expr_optimize (root->astnode.expression.rhs);
          break;
      }
      break;
    case Substring:
      expr_optimize(root->astnode.ident.arraylist);
      expr_optimize(root->astnode.ident.arraylist->nextstmt);
      break;
    default:
      fprintf(stderr,"Warning: Unknown nodetype in expr_optimize(): %s\n",
        print_nodetype(root));
  }
}


/*
 * This function generates code to implement the fortran DO loop.
 * naturally, we use Java's 'for' loop for this purpose.
 *
 * We also keep track of the nesting of for loops so that if we
 * encounter a goto statement within a loop, we can generate a
 * java 'break' or 'continue' statement.
 */

void
forloop_optimize (AST * root)
{
  char *indexname;
  int *tmp_int;
  void name_optimize (AST *);
  void assign_optimize (AST *);

  tmp_int = (int*)malloc(sizeof(int));

  if(!tmp_int) { perror("malloc"); exit(1); }

  *tmp_int = atoi(root->astnode.forloop.Label->astnode.constant.number);

   /*  
    *  Some point I will need to test whether this is really a name
    *  because it will crash if not.  
    */
  indexname = 
      root->astnode.forloop.start->astnode.assignment.lhs->astnode.ident.name;

  if(root->astnode.forloop.incr != NULL)
  {
    expr_optimize (root->astnode.forloop.incr);
  }

  assign_optimize (root->astnode.forloop.start);

  if(root->astnode.forloop.incr == NULL)
  {
    name_optimize(root->astnode.forloop.start->astnode.assignment.lhs);

    expr_optimize (root->astnode.forloop.stop);


    name_optimize(root->astnode.forloop.start->astnode.assignment.lhs);

  }
  else
  {
    name_optimize(root->astnode.forloop.start->astnode.assignment.lhs);
    expr_optimize (root->astnode.forloop.stop);
    name_optimize(root->astnode.forloop.start->astnode.assignment.lhs);
    expr_optimize (root->astnode.forloop.stop);
    
    name_optimize(root->astnode.forloop.start->astnode.assignment.lhs);
  }

   /*  Done with loop parameters.  */

}

void
logicalif_optimize (AST * root)
{
  if (root->astnode.logicalif.conds != NULL)
    expr_optimize (root->astnode.logicalif.conds);
  optimize (root->astnode.logicalif.stmts);
}

/*
 * This function generates labels.  We generate both a java label
 * and a call to the Dummy.label() method for goto translation.
 */

void
label_optimize (AST * root)
{
  int dl_int_examine(Dlist);

  if (root->astnode.label.stmt != NULL) {
    if (root->astnode.label.stmt->nodetype != Format) {
      optimize (root->astnode.label.stmt);
    }
  } 
}

void
read_optimize (AST * root)
{
  AST *temp;
  void read_implied_loop_optimize(AST *);

  if(root->astnode.io_stmt.arg_list == NULL) {
    return;
  }

  for(temp=root->astnode.io_stmt.arg_list;temp!=NULL;temp=temp->nextstmt)
  {
    if(temp->nodetype == ImpliedLoop)
      read_implied_loop_optimize(temp);
    else if(temp->nodetype == Identifier)
    {
      name_optimize(temp);
    }
    else
    {
      fprintf(stderr,"Read list must consist of idents or implied loops\n");
      fprintf(stderr,"   nodetype is %s\n", print_nodetype(temp));
      continue;
    }
  }

}

void
read_implied_loop_optimize(AST *node)
{

  expr_optimize(node->astnode.forloop.start);
  expr_optimize(node->astnode.forloop.stop);
  if(node->astnode.forloop.incr != NULL)
    expr_optimize(node->astnode.forloop.incr);

  if(node->astnode.forloop.Label->nodetype != Identifier) {
    fprintf(stderr,"Cant handle this nodetype (%s) ",
      print_nodetype(node->astnode.forloop.Label));
    fprintf(stderr," in implied loop (read stmt)\n");
  }
  else {
    name_optimize(node->astnode.forloop.Label);
  }
}

/*
 * This function generates the code which implements fortran's
 * block if.  This could also be a simulated while loop, which
 * is why we push this loop's number on the while_list.  This
 * way we can generate a java 'while' loop instead of the
 * simulated while loop using gotos.
 */

void
blockif_optimize (AST * root)
{
  AST *prev = root->prevstmt;
  AST *temp;
  int *tmp_int;
  void while_optimize(AST *);

  tmp_int = (int*)malloc(sizeof(int));

  if(!tmp_int) { perror("malloc"); exit(1); }

  /* if the previous node was a label, this could be a simulated
   * while loop.
   */
  if(prev != NULL)
    if(prev->nodetype == Label)
    {
      *tmp_int = root->prevstmt->astnode.label.number;

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
              while_optimize(root);
              return;
            }
        }

    }

  if (root->astnode.blockif.conds != NULL)
    expr_optimize (root->astnode.blockif.conds);

  optimize (root->astnode.blockif.stmts);

  if (root->astnode.blockif.elseifstmts != NULL)
    optimize (root->astnode.blockif.elseifstmts);

  if (root->astnode.blockif.elsestmts != NULL)
    optimize (root->astnode.blockif.elsestmts);
}

/* 
 *   while_optimize() is called when an if statement has been identified
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
while_optimize(AST *root)
{

  if (root->astnode.blockif.conds != NULL)
    expr_optimize (root->astnode.blockif.conds);
  optimize (root->astnode.blockif.stmts);

}

/* 
 * This function generates the code for the fortran 'else if'
 * construct.
 */

void
elseif_optimize (AST * root)
{
    if (root->astnode.blockif.conds != NULL)
	expr_optimize (root->astnode.blockif.conds);
    optimize (root->astnode.blockif.stmts);
}

/* 
 * This function generates the code for the fortran 'else'
 * construct.
 */

void
else_optimize (AST * root)
{
    optimize (root->astnode.blockif.stmts);
}

/* 
 *  This procedure implements Lapack and Blas type methods.
 *  They are translated to static method invocations.
 *  This is not a portable solution, it is specific to
 *  the Blas and Lapack. 
 */

void
call_optimize (AST * root)
{
  AST *temp;
  char *tempname;
  HASHNODE *hashtemp;
  HASHNODE *ht;
  HASHNODE *ht2;
  int needs_adapter(AST *);
  void insert_adapter(AST *);
  void insert_methcall(Dlist, AST *);
  SYMTABLE *opt_args_table = ASTroot->astnode.source.args_table;
  SYMTABLE *opt_array_table = ASTroot->astnode.source.type_table;

  assert (root != NULL);

  printf("@##@ in call_optimize, %s\n",root->astnode.ident.name);

  /* shouldn't be necessary to lowercase the name
   *   lowercase (root->astnode.ident.name);
   */

  tempname = strdup (root->astnode.ident.name);
  *tempname = toupper (*tempname);

  /* If this function was passed in as an argument, we call an
   * 'adapter' which performs the reflective method invocation..
   */

  if(type_lookup(opt_args_table, root->astnode.ident.name)) {
    printf("@@ calling passed-in func %s\n",root->astnode.ident.name);

    /* if this function has no args, we can simplify the calling
     * process by not creating an argument array or calling a
     * method adapter.
     */

    if((root->astnode.ident.arraylist->nodetype == EmptyArgList) ||
       (root->astnode.ident.arraylist == NULL)) {

      /* no args.  either function or subroutine. */

      return;
    }
    else if (root->nodetype == Call) {

      /* subroutine with args.  */

      int cnt = 0;

      for( temp = root->astnode.ident.arraylist; temp; temp = temp->nextstmt)
        cnt++;
      

      cnt = 0;
      for( temp = root->astnode.ident.arraylist; temp; temp = temp->nextstmt)
      {
        if(((temp->nodetype == Identifier) && (temp->astnode.ident.arraylist == NULL)) ||
            (temp->nodetype == Constant))
        {
          expr_optimize (temp);
        }
        else
        {
          expr_optimize (temp);
        }


        cnt++;
      }

      return;
    }
  }

  /* analyze this function call to determine if we need to generate an 
   * 'adapter' which will simulate passing array elements by reference.
   */

  else if( needs_adapter(root) )
  {
    printf("wow, guess we need an adapter for %s.\n", root->astnode.ident.name);
    insert_adapter(root);

    /* Assume all methods that are invoked are static.  */
  }

  if((root->astnode.ident.arraylist->nodetype == EmptyArgList) ||
     (root->astnode.ident.arraylist == NULL))
  {
    return;
  }



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
          && (ht=type_lookup(opt_array_table, temp->astnode.ident.name)) )
       {
         ht2 = type_lookup(opt_args_table, temp->astnode.ident.name);

         if(t2->astnode.ident.arraylist)     /* it is expecting an array */
         {
 
         }
         else                                /* it is not expecting an array */
         {

         }
       }
         /* 
          * else if the arg is an identifier AND
          *      it does not look like an array access AND
          *      it is in the array table
          */
       else if((temp->nodetype == Identifier) &&
               (temp->astnode.ident.arraylist == NULL) && 
               type_lookup(opt_array_table, temp->astnode.ident.name) )
       {
         if(t2->astnode.ident.arraylist)     /* it is expecting an array */
         {
           printf("expecting array\n");
           expr_optimize(temp);
         }
         else
         {
           printf("NOT expecting array\n");
         }
       }
       else if(
         ((temp->nodetype == Identifier) &&
          (temp->astnode.ident.arraylist == NULL) )
          || (temp->nodetype == Constant) )
       {
         expr_optimize(temp);
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

         expr_optimize(temp);

       }
       if(t2 != NULL)
         t2 = t2->nextstmt;
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
        expr_optimize (temp);
      }
      else
      {
        expr_optimize (temp);
      }

    }
  }

  /*  
   *  Problem here, depends on who called this procedure.
   *  When this is used by the CALL keyword, it works as
   *  written.  When used to create an external function call,
   *  it adds an extra ; and \n to the output.  Might be
   *  able to fix this by checking the nodetype. 
   */

}				/*  Close call_optimize().  */

/* 
 * This function generates the code for assignment statements.
 * If it looks like the lhs and rhs have different types, we
 * try to provide the appropriate cast, but in some cases the
 * resulting code may need to be modified slightly.
 */

void
assign_optimize (AST * root)
{
  enum returntype ltype, rtype;
  void name_optimize (AST *);

  ltype = root->astnode.assignment.lhs->vartype;
  rtype = root->astnode.assignment.rhs->vartype;

  /* handle lhs substring operations elsewhere */
  if(root->astnode.assignment.lhs->nodetype == Substring)
  {
    return;
  }

  name_optimize (root->astnode.assignment.lhs);

  if(ltype != rtype)
  {
    /* lhs and rhs have different types */

    if((ltype != String) && (ltype != Logical) && (rtype == String))
    {
      expr_optimize (root->astnode.assignment.rhs);
    }
    else if((ltype != String) && (ltype != Logical) && (rtype == Character))
    {
      expr_optimize (root->astnode.assignment.rhs);
    }
    else if( (ltype == Logical) && (rtype == String) )
    {
      expr_optimize (root->astnode.assignment.rhs);
    }
    else if( (ltype == Logical) && (rtype == Character) )
    {
      expr_optimize (root->astnode.assignment.rhs);
    }
    else if( (ltype == Logical) && (rtype != String) )
    {
      expr_optimize (root->astnode.assignment.rhs);
    }
    else
    {
      expr_optimize (root->astnode.assignment.rhs);
    }
  }
  else   /* lhs and rhs have same types, everything is cool */
    expr_optimize (root->astnode.assignment.rhs);
}
