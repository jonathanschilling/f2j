/*
 * $Source$
 * $Revision$
 * $Date$
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

int optdebug = FALSE;

char * strdup ( const char * );
char * print_nodetype ( AST * ); 
char * lowercase ( char * );
char * methodscan (METHODTAB * , char * );

/*
 * optimiziation
 */

void
optScalar(AST *root)
{
  AST *temp;
  HASHNODE *ht;
  void optimize (AST *, AST *);
  SYMTABLE *opt_type_table = root->astnode.source.type_table;

  ht = type_lookup(global_func_table, 
   root->astnode.source.progtype->astnode.source.name->astnode.ident.name);

  if(!ht) {
    fprintf(stderr,"optScalar: Cant find %s in global function table\n",
     root->astnode.source.progtype->astnode.source.name->astnode.ident.name);
    return;
  }

  printf("attempting to optimize %s\n",
   root->astnode.source.progtype->astnode.source.name->astnode.ident.name);

  if(ht->variable->astnode.source.scalarOptStatus == NOT_VISITED)
    printf("%s has not been visited yet\n",
      root->astnode.source.progtype->astnode.source.name->astnode.ident.name);
  else if(ht->variable->astnode.source.scalarOptStatus == VISITED)
    printf("%s has been visited but not finished\n",
      root->astnode.source.progtype->astnode.source.name->astnode.ident.name);
  else if(ht->variable->astnode.source.scalarOptStatus == FINISHED)
    printf("%s has been finished\n",
      root->astnode.source.progtype->astnode.source.name->astnode.ident.name);
  else
    printf("%s has an invalid status field\n",
      root->astnode.source.progtype->astnode.source.name->astnode.ident.name);

  if(ht->variable->astnode.source.scalarOptStatus == NOT_VISITED) {
    ht->variable->astnode.source.scalarOptStatus = VISITED;
    optimize(root, root);
  }
  
  ht->variable->astnode.source.scalarOptStatus = FINISHED;

  temp = root->astnode.source.progtype->astnode.source.args;
  for(;temp != NULL;temp = temp->nextstmt)
    if((ht = type_lookup(opt_type_table,temp->astnode.ident.name)) != NULL)
      if(ht->variable->astnode.ident.passByRef)
        temp->astnode.ident.passByRef = TRUE;
}

void
optimize (AST * root, AST * rptr)
{
  void assign_optimize(AST *, AST*);
  void call_optimize(AST *, AST*);
  void forloop_optimize(AST *, AST*);
  void blockif_optimize(AST *, AST*);
  void elseif_optimize(AST *, AST*);
  void else_optimize(AST *, AST*);
  void logicalif_optimize(AST *, AST*);
  void read_optimize(AST *, AST*);
  void write_optimize(AST *, AST*);
  void spec_optimize(AST *, AST*);

  switch (root->nodetype)
  {
    case 0:
      if (optdebug)
        printf ("Bad node\n");

      optimize (root->nextstmt, rptr);
    case Progunit:
      if (optdebug)
        printf ("Source.\n");

      optimize(root->astnode.source.typedecs, rptr);
      optimize(root->astnode.source.progtype, rptr);
      optimize(root->astnode.source.statements, rptr);

      break;
    case Subroutine:
    case Function:
    case Program:
      if (optdebug)
        printf ("Unit name: %s\n", 
          root->astnode.source.name->astnode.ident.name);
      break;
    case Assignment:
      if (optdebug)
        printf ("Assignment.\n");
 
      assign_optimize (root, rptr);

      if (root->nextstmt != NULL)
        optimize (root->nextstmt, rptr);
      break;
    case Call:
      if (optdebug)
        printf ("Call.\n");

      call_optimize (root, rptr);

      if (root->nextstmt != NULL)	/* End of typestmt list. */
        optimize (root->nextstmt, rptr);
      break;
    case Forloop:
      if (optdebug)
        printf ("Forloop.\n");

      forloop_optimize (root, rptr);

      if (root->nextstmt != NULL)	/* End of typestmt list. */
        optimize (root->nextstmt, rptr);
      break;
    case Blockif:
      if (optdebug)
        printf ("Blockif.\n");

      blockif_optimize (root, rptr);

      if (root->nextstmt != NULL)	/* End of typestmt list. */
        optimize (root->nextstmt, rptr);
      break;
    case Elseif:
      if (optdebug)
        printf ("Elseif.\n");

      elseif_optimize (root, rptr);

      if (root->nextstmt != NULL)	/* End of typestmt list. */
        optimize (root->nextstmt, rptr);
      break;
    case Else:
      if (optdebug)
        printf ("Else.\n");

      else_optimize (root, rptr);

      if (root->nextstmt != NULL)	/* End of typestmt list. */
        optimize (root->nextstmt, rptr);
      break;
    case Logicalif:
      if (optdebug)
        printf ("Logicalif.\n");

      logicalif_optimize (root, rptr);

      if (root->nextstmt != NULL)	/* End of typestmt list. */
        optimize (root->nextstmt, rptr);
      break;
    case Label:
      if (optdebug)
        printf ("Label.\n");

      if((root->astnode.label.stmt != NULL) &&
         (root->astnode.label.stmt->nodetype != Format))
            optimize(root->astnode.label.stmt, rptr);

      if (root->nextstmt != NULL)	/* End of typestmt list. */
        optimize (root->nextstmt, rptr);
      break;
    case Write:
      if (optdebug)
        printf ("Write statement.\n");

      write_optimize(root, rptr);

      if (root->nextstmt != NULL)
        optimize (root->nextstmt, rptr);
      break;
    case Read:
      if (optdebug)
        printf ("Read statement.\n");

      read_optimize (root, rptr);

      if (root->nextstmt != NULL)
        optimize (root->nextstmt, rptr);
      break;
    case Format:
    case Stop:
    case Save:
    case Common:
    case ComputedGoto:
    case Goto:
    case Return:
    case Statement:
    case Comment:
    case DataList:
    case Equivalence:
    case Typedec:
    case Unimplemented:
      if (root->nextstmt != NULL)
        optimize (root->nextstmt, rptr);
      break;
    case Specification:
      spec_optimize(root, rptr);
      if (root->nextstmt != NULL)
        optimize (root->nextstmt, rptr);
      break;
    case End:
      break;
    case Constant:
    default:
      fprintf(stderr,"optimize(): Error, bad nodetype (%s)\n",
         print_nodetype(root));
    }				/* switch on nodetype.  */
}

void
spec_optimize(AST *root, AST *rptr)
{
  SYMTABLE *opt_external_table = rptr->astnode.source.external_table;
  AST *temp;
  HASHNODE *ht, *ht2;
  void name_emit(AST *);

  switch (root->astnode.typeunit.specification)
  {
    case Parameter:
    case Implicit:
      break;
    case Intrinsic:
      name_emit (root);
      break;
    case External:
      temp = root->astnode.typeunit.declist;
      for(;temp != NULL;temp = temp->nextstmt) {

        printf("external %s\n", temp->astnode.ident.name);
        ht= type_lookup(opt_external_table,temp->astnode.ident.name);
        if(ht)
        {
          printf("going to optimize external %s\n",temp->astnode.ident.name);

          ht2 = type_lookup(global_func_table,temp->astnode.ident.name);
          if(!ht2) {
            fprintf(stderr,"Cant locate %s, not optimizing.\n",
               temp->astnode.ident.name);
            continue;
          }

          optScalar(ht2->variable);
        }
      }
      break;
  }
}

void
external_optimize(AST *root, AST *rptr)
{
  extern METHODTAB intrinsic_toks[];
  char *tempname, *javaname;
  void call_optimize(AST *, AST *);

  if(optdebug) {
    printf("here we are in external_optimize\n");
    printf("nodetype = %s, parent nodetype = %s\n",
      print_nodetype(root),print_nodetype(root->parent));
  }

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
      call_optimize (root, rptr);

    return;
  }

}

void
name_optimize (AST * root, AST *rptr)
{
  HASHNODE *hashtemp;
  char * tempname;
  extern METHODTAB intrinsic_toks[];
  SYMTABLE *opt_external_table = rptr->astnode.source.external_table;
  SYMTABLE *opt_intrinsic_table = rptr->astnode.source.intrinsic_table;
  SYMTABLE *opt_type_table = rptr->astnode.source.type_table;
  SYMTABLE *opt_array_table = rptr->astnode.source.array_table;

  void subcall_optimize(AST *, AST *);

printf("here in name_optimize... %s\n",print_nodetype(root));
if(root->nodetype == Identifier)
  printf("name is %s\n",root->astnode.ident.name);

  /*  
   *  Check to see whether name is in external table.  Names are
   *  loaded into the external table from the parser.   
   */

  tempname = strdup(root->astnode.ident.name);
  uppercase(tempname);

  /* 
   * If the name is in the external table, then check to see if
   * it is an intrinsic function instead (e.g. SQRT, ABS, etc).  
   */

  if (type_lookup (opt_external_table, root->astnode.ident.name) != NULL)
  {
printf("going to external_optimize\n");
    external_optimize(root, rptr);
  }
  else if(( methodscan (intrinsic_toks, tempname) != NULL) 
     && ( (type_lookup(opt_intrinsic_table, root->astnode.ident.name) != NULL)
       || (type_lookup(opt_type_table, root->astnode.ident.name) == NULL)))
  {
printf("looks like an intrinsic\n");
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
        {
printf("going to subcall_optimize\n");
          subcall_optimize(root, rptr);
        }
        break;
    }
}

/*  This function optimize a function call.  I think this function
 * is only called in cases where the function or subroutine is
 * not declared external or intrinsic and we dont know what
 * else to do with it.
 */

void 
subcall_optimize(AST *root, AST *rptr)
{
  AST *temp;
  char *tempstr;
  void expr_optimize (AST *, AST *);

  tempstr = strdup (root->astnode.ident.name);
  *tempstr = toupper (*tempstr);

  temp = root->astnode.ident.arraylist;

  if(temp->nodetype != EmptyArgList)
    for (; temp != NULL; temp = temp->nextstmt)
    {
                        
      if (*temp->astnode.ident.name != '*')
        expr_optimize (temp, rptr);
    }
}

/* 
 * All this will do is optimize a number if there is one.
 * Needs to be extended for arrays, etc.  Consider using
 * a switch/case structure for this.
 */

void
expr_optimize (AST * root, AST *rptr)
{
  char *tempname;
  void name_optimize (AST *, AST *);

  if(root == NULL)
  {
    fprintf(stderr,"Warning: NULL root in expr_optimize\n");
    return;
  }

  switch (root->nodetype)
  {
    case Identifier:
      name_optimize (root, rptr);
      break;
    case Expression:

      if (root->astnode.expression.lhs != NULL)
        expr_optimize (root->astnode.expression.lhs, rptr);

      expr_optimize (root->astnode.expression.rhs, rptr);

      break;
    case Power:
      /* hack alert: */
      expr_optimize (root->astnode.expression.lhs, rptr);
      expr_optimize (root->astnode.expression.rhs, rptr);
      break;
    case Binaryop:
      expr_optimize (root->astnode.expression.lhs, rptr);
      expr_optimize (root->astnode.expression.rhs, rptr);
      break;
    case Unaryop:
      expr_optimize (root->astnode.expression.rhs, rptr);
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
        expr_optimize (root->astnode.expression.lhs, rptr);
      expr_optimize (root->astnode.expression.rhs, rptr);
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
            expr_optimize (root->astnode.expression.lhs, rptr);
            expr_optimize (root->astnode.expression.rhs, rptr);
          }
          else
          {
            expr_optimize (root->astnode.expression.lhs, rptr);
            expr_optimize (root->astnode.expression.rhs, rptr);
          }
          break;
        case rel_ne:
          if(((root->astnode.expression.lhs->vartype == String) ||
              (root->astnode.expression.lhs->vartype == Character)) &&
             ((root->astnode.expression.rhs->vartype == String) ||
              (root->astnode.expression.rhs->vartype == Character)))
          {
            expr_optimize (root->astnode.expression.lhs, rptr);
            expr_optimize (root->astnode.expression.rhs, rptr);
          }
          else
          {
            expr_optimize (root->astnode.expression.lhs, rptr);
            expr_optimize (root->astnode.expression.rhs, rptr);
          }
          break;
        case rel_lt:
          expr_optimize (root->astnode.expression.lhs, rptr);
          expr_optimize (root->astnode.expression.rhs, rptr);
          break;
        case rel_le:
          expr_optimize (root->astnode.expression.lhs, rptr);
          expr_optimize (root->astnode.expression.rhs, rptr);
          break;
        case rel_gt:
          expr_optimize (root->astnode.expression.lhs, rptr);
          expr_optimize (root->astnode.expression.rhs, rptr);
          break;
        case rel_ge:
          expr_optimize (root->astnode.expression.lhs, rptr);
          expr_optimize (root->astnode.expression.rhs, rptr);
          break;
      }
      break;
    case Substring:
      expr_optimize(root->astnode.ident.arraylist, rptr);
      expr_optimize(root->astnode.ident.arraylist->nextstmt, rptr);
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
forloop_optimize (AST * root, AST *rptr)
{
  char *indexname;
  int *tmp_int;
  void name_optimize (AST *, AST *);
  void assign_optimize (AST *, AST *);

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
    expr_optimize (root->astnode.forloop.incr, rptr);
  }

  assign_optimize (root->astnode.forloop.start, rptr);

  if(root->astnode.forloop.incr == NULL)
  {
    name_optimize(root->astnode.forloop.start->astnode.assignment.lhs, rptr);

    expr_optimize (root->astnode.forloop.stop, rptr);


    name_optimize(root->astnode.forloop.start->astnode.assignment.lhs, rptr);

  }
  else
  {
    name_optimize(root->astnode.forloop.start->astnode.assignment.lhs, rptr);
    expr_optimize (root->astnode.forloop.stop, rptr);
    name_optimize(root->astnode.forloop.start->astnode.assignment.lhs, rptr);
    expr_optimize (root->astnode.forloop.stop, rptr);
    
    name_optimize(root->astnode.forloop.start->astnode.assignment.lhs, rptr);
  }

   /*  Done with loop parameters.  */

}

void
logicalif_optimize (AST * root, AST *rptr)
{
  if (root->astnode.logicalif.conds != NULL)
    expr_optimize (root->astnode.logicalif.conds, rptr);
  optimize (root->astnode.logicalif.stmts, rptr);
}

void
write_optimize (AST * root, AST *rptr)
{
  AST *temp;
  void expr_optimize(AST *, AST *);

  for(temp = root->astnode.io_stmt.arg_list; temp!=NULL;temp=temp->nextstmt)
    if(temp->nodetype != ImpliedLoop)
      expr_optimize(temp, rptr);
}

void
read_optimize (AST * root, AST *rptr)
{
  SYMTABLE *opt_args_table = rptr->astnode.source.args_table;
  SYMTABLE *opt_type_table = rptr->astnode.source.type_table;
  HASHNODE *ht;
  void read_implied_loop_optimize(AST *, AST *);
  AST *temp;

  if(root->astnode.io_stmt.arg_list == NULL) {
    return;
  }

  for(temp=root->astnode.io_stmt.arg_list;temp!=NULL;temp=temp->nextstmt)
  {
    if(temp->nodetype == ImpliedLoop)
      read_implied_loop_optimize(temp, rptr);
    else if(temp->nodetype == Identifier)
    {
      name_optimize(temp, rptr);

      ht = type_lookup(opt_type_table,temp->astnode.ident.name);
      if(ht) {
        if(type_lookup(opt_args_table, temp->astnode.ident.name) != NULL)
          ht->variable->astnode.ident.passByRef = TRUE;
      }
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
read_implied_loop_optimize(AST *node, AST *rptr)
{

  expr_optimize(node->astnode.forloop.start, rptr);
  expr_optimize(node->astnode.forloop.stop, rptr);
  if(node->astnode.forloop.incr != NULL)
    expr_optimize(node->astnode.forloop.incr, rptr);

  if(node->astnode.forloop.Label->nodetype != Identifier) {
    fprintf(stderr,"Cant handle this nodetype (%s) ",
      print_nodetype(node->astnode.forloop.Label));
    fprintf(stderr," in implied loop (read stmt)\n");
  }
  else {
    name_optimize(node->astnode.forloop.Label, rptr);
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
blockif_optimize (AST * root, AST *rptr)
{
  AST *prev = root->prevstmt;
  AST *temp;
  int *tmp_int;
  void while_optimize(AST *, AST *);

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
              while_optimize(root, rptr);
              return;
            }
        }

    }

  if (root->astnode.blockif.conds != NULL)
    expr_optimize (root->astnode.blockif.conds, rptr);

  optimize (root->astnode.blockif.stmts, rptr);

  if (root->astnode.blockif.elseifstmts != NULL)
    optimize (root->astnode.blockif.elseifstmts, rptr);

  if (root->astnode.blockif.elsestmts != NULL)
    optimize (root->astnode.blockif.elsestmts, rptr);
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
while_optimize(AST *root, AST *rptr)
{

  if (root->astnode.blockif.conds != NULL)
    expr_optimize (root->astnode.blockif.conds, rptr);
  optimize (root->astnode.blockif.stmts, rptr);

}

/* 
 * This function generates the code for the fortran 'else if'
 * construct.
 */

void
elseif_optimize (AST * root, AST *rptr)
{
    if (root->astnode.blockif.conds != NULL)
	expr_optimize (root->astnode.blockif.conds, rptr);
    optimize (root->astnode.blockif.stmts, rptr);
}

/* 
 * This function generates the code for the fortran 'else'
 * construct.
 */

void
else_optimize (AST * root, AST *rptr)
{
    optimize (root->astnode.blockif.stmts, rptr);
}

/* 
 *  This procedure implements Lapack and Blas type methods.
 *  They are translated to static method invocations.
 *  This is not a portable solution, it is specific to
 *  the Blas and Lapack. 
 */

void
call_optimize (AST * root, AST *rptr)
{
  AST *temp, *temp2;
  char *tempname;
  HASHNODE *hashtemp;
  HASHNODE *ht, *ht2, *ht3;
  SYMTABLE *opt_args_table = rptr->astnode.source.args_table;
  SYMTABLE *opt_array_table = rptr->astnode.source.array_table;
  SYMTABLE *opt_type_table = rptr->astnode.source.type_table;
  SYMTABLE *opt_common_table = rptr->astnode.source.common_table;
  void expr_optimize(AST *, AST *);
  int cnt;

printf("enter call_optimize\n");

  assert (root != NULL);

  tempname = strdup (root->astnode.ident.name);
  *tempname = toupper (*tempname);

  /* If this function was passed in as an argument, we call an
   * 'adapter' which performs the reflective method invocation..
   */

  if(type_lookup(opt_args_table, root->astnode.ident.name)) {

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

      for( temp = root->astnode.ident.arraylist; temp; temp = temp->nextstmt)
        expr_optimize (temp, rptr);

      return;
    }
  }

  if(root->astnode.ident.arraylist->nodetype == EmptyArgList)
    return;

  /* look up the function name so that we may compare the parameters */

printf("looking up %s in the global function table\n",root->astnode.ident.name);
  if((hashtemp=type_lookup(global_func_table, root->astnode.ident.name)) != NULL)
  {
    AST *t2;

    temp = root->astnode.ident.arraylist;
    t2=hashtemp->variable->astnode.source.progtype->astnode.source.args;

    for( ; temp != NULL; temp = temp->nextstmt)
    {
       expr_optimize(temp, rptr);

       /*  if this node is an identifier AND
        *      it does not look like an array AND
        *      it is not in the array table
        */

       if((temp->nodetype == Identifier) && 
          (temp->astnode.ident.arraylist == NULL) && 
          (type_lookup(opt_array_table,temp->astnode.ident.name)==NULL))
       {
         /* now we check whether the function/subroutine expects this 
          * to be passed by reference.
          */

         if(t2->astnode.ident.passByRef) {
           ht = type_lookup(opt_type_table,temp->astnode.ident.name);
           if(ht) {
             ht->variable->astnode.ident.passByRef = TRUE;

             ht2 = type_lookup(opt_common_table,temp->astnode.ident.name);
             if(ht2) {
               ht3 = type_lookup(global_common_table,
                       ht2->variable->astnode.ident.commonBlockName);
   
               if(ht3) {
 
                 temp2 = ht3->variable->astnode.common.nlist;
                 cnt = 0;

                 while((cnt < ht2->variable->astnode.ident.position) &&
                       (temp2 != NULL))
                 {
                   cnt++;
                   temp2 = temp2->nextstmt;
                 }

                 if(temp2 != NULL) {
                   temp2->astnode.ident.passByRef = TRUE;
                 }
                 else {
                   fprintf(stderr, "optimize(): Common block length ");
                   fprintf(stderr, "does not match position of ident\n");
                 }
               }
               else {
                 fprintf(stderr,"Cant find common block %s\n", 
                   ht2->variable->astnode.ident.commonBlockName);
               }
             }
           }
         }
       }

       if(t2 != NULL)
         t2 = t2->nextstmt;
    }
  }
  else
  {
    printf("call_optimize(): %s not found in global function table.\n",
      root->astnode.ident.name);

    temp = root->astnode.ident.arraylist;

    for( ; temp != NULL; temp = temp->nextstmt)
      expr_optimize (temp, rptr);
  }
}

/* 
 * This function generates the code for assignment statements.
 * If it looks like the lhs and rhs have different types, we
 * try to provide the appropriate cast, but in some cases the
 * resulting code may need to be modified slightly.
 */

void
assign_optimize (AST * root, AST *rptr)
{
  SYMTABLE *opt_args_table = rptr->astnode.source.args_table;
  SYMTABLE *opt_type_table = rptr->astnode.source.type_table;
  enum returntype ltype, rtype;
  void name_optimize (AST *, AST *);
  HASHNODE *ht;

  ltype = root->astnode.assignment.lhs->vartype;
  rtype = root->astnode.assignment.rhs->vartype;

  /* handle lhs substring operations elsewhere */
  if(root->astnode.assignment.lhs->nodetype == Substring)
  {
    return;
  }

  name_optimize (root->astnode.assignment.lhs, rptr);
  
  ht = type_lookup(opt_type_table,root->astnode.assignment.lhs->astnode.ident.name);
  if(ht) {
    if(type_lookup(opt_args_table, 
          root->astnode.assignment.lhs->astnode.ident.name) != NULL)
      ht->variable->astnode.ident.passByRef = TRUE;
  }
  else
    printf("Can't find lhs of assignment: %s\n", 
       root->astnode.assignment.lhs->astnode.ident.name);

  expr_optimize (root->astnode.assignment.rhs, rptr);
}
