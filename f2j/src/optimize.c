/*
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */

/*****************************************************************************
 * optimize.c                                                                *
 *                                                                           *
 * Determines which scalars really need to be wrapped in objects             *
 * for emulation of pass-by-reference.  For the most part, this file         *
 * mimics codegen.c since we must traverse the AST in the same way           *
 * for both operations.                                                      *
 *                                                                           *
 * Basically, all we're doing here is trying to determine which variables    *
 * are modified within this function (and functions called from this one).   *
 * So, we are looking for three cases:                                       *
 *                                                                           *
 *   1.  the variable is an argument to this function and it is on the LHS   *
 *         of an assignment                                                  *
 *   2.  the variable is an argument to this function and it is an argument  *
 *         to a READ statement                                               *
 *   3.  the variable is passed to a function/subroutine that modifies it    *
 *                                                                           *
 * If any of the three cases are met, we classify the variable as a          *
 * 'pass by reference' variable, meaning that it must be wrapped in an       *
 * object.                                                                   *
 *                                                                           *
 *****************************************************************************/

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>
#include"f2j.h"
#include"f2jparse.tab.h"

/*****************************************************************************
 * Set optdebug to TRUE to get debugging output from the optimization        *
 * routines.                                                                 *
 *****************************************************************************/

int optdebug = FALSE;

/*****************************************************************************
 * Function prototypes:                                                      *
 *****************************************************************************/

char 
  * strdup ( const char * ),
  * print_nodetype ( AST * ),
  * lowercase ( char * ),
  * methodscan (METHODTAB * , char * );

/*****************************************************************************
 *                                                                           *
 * optScalar                                                                 *
 *                                                                           *
 * This is the main entry point for the optimization routines.  Here we look *
 * up the current function name to determine whether it has been optimized   *
 * yet.  If so, skip it - otherwise, optimize.                               *
 *                                                                           *
 *****************************************************************************/

void
optScalar(AST *root)
{
  AST *temp;
  HASHNODE *ht;
  void optimize (AST *, AST *);
  SYMTABLE *opt_type_table = root->astnode.source.type_table;

  /* look up this function name */

  ht = type_lookup(global_func_table, 
   root->astnode.source.progtype->astnode.source.name->astnode.ident.name);

  if(!ht) {
    fprintf(stderr,"optScalar: Cant find %s in global function table\n",
     root->astnode.source.progtype->astnode.source.name->astnode.ident.name);
    return;
  }

  if(optdebug) {
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
  }

  /* if this function hasn't been visited yet, set the status to 'VISITED'
   * and start optimizing it.
   */

  if(ht->variable->astnode.source.scalarOptStatus == NOT_VISITED) {
    ht->variable->astnode.source.scalarOptStatus = VISITED;
    optimize(root, root);
  }
  
  /* afterwards, make sure the status is set to 'FINISHED' */

  ht->variable->astnode.source.scalarOptStatus = FINISHED;

  /* for each argument in the function, set its passByRef field from
   * the values in the symbol table.  This saves some time later on
   * when we want to know which arguments are pass by reference and
   * which aren't.  we wont have to do symbol table lookups - just
   * loop through each arg in the function.
   */

  temp = root->astnode.source.progtype->astnode.source.args;

  for(;temp != NULL;temp = temp->nextstmt)
    if((ht = type_lookup(opt_type_table,temp->astnode.ident.name)) != NULL)
      if(ht->variable->astnode.ident.passByRef)
        temp->astnode.ident.passByRef = TRUE;
}

/*****************************************************************************
 *                                                                           *
 * optimize                                                                  *
 *                                                                           *
 * This is the main optimization routine.  It just determines what kind of   *
 * node we're looking at and calls the appropriate function to handle it.    *
 *                                                                           *
 *****************************************************************************/

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
        fprintf (stderr,"Bad node\n");

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
    case CommonList:
    case ComputedGoto:
    case Goto:
    case Return:
    case Statement:
    case Comment:
    case MainComment:
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

/*****************************************************************************
 *                                                                           *
 * spec_optimize                                                             *
 *                                                                           *
 * The only Specification we really care about here is the EXTERNAL          *
 * declaration.  For each function declared external, we attempt to          *
 * optimize that function.  This way we can be assured that when we're       *
 * optimizing the executable code for this function, we already know         *
 * which args to each function must be passed by reference.                  *
 *                                                                           *
 *****************************************************************************/

void
spec_optimize(AST *root, AST *rptr)
{
  SYMTABLE *opt_external_table = rptr->astnode.source.external_table;
  AST *temp;
  HASHNODE *ht, *ht2;
  void name_optimize(AST *, AST *);

  switch (root->astnode.typeunit.specification)
  {
    case Parameter:
    case Implicit:
      break;
    case Intrinsic:
      /* name_optimize will ignore Intrinsics */
      name_optimize (root,rptr);
      break;
    case External:
      temp = root->astnode.typeunit.declist;
      for(;temp != NULL;temp = temp->nextstmt) {

        if(optdebug)
          printf("external %s\n", temp->astnode.ident.name);

        ht= type_lookup(opt_external_table,temp->astnode.ident.name);
        if(ht)
        {
          if(optdebug)
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

/*****************************************************************************
 *                                                                           *
 * external_optimize                                                         *
 *                                                                           *
 * This function is called when we're looking at a name that is declared     *
 * EXTERNAL.  Normally, this corresponds to a function/subroutine call.      *
 *                                                                           *
 *****************************************************************************/

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

  /* First, make sure this isn't some intrinsic function. */

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

/*****************************************************************************
 *                                                                           *
 * name_optimize                                                             *
 *                                                                           *
 * Surprisingly, we dont do much here in name_optimze.  If the name looks    *
 * like an EXTERNAL, call external_optimize.  If it looks like a call of     *
 * some sort, but we didn't find it in the external or intrinsic tables,     *
 * call subcall_optimize.                                                    *
 *                                                                           *
 *****************************************************************************/

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

  if(optdebug) {
    printf("here in name_optimize... %s\n",print_nodetype(root));
    if(root->nodetype == Identifier)
      printf("name is %s\n",root->astnode.ident.name);
  }

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
    if(optdebug)
      printf("going to external_optimize\n");
    external_optimize(root, rptr);
  }
  else if(( methodscan (intrinsic_toks, tempname) != NULL) 
     && ( (type_lookup(opt_intrinsic_table, root->astnode.ident.name) != NULL)
       || (type_lookup(opt_type_table, root->astnode.ident.name) == NULL)))
  {
    if(optdebug)
      printf("looks like an intrinsic\n");
  }
  else
    switch (root->token)
    {
      case STRING:
      case CHAR:
      case INTRINSIC: 
        /* do nothing */
        break;
      case NAME:
      default:
        /* we only care if this looks like a subcall */

        hashtemp = type_lookup (opt_array_table, root->astnode.ident.name);

        if (root->astnode.ident.arraylist == NULL)
        {
          /* dont care */
        }
        else if (hashtemp != NULL)
        {
          /* dont care */
        }
        else
        {
          if(optdebug)
            printf("going to subcall_optimize\n");

          subcall_optimize(root, rptr);
        }
        break;
    }
}

/*****************************************************************************
 *                                                                           *
 * subcall_optimize                                                          *
 *                                                                           *
 *  This function optimize a function call.  I think this function           *
 * is only called in cases where the function or subroutine is               *
 * not declared external or intrinsic and we dont know what                  *
 * else to do with it.                                                       *
 *                                                                           *
 *****************************************************************************/

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

/*****************************************************************************
 *                                                                           *
 * expr_optimize                                                             *
 *                                                                           *
 * All this will do is optimize an expression.                               *
 * Needs to be extended for arrays, etc.  Consider using                     *
 * a switch/case structure for this.                                         *
 *                                                                           *
 *****************************************************************************/

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
    case Expression:
    case Logicalop:
      if (root->astnode.expression.lhs != NULL)
        expr_optimize (root->astnode.expression.lhs, rptr);
      expr_optimize (root->astnode.expression.rhs, rptr);
      break;
    case Power:
    case Binaryop:
    case Relationalop:

      expr_optimize (root->astnode.expression.lhs, rptr);
      expr_optimize (root->astnode.expression.rhs, rptr);
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


/*****************************************************************************
 *                                                                           *
 * forloop_optimize                                                          *
 *                                                                           *
 * This function traverses forloops.  Nothing much happens here.             *
 *                                                                           *
 *****************************************************************************/

void
forloop_optimize (AST * root, AST *rptr)
{
  char *indexname;
  int *tmp_int;
  void name_optimize (AST *, AST *);
  void assign_optimize (AST *, AST *);

  tmp_int = (int*)f2jalloc(sizeof(int));

  *tmp_int = atoi(root->astnode.forloop.Label->astnode.constant.number);

   /*  
    *  Some point I will need to test whether this is really a name
    *  because it will crash if not.  
    */

  indexname = 
      root->astnode.forloop.start->astnode.assignment.lhs->astnode.ident.name;

  if(root->astnode.forloop.incr != NULL)
    expr_optimize (root->astnode.forloop.incr, rptr);

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
}

/*****************************************************************************
 *                                                                           *
 * logicalif_optimize                                                        *
 *                                                                           *
 * Optimize a logical if statement.                                          *
 *                                                                           *
 *****************************************************************************/

void
logicalif_optimize (AST * root, AST *rptr)
{
  if (root->astnode.logicalif.conds != NULL)
    expr_optimize (root->astnode.logicalif.conds, rptr);
  optimize (root->astnode.logicalif.stmts, rptr);
}

/*****************************************************************************
 *                                                                           *
 * write_optimize                                                            *
 *                                                                           *
 * Optimize a WRITE statement.                                               *
 *                                                                           *
 *****************************************************************************/

void
write_optimize (AST * root, AST *rptr)
{
  AST *temp;
  void expr_optimize(AST *, AST *);

  for(temp = root->astnode.io_stmt.arg_list; temp!=NULL;temp=temp->nextstmt)
    if(temp->nodetype != IoImpliedLoop)
      expr_optimize(temp, rptr);
}

/*****************************************************************************
 *                                                                           *
 * read_optimize                                                             *
 *                                                                           *
 * Optimize a READ statement.  Here we examine each argument of the READ     *
 * statement to determine whether it's an argument to the current function.  *
 * If so, we must mark it as pass by reference.
 *                                                                           *
 *****************************************************************************/

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

  /* for each arg... */
  for(temp=root->astnode.io_stmt.arg_list;temp!=NULL;temp=temp->nextstmt)
  {
    if(temp->nodetype == IoImpliedLoop)
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

/*****************************************************************************
 *                                                                           *
 * read_implied_loop_optimize                                                *
 *                                                                           *
 * We're looking at an implied loop in a READ statement.  The only time we   *
 * care about arrays being 'pass by reference' is when we're generating      *
 * the front-end inteerface.  In that case, we use the passByRef field       *
 * to determine which arrays must be copied back after the call.             *
 *                                                                           *
 *****************************************************************************/

void
read_implied_loop_optimize(AST *node, AST *rptr)
{

  /* NOTE: we need to set the passByRef field of the array somewhere in here */

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

/*****************************************************************************
 *                                                                           *
 * blockif_optimize                                                          *
 *                                                                           *
 * Here we have a block IF statement.  We should optimize the expression     *
 * and the statements.
 *                                                                           *
 *****************************************************************************/

void
blockif_optimize (AST * root, AST *rptr)
{
  AST *prev = root->prevstmt;
  AST *temp;
  int *tmp_int;
  void while_optimize(AST *, AST *);

  /* This function could probably be simplified by getting rid of all the
   * while detection code.  It isn't really necessary here.
   */

  tmp_int = (int*)f2jalloc(sizeof(int));

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

/*****************************************************************************
 *                                                                           *
 * while_optimize                                                            *
 *                                                                           *
 * while_optimize() is called when an if statement has been identified       *
 * as a simulated while loop.   This could probably be inlined into the      *
 * block if routine.                                                         *
 *                                                                           *
 *****************************************************************************/

void 
while_optimize(AST *root, AST *rptr)
{

  if (root->astnode.blockif.conds != NULL)
    expr_optimize (root->astnode.blockif.conds, rptr);
  optimize (root->astnode.blockif.stmts, rptr);

}

/*****************************************************************************
 *                                                                           *
 * elseif_optimize                                                           *
 *                                                                           *
 * Nothing special here.  we examine the elseif portion of a block if.       *
 *                                                                           *
 *****************************************************************************/

void
elseif_optimize (AST * root, AST *rptr)
{
  if (root->astnode.blockif.conds != NULL)
    expr_optimize (root->astnode.blockif.conds, rptr);
  optimize (root->astnode.blockif.stmts, rptr);
}

/*****************************************************************************
 *                                                                           *
 * else_optimize                                                             *
 *                                                                           *
 * Here we examine the else portion of a block if.                           *
 *                                                                           *
 *****************************************************************************/

void
else_optimize (AST * root, AST *rptr)
{
    optimize (root->astnode.blockif.stmts, rptr);
}

/*****************************************************************************
 *                                                                           *
 * call_optimize                                                             *
 *                                                                           *
 * Handles external calls.  What we really want to know is whether any of    *
 * the arguments to the function we're calling are passed by reference.      *
 * If so, we must wrap the corresponding variable in this function.          *
 *                                                                           *
 *****************************************************************************/

void
call_optimize (AST * root, AST *rptr)
{
  AST *temp, *temp2;
  char *tempname;
  HASHNODE *hashtemp;
  HASHNODE *ht, *ht2, *ht3;
  SYMTABLE *opt_args_table = rptr->astnode.source.args_table;
  SYMTABLE *opt_type_table = rptr->astnode.source.type_table;
  SYMTABLE *opt_common_table = rptr->astnode.source.common_table;
  void expr_optimize(AST *, AST *);
  int cnt;

  if(optdebug)
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

  if(optdebug)
    printf("looking up %s in the global func table\n",root->astnode.ident.name);

  if((hashtemp=type_lookup(global_func_table, root->astnode.ident.name)) != NULL)
  {
    AST *t2;

    temp = root->astnode.ident.arraylist;
    t2=hashtemp->variable->astnode.source.progtype->astnode.source.args;

    for( ; temp != NULL; temp = temp->nextstmt)
    {
       expr_optimize(temp, rptr);

       if(temp->nodetype == Identifier)
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
 
                 /* special handling for COMMON variables */

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
    if(optdebug)
      printf("call_optimize(): %s not found in global function table.\n",
        root->astnode.ident.name);

    temp = root->astnode.ident.arraylist;

    for( ; temp != NULL; temp = temp->nextstmt)
      expr_optimize (temp, rptr);
  }
}

/*****************************************************************************
 *                                                                           *
 * assign_optimize                                                           *
 *                                                                           *
 * We're looking at an assignment statement.  If the LHS of this assignment  *
 * is an argument to the current function, then it must be classified as     *
 * pass by reference.                                                        *
 *                                                                           *
 *****************************************************************************/

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
  
  ht=type_lookup(opt_type_table,root->astnode.assignment.lhs->astnode.ident.name);

  if(ht) {
    if(type_lookup(opt_args_table, 
          root->astnode.assignment.lhs->astnode.ident.name) != NULL)
      ht->variable->astnode.ident.passByRef = TRUE;
  }
  else
    fprintf(stderr,"Can't find lhs of assignment: %s\n", 
       root->astnode.assignment.lhs->astnode.ident.name);

  expr_optimize (root->astnode.assignment.rhs, rptr);
}
