/*
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */


/*****************************************************************************
 * typeassign.c                                                              *
 *                                                                           *
 * Assigns the appropriate opcodes to each node in the AST for use in the    *
 * generation of Jasmin assembly code.                                       *
 *                                                                           *
 *****************************************************************************/

#include<stdio.h>
#include<ctype.h>
#include"f2j.h"
#include<string.h>
#include"f2jparse.tab.h"
#include"f2j_externs.h"

/*****************************************************************************
 *   Global variables.                                                       *
 *****************************************************************************/

int 
  typedebug = FALSE,          /* set to TRUE to get debugging output         */
  labelnumber = 1;            /* current label number for Jasmin source      */

FILE *temp_javafp;            /* this was FILE *javafp, but I'm moving       *
                               * around the file creation code, so I'm       *
                               * altering this so that it'll compile.        * 
                               * Currently this code is not used, anyway.    *
                               * 9-11-97, Keith                              */

/*****************************************************************************
 * Function prototypes:                                                      *
 *****************************************************************************/

char
  * strdup(const char *),
  * lowercase(char * );

void
  logicalop_assign (AST *),
  assign (AST *),
  elseif_assign (AST *),
  else_assign (AST *),
  name_assign (AST *),
  assign_assign (AST *),
  spec_assign (AST *),
  call_assign (AST *),
  forloop_assign (AST *),
  logicalif_assign (AST *),
  blockif_assign (AST *),
  return_assign (void),
  label_assign (AST *),
  jas_expr_emit (AST *),
  expr_assign (AST *),
  constant_assign (AST *),
  relationalop_assign (AST *);


/*****************************************************************************
 *                                                                           *
 * assign                                                                    *
 *                                                                           *
 * This is the main type assignment routine.  It determines what kind of     *
 * node we're looking at and calls the appropriate function to handle it.    *
 *                                                                           *
 *****************************************************************************/

void
assign (AST * root)
{
  switch (root->nodetype)
  {
    case 0:
      if (typedebug)
        printf ("Bad node\n");
      assign (root->nextstmt);
    case Source:
      assign (root->astnode.source.progtype);
      assign (root->astnode.source.statements);
      break;

    case Subroutine:
      break;
    case Function:
      name_assign (root->astnode.source.name);
      break;

    case Assignment:
      assign_assign (root);
      if (root->nextstmt != NULL)
        assign (root->nextstmt);
      break;

    case Typedec:
      if (root->nextstmt != NULL)
        assign (root->nextstmt);
      break;

    case Specification:
      spec_assign (root);
      if (root->nextstmt != NULL)
        assign (root->nextstmt);
      break;

    case Statement:
      if (typedebug)
        printf ("Statement.\n");
      if (root->nextstmt != NULL)
        assign (root->nextstmt);
      break;

    case Call:
      if (typedebug)
        printf ("Call.\n");
      call_assign (root);
      if (root->nextstmt != NULL)
        assign (root->nextstmt);
      break;

    case Forloop:
      forloop_assign (root);
      if (root->nextstmt != NULL)
        assign (root->nextstmt);
      break;

    case Logicalif:
      logicalif_assign (root);
      if (root->nextstmt != NULL)
        assign (root->nextstmt);
      break;

    case Blockif:
      blockif_assign (root);
      if (root->nextstmt != NULL)
        assign (root->nextstmt);
      break;

    case Elseif:
      elseif_assign (root);
      if (root->nextstmt != NULL)
        assign (root->nextstmt);
      break;

    case Else:
      else_assign (root);
      if (root->nextstmt != NULL)
        assign (root->nextstmt);
      break;

    case Return:
      return_assign ();
      if (root->nextstmt != NULL)
        assign (root->nextstmt);
      break;

    case Label:
      label_assign (root);
    case Goto:
      if (root->nextstmt != NULL)
        assign (root->nextstmt);
      break;

    case End:
      break;
    case Constant:
    default:
      if (typedebug)
        printf ("Default\n");
  }				/* switch on nodetype.  */
}


/*****************************************************************************
 *                                                                           *
 * logicalif_assign                                                          *
 *                                                                           *
 * Before I write any of this stuff out I need to know                       *
 * what exactly the local variable name is that corresponds                  *
 * to the program name.   This is in the jasmin_table.                       *
 * The first part has to be done in expr, because that is                    *
 * where the big action is.                                                  *
 *                                                                           *
 *****************************************************************************/

void
logicalif_assign (AST * root)
{
  if (root->astnode.logicalif.conds != NULL)
    expr_assign (root->astnode.logicalif.conds);

  assign (root->astnode.logicalif.stmts);
  root->astnode.logicalif.fall_label = labelnumber++;
  root->astnode.logicalif.skip_label = labelnumber++;
}


/*****************************************************************************
 *                                                                           *
 * name_assign                                                               *
 *                                                                           *
 * A name can be a local value, an array value stored in                     *
 * local value, or associated with an object reference.                      *
 * If we have gotten this far, we may safely assume that                     *
 * that the nodetype is `identifier'.                                        *
 *                                                                           *
 *****************************************************************************/

void
name_assign (AST * root)
{
  AST *temp;

  HASHNODE *hashtemp;
  int stack = 0;

  if (root->astnode.ident.arraylist == NULL)
  {

    hashtemp = type_lookup (type_table, root->astnode.ident.name);

    if (hashtemp != NULL)
    {
      switch (hashtemp->type)
      {
        case Double:
          root->astnode.ident.opcode = strdup ("dload");
          stack += 2;
          break;
        case Integer:
          root->astnode.ident.opcode = strdup ("iload");
          stack++;
          break;
        case Float:
          root->astnode.ident.opcode = strdup ("fload");
          stack++;
          break;
        case String: 
          root->astnode.ident.opcode = strdup ("ldc");
          break;
          default:
          break;
      }
    }
    else
    {
      printf ("Name %s has not been declared.\n",
        root->astnode.ident.name);
      exit (-1);
    }
  }
    
  stacksize += stack;

  if (root->astnode.ident.arraylist != NULL)
  {
    if(typedebug)
      printf("Found an array in typeassign\n");

    root->astnode.ident.opcode = strdup ("aload");
    temp = root->astnode.ident.arraylist;

    /* should this really be jas_expr_emit?? */
    jas_expr_emit(temp);

  /*
   *if (temp->nextstmt != NULL)
   *  temp = temp->nextstmt;
   */
  }
}


/*****************************************************************************
 *                                                                           *
 * assign_assign                                                             *
 *                                                                           *
 * This function performs type assignment to assignment statements.          *
 *                                                                           *
 *****************************************************************************/

void
assign_assign (AST * root)
{
  char *javaname;

  name_assign (root->astnode.assignment.lhs);
  expr_assign (root->astnode.assignment.rhs);
  javaname = root->astnode.assignment.lhs->astnode.ident.opcode;

  /* So, basically, the assignment takes the same type as the
   * the left-hand-side of the expression.  To do this properly
   * we need to check the context of each side (I think), and
   * possibly widen or narrow (i.e. int to float etc.) the
   * expressions.  This might require opcode like `f2i' etc. 
   * This is definitely a hack and needs to be fixed. 
   */

  switch (*javaname)
  {
    case 'i':
      root->astnode.assignment.opcode = strdup ("istore");
      break;
  }
}				/* Close assign_assign(). */

/*****************************************************************************
 *                                                                           *
 * constant_assign                                                           *
 *                                                                           *
 * Deal with pushing constant values onto the JVM stack.                     *
 * The opcode here is  going to be stuff like ldc, dconst, iconst,           *
 * etc.  Short term, we deal with only 32 bit ints.                          *
 *                                                                           *
 *****************************************************************************/

void
constant_assign (AST * root)
{
  /*  Need to check for arrays here also.  */
  switch (root->vartype)
  {
    case Integer:
      root->astnode.constant.opcode = strdup ("ldc");
      break;
    case Double:
      root->astnode.constant.opcode = strdup ("ldc");
      break;
    default:
      break;  /* last break for ANSI compliance */
  }
}				/* Close constant_assign()  */

/*****************************************************************************
 *                                                                           *
 * expr_assign                                                               *
 *                                                                           *
 * All this will do is assign a number if there is one.                      *
 * Needs to be extended for arrays, etc.  Consider using                     *
 * a switch/case structure for this.                                         *
 *                                                                           *
 *****************************************************************************/

void
expr_assign (AST * root)
{
  int stack;

  switch (root->nodetype)
  {
    case Identifier:
      name_assign (root);
      break;

    case Expression:
      /*
         if (root->astnode.expression.parens)
           fprintf (temp_javafp, "(");
       */

      if (root->astnode.expression.lhs != NULL)
        expr_assign (root->astnode.expression.lhs);
      if (root->astnode.expression.rhs != NULL)
        expr_assign (root->astnode.expression.rhs);

      /*
         if (root->astnode.expression.parens)
           fprintf (temp_javafp, ")");
       */

      break;

    case Binaryop:

      /* Ok, to emit a binary op, we need to examine the
       * context of the stuff being operated on, that is
       * to choose iadd, dadd, etc.  
       */

      expr_assign (root->astnode.expression.lhs);
      switch (root->astnode.expression.optype)
      {
        case '+':
          root->astnode.expression.opcode = strdup ("iadd");
          stack--;
          break;
        case '*':
          root->astnode.expression.opcode = strdup ("imul");
          stack--;
          break;
        case '-':
          root->astnode.expression.opcode = strdup ("isub");
          stack--;
          break;
        case '/':
          root->astnode.expression.opcode = strdup ("idiv");
          stack--;
          break;
      }
      expr_assign (root->astnode.expression.rhs);
      stacksize += stack;
      break;
    case Unaryop:
      fprintf (temp_javafp, "%c", root->astnode.expression.minus);
      expr_assign (root->astnode.expression.rhs);
      break;
    case Constant:
      constant_assign (root);
      break;
    case Logicalop:
      expr_assign (root->astnode.expression.lhs);
      expr_assign (root->astnode.expression.rhs);
      /* logicalop_assign(root); */
      break;

    case Relationalop:
      expr_assign (root->astnode.expression.lhs);
      expr_assign (root->astnode.expression.rhs);
      relationalop_assign (root);
      break;
    default:
      fprintf(stderr,"typeassign: Bad node in expr_assign\n");
  }
}


/*****************************************************************************
 *                                                                           *
 * forloop_assign                                                            *
 *                                                                           *
 * This function performs type assignment for DO loops.                      *
 *                                                                           *
 *****************************************************************************/

void
forloop_assign (AST * root)
{
  assign_assign (root->astnode.forloop.start);
  root->astnode.forloop.startlabel = labelnumber++;

  if (root->astnode.forloop.incr)
    expr_assign (root->astnode.forloop.incr);

  /* assign (root->astnode.forloop.stmts); */

  /*  This has probably already been dealt with
   *   due to the way the pointers work.  
   */

  name_assign (root->astnode.forloop.counter);

  root->astnode.forloop.stoplabel = labelnumber++;
  expr_assign (root->astnode.forloop.stop);
}

/*****************************************************************************
 *                                                                           *
 * blockif_assign                                                            *
 *                                                                           *
 * This function performs type assignment for block IF statements.           *
 *                                                                           *
 *****************************************************************************/

void
blockif_assign (AST * root)
{
  if (root->astnode.blockif.conds != NULL)
    expr_assign (root->astnode.blockif.conds);

  if (root->astnode.blockif.stmts != NULL)
    assign (root->astnode.blockif.stmts);

  root->astnode.blockif.skip_label = labelnumber++;

  if (root->astnode.blockif.elseifstmts != NULL)
    assign (root->astnode.blockif.elseifstmts);

  if (root->astnode.blockif.elsestmts != NULL)
    assign (root->astnode.blockif.elsestmts);

  /* This label lands us at the very end of the
   * blockif statements.  
   */

  root->astnode.blockif.break_label = labelnumber - 1;
}

/*****************************************************************************
 *                                                                           *
 * elseif_assign                                                             *
 *                                                                           *
 * This function performs type assignment for the elseif block of an IF      *
 * statement.                                                                *
 *                                                                           *
 *****************************************************************************/

void
elseif_assign (AST * root)
{
  if (root->astnode.blockif.conds != NULL)
    expr_assign (root->astnode.blockif.conds);

  if (root->astnode.blockif.stmts != NULL)
    assign (root->astnode.blockif.stmts);

  root->astnode.blockif.skip_label = labelnumber++;
}

/*****************************************************************************
 *                                                                           *
 * else_assign                                                               *
 *                                                                           *
 * This function performs type assignment for the else block of an IF stmt.  *
 *                                                                           *
 *****************************************************************************/

void
else_assign (AST * root)
{
  assign (root->astnode.blockif.stmts);

  root->astnode.blockif.skip_label = labelnumber++;
}

/*****************************************************************************
 *                                                                           *
 * call_assign                                                               *
 *                                                                           *
 * Not sure exactly what is going on here.  Probably need to                 *
 * rewrite this routine completely.  The fortran subroutine                  *
 * calls (CALL) are critical to implement properly.  For now,                *
 * they are translated to static method invocations.                         *
 *                                                                           *
 *****************************************************************************/

void
call_assign (AST * root)
{
  AST *temp;
  char *tempname;

  lowercase (root->astnode.ident.name);
  tempname = strdup (root->astnode.ident.name);
  *tempname = toupper (*tempname);

  /* Assume all methods that are invoked are static.  */
  fprintf (temp_javafp, "%s.%s", tempname, root->astnode.ident.name);

  temp = root->astnode.ident.arraylist;
  fprintf (temp_javafp, "(");

  while (temp->nextstmt != NULL)
  {
    expr_assign (temp);
    fprintf (temp_javafp, ",");
    temp = temp->nextstmt;
  }

  expr_assign (temp);

  fprintf (temp_javafp, ");\n");
}

/*****************************************************************************
 *                                                                           *
 * spec_assign                                                               *
 *                                                                           *
 * This function performs type assignment for a specification statement.     *
 *                                                                           *
 *****************************************************************************/

void
spec_assign (AST * root)
{
  AST *assigntemp;

  /* I am reaching every case in this switch.  */
  switch (root->astnode.typeunit.specification)
  {
    /* PARAMETER in fortran corresponds to a class
     * constant in java, that has to be declared
     * class wide outside of any method.  This is
     * currently not implemented, but the assignment
     * is made.  
     */

    case Parameter:
      assigntemp = root->astnode.typeunit.declist;
      printf ("Parameter stmt.\n");

      /*  fprintf (temp_javafp, "public static final "); */

      /* Now look up the variable in the symbol table to
       * see what kind of type it is. 
       */

      /* Let's assign a comment here noting that the assignment
       * comes from a fortran PARAMETER specification. 
       */

      fprintf(temp_javafp,"// Assignment from fortran PARAMETER spec.\n");
      name_assign (assigntemp->astnode.assignment.lhs);
      fprintf (temp_javafp, " = ");
      expr_assign (assigntemp->astnode.assignment.rhs);
      fprintf (temp_javafp, ";\n");
      break;

      /*  I am reaching these next two cases.  */
    case Intrinsic:
      /*        printf ("Intrinsic stmt.\n");  */

      name_assign (root);
      break;

    case External:
      /*        printf ("External stmt.\n");   */
      break;
    default:
      break;
  }
}

/*****************************************************************************
 *                                                                           *
 * return_assign                                                             *
 *                                                                           *
 * Do nothing here.                                                          *
 *                                                                           *
 *****************************************************************************/
void
return_assign ()
{
    ;
}

/*****************************************************************************
 *                                                                           *
 * label_assign                                                              *
 *                                                                           *
 * This could probably have been handled up top,                             *
 * but just for uniformity, handle it here.                                  *
 *                                                                           *
 *****************************************************************************/

void
label_assign (AST * root)
{
  assign (root->astnode.label.stmt);
}

/*****************************************************************************
 *                                                                           *
 * logicalop_assign                                                          *
 *                                                                           *
 *                                                                           *
 *****************************************************************************/

void
logicalop_assign (AST * root)
{
  if (root->token == AND)
  {
    /* We want to skip over the conditional execution. */

    root->astnode.expression.label = labelnumber + 1;
  }
  else if (root->token == OR)
  {
    /* Jump to the executable statements. */

    root->astnode.expression.label = labelnumber;
  }
  else
  {
    /*  Handle the NOT token.  */

    ;
  }
}				/* Close logicalop_assign().   */


/*****************************************************************************
 *                                                                           *
 * relationalop_assign                                                       *
 *                                                                           *
 * This whole mess of stuff is a terrible hack.                              *
 * Mostly it indicates that I don't have a good                              *
 * grip on LR grammar.                                                       *
 *                                                                           *
 *****************************************************************************/

void
relationalop_assign (AST * root)
{
  /*  Cases to consider:  parent is AND, OR.  */
  if (root->parent == NULL)
  {
	  printf ("NULL parent. Bug in the parser.\n");
	  fclose (jasminfp);
	  exit (-1);
  }

  if ((root->parent->token == OR && root->expr_side == right) ||
	(root->parent->token == AND && root->expr_side == left))
  {
    switch (root->token)
    {
      case rel_eq:
        root->astnode.expression.opcode = strdup ("if_icmpne");
        break;
      case rel_ne:
        root->astnode.expression.opcode = strdup ("if_icmpeq");
        break;
      case rel_lt:
        root->astnode.expression.opcode = strdup ("if_icmpge");
        break;
      case rel_le:
        root->astnode.expression.opcode = strdup ("if_icmpgt");
        break;
      case rel_gt:
        root->astnode.expression.opcode = strdup ("if_icmple");
        break;
      case rel_ge:
        root->astnode.expression.opcode = strdup ("if_icmplt");
        break;
    }
    root->astnode.expression.label = labelnumber;
    return;
  }

  if (root->parent->token == OR && root->expr_side == left)
  {
    switch (root->token)
    {
      case rel_eq:
        root->astnode.expression.opcode = strdup ("if_icmpeq");
        break;
      case rel_ne:
        root->astnode.expression.opcode = strdup ("if_icmpne");
        break;
      case rel_lt:
        root->astnode.expression.opcode = strdup ("if_icmplt");
        break;
      case rel_le:
        root->astnode.expression.opcode = strdup ("if_icmple");
        break;
      case rel_gt:
        root->astnode.expression.opcode = strdup ("if_icmpgt");
        break;
      case rel_ge:
        root->astnode.expression.opcode = strdup ("if_icmpge");
        break;
    }
    root->astnode.expression.label = labelnumber;
    return;
  }

  switch (root->token)
  {
    case rel_eq:
      root->astnode.expression.opcode = strdup ("if_icmpne");
      break;
    case rel_ne:
      root->astnode.expression.opcode = strdup ("if_icmpeq");
      break;
    case rel_lt:
      root->astnode.expression.opcode = strdup ("if_icmpge");
      break;
    case rel_le:
      root->astnode.expression.opcode = strdup ("if_icmpgt");
      break;
    case rel_gt:
      root->astnode.expression.opcode = strdup ("if_icmple");
      break;
    case rel_ge:
      root->astnode.expression.opcode = strdup ("if_icmplt");
      break;
  }

  root->astnode.expression.label = labelnumber;
  return;
}
