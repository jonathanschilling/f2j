/*
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */


/*****************************************************************************
 * opcode.c                                                                  *
 *                                                                           *
 * Generates Jasmin source code from the AST representation of a Fortran     *
 * program.  This code is seriously out of date with respect to the recent   *
 * changes in Java code generation.  This stuff probably does not work any   *
 * more actually.  Now that we can translate GOTOs in Java source, Jasmin    *
 * code generation has become much less important.                           *
 *                                                                           *
 *****************************************************************************/


#include<stdio.h>
#include<stdlib.h>
#include"f2j.h"
#include<string.h>
#include"f2jparse.tab.h"

#define Mindent1 "   "		/* Indentation space macro.                  */

AST *returnname;                /* return type of function                   */

int 
  jas_gendebug = 0,             /* Set to 1 for debugging output.            */
  labelno = 1,		        /* Matches output from D-Java.               */
  breaklabel;			/* Global to deal with if-then-else-endif.   */

/* Strings representing the different return types in Jasmin.                */

char *jas_returnstring[] =
  {"Ljava/lang/String;", "complex", "D", "F", "I", "B"};

char *typestring[] =
  {"Ljava/lang/String;", "complex", "d", "f", "i", "b"};


void method (AST *),
  jas_emit (AST *),
  jas_logicalif_emit (AST *),
  jas_blockif_emit (AST *),
  jas_assign_emit (AST *),
  jas_forloop_emit (AST *),
  jas_goto_emit (AST *),
  jas_label_emit (AST *),
  jas_elseif_emit (AST *),
  jas_else_emit (AST *),
  jas_return_emit (AST *),
  jas_constant_emit (AST *),
  jas_name_emit (AST *),
  jas_expr_emit (AST *),
  jas_incr_emit (AST *),
  jas_logicalop_emit (AST *);

/*****************************************************************************
 *                                                                           *
 * jas_emit                                                                  *
 *                                                                           *
 * This is the main code generation routine.  It figures out what kind of    *
 * AST node we're looking at and calls the appropriate routine to handle     *
 * that node.                                                                *
 *                                                                           *
 *****************************************************************************/

void
jas_emit (AST * root)
{

  switch (root->nodetype)
  {
    case 0:
fprintf (stderr,"jas_emit(): Bad node\n");
jas_emit (root->nextstmt);
    case Source:
jas_emit (root->astnode.source.progtype);
jas_emit (root->astnode.source.statements);
break;
    case Subroutine:
returnname = NULL;
method (root);
break;
    case Function:
returnname = root->astnode.source.name;
method (root);
break;
    case Logicalif:
jas_logicalif_emit (root);
/*  I think the way this works is that there are two cases
* "true" and "false", therefore need to increment the
* label number by two. Could be interesting when I try
* to handle if-then-else...  
*/

labelno += 2;
if (root->nextstmt != NULL)
jas_emit (root->nextstmt);
break;
    case Blockif:
jas_blockif_emit (root);
labelno += 2;
if (root->nextstmt != NULL)
jas_emit (root->nextstmt);
break;
    case Elseif:
jas_elseif_emit (root);
labelno += 2;
if (root->nextstmt != NULL)
jas_emit (root->nextstmt);
break;

    case Else:
jas_else_emit (root);
labelno += 2;
if (root->nextstmt != NULL)
jas_emit (root->nextstmt);
break;

    case Assignment:
jas_assign_emit (root);
if (root->nextstmt != NULL)
jas_emit (root->nextstmt);
break;
    case Forloop:
jas_forloop_emit (root);
if (root->nextstmt != NULL)
jas_emit (root->nextstmt);
break;
    case End:
fprintf (jasminfp, ".end method\n");
break;
    case Goto:
jas_goto_emit (root);
if (root->nextstmt != NULL)
jas_emit (root->nextstmt);
break;

    case Return:
jas_return_emit (root);
if (root->nextstmt != NULL)
jas_emit (root->nextstmt);
break;

    case Label:
jas_label_emit (root);
if (root->nextstmt != NULL)
jas_emit (root->nextstmt);
break;

    default:
if (root->nextstmt != NULL)
jas_emit (root->nextstmt);
break;
    }				/* Close switch().  */
}


/*****************************************************************************
 *                                                                           *
 * method                                                                    *
 *                                                                           *
 * This function emits the method header.  For a function like ddot,         *
 * the string  needs to look like ddot(I[DI[DI)D.  So I have to create       *
 * a function here, or some code statements to build the string.  The        *
 * return type goes at the end of the method argument list.                  *
 *                                                                           *
 *****************************************************************************/

void
method (AST * root)
{
  enum returntype returns;
  AST *tempnode, *temp;
  char *tempstring;
  HASHNODE *hashtemp;

  /* Ok, this line does the deed to print out the message header.  */

  fprintf (jasminfp, "\n\n.method public static %s(",
	     root->astnode.source.name->astnode.ident.name);

  /* Now traverse the argument list.  */

  tempnode = root->astnode.source.args;

  for (; tempnode != NULL; tempnode = tempnode->nextstmt)
  {
    hashtemp = type_lookup (type_table, tempnode->astnode.ident.name);
    if (hashtemp == NULL)
    {
      printf ("Type table is screwed.\n");
      exit (-1);
    }
    returns = hashtemp->type;

    tempstring = jas_returnstring[returns];

    if (hashtemp->variable->astnode.ident.arraylist)
    {
      temp = hashtemp->variable->astnode.ident.arraylist;
      for (; temp; temp = temp->nextstmt)
        fprintf (jasminfp, "[");
    }
    fprintf (jasminfp, "%s", tempstring);
  }				/* End for() loop.  */

  /*  Returns...  */
  if (returnname)
    fprintf (jasminfp, ")%s\n", jas_returnstring[root->astnode.source.returns]);
  else
    fprintf (jasminfp, ")V\n\n");

  /*  Method directives for jasmin. */
  fprintf (jasminfp, ".limit stack %d\n", stacksize);
  fprintf (jasminfp, ".limit locals %d\n\n", locals);

}				/* Close method()  */


/*****************************************************************************
 *                                                                           *
 * jas_logicalif_emit                                                        *
 *                                                                           *
 * Before I write any of this stuff out I need to know                       *
 * what exactly the local variable name is that corresponds                  *
 * to the program name.   This is in the jasmin_table.                       *
 * The first part has to be done in expr, because that is where the          *
 * big action is.                                                            *
 *                                                                           *
 *****************************************************************************/

void
jas_logicalif_emit (AST * root)
{
  fprintf (jasminfp, "\n; Logical `if' statement.\n");

  if (root->astnode.logicalif.conds != NULL)
    jas_expr_emit (root->astnode.logicalif.conds);

  /*  Big test.  All the rest works great... */

  if (root->astnode.logicalif.conds->token == AND)
    fprintf (jasminfp, "Label%d:\n", root->astnode.logicalif.fall_label);

  if (root->astnode.logicalif.conds->token == OR)
    fprintf (jasminfp, "Label%d:\n", root->astnode.logicalif.fall_label);

  jas_emit (root->astnode.logicalif.stmts);

  fprintf (jasminfp, "Label%d:\n", root->astnode.logicalif.skip_label);
}


/*****************************************************************************
 *                                                                           *
 * jas_blockif_emit                                                          *
 *                                                                           *
 * This routine emits a block IF statement which is an IF that may           *
 * optionally include ELSE IF and ELSE blocks.                               *
 *                                                                           *
 *****************************************************************************/

void
jas_blockif_emit (AST * root)
{
  breaklabel = root->astnode.blockif.break_label;

  fprintf (jasminfp, "\n; Block `if' statement.\n");

  if (root->astnode.blockif.conds != NULL)
    jas_expr_emit (root->astnode.blockif.conds);

  if (root->astnode.blockif.stmts != NULL)
    jas_emit (root->astnode.blockif.stmts);

  fprintf (jasminfp, Mindent1 "goto Label%d:\t; No falling through.\n",
     breaklabel);

  if (root->astnode.blockif.conds->token == AND)
    fprintf (jasminfp, "Label%d:\n", root->astnode.blockif.skip_label);
  else if (root->astnode.blockif.conds->token == OR)
    fprintf (jasminfp, "Label%d:\n", root->astnode.blockif.skip_label);
  else
    fprintf (jasminfp, "Label%d:\n", root->astnode.blockif.skip_label);

  if (root->astnode.blockif.elseifstmts != NULL)
    jas_emit (root->astnode.blockif.elseifstmts);

  if (root->astnode.blockif.elsestmts != NULL)
  {
    jas_emit (root->astnode.blockif.elsestmts);
    fprintf (jasminfp, "Label%d:\n", breaklabel);
  }
}

/*****************************************************************************
 *                                                                           *
 * jas_elseif_emit                                                           *
 *                                                                           *
 * This function emits the ELSE IF part of a block IF statement.             *
 *                                                                           *
 *****************************************************************************/

void
jas_elseif_emit (AST * root)
{
  if (root->astnode.blockif.conds != NULL)
    jas_expr_emit (root->astnode.blockif.conds);

  if (root->astnode.blockif.conds->token == AND)
    fprintf (jasminfp, "Label%d:\n", root->astnode.blockif.fall_label);

  if (root->astnode.blockif.conds->token == OR)
    fprintf (jasminfp, "Label%d:\n", root->astnode.blockif.fall_label);

  jas_emit (root->astnode.blockif.stmts);

  fprintf(jasminfp,Mindent1 "goto Label%d:\t; Skip remainder.\n", breaklabel);

  fprintf (jasminfp, "Label%d:\n", root->astnode.blockif.skip_label);
}

/*****************************************************************************
 *                                                                           *
 * jas_else_emit                                                             *
 *                                                                           *
 * This function emits the ELSE part of a block IF statement.                *
 *                                                                           *
 *****************************************************************************/

void
jas_else_emit (AST * root)
{
  jas_emit (root->astnode.blockif.stmts);
}

/*****************************************************************************
 *                                                                           *
 * jas_expr_emit                                                             *
 *                                                                           *
 * Here is where most of the action is.  This function generates expressions *
 * in Jasmin.  Sometimes we call other routines to handle things (for        *
 * example, identifiers), but most of it is done here with recursive calls.  *
 *                                                                           *
 *****************************************************************************/

void
jas_expr_emit (AST * root)
{

  switch (root->nodetype)
  {
    case Expression:
      if (root->astnode.expression.lhs != NULL)
        jas_expr_emit (root->astnode.expression.lhs);
      jas_expr_emit (root->astnode.expression.rhs);
      break;
    case Identifier:
      jas_name_emit (root);
      break;
    case Constant:
      jas_constant_emit (root);
      break;
    case Binaryop:
      jas_expr_emit (root->astnode.expression.lhs);
      jas_expr_emit (root->astnode.expression.rhs);
      fprintf (jasminfp, Mindent1 "%s\t\t; %c\n",
            root->astnode.expression.opcode,
            root->astnode.expression.optype);
      break;
    case Logicalop:
      /*  Might be easier to inline this procedure.  */
      jas_logicalop_emit (root);
      break;
    case Relationalop:
      /* May have to change the way these work because of
       * problems with getting logical operations such as
       * AND and OR to work properly.   
       */

      jas_expr_emit (root->astnode.expression.rhs);
      jas_expr_emit (root->astnode.expression.lhs);

      if (root->parent->token == AND /* && root->expr_side == left */ )
        fprintf (jasminfp, Mindent1 "%s Label%d\n",
            root->astnode.expression.opcode,
            root->astnode.expression.label);

      /*  May be some funny business here.  */
      else if (root->parent->token == OR && root->expr_side == right)
        fprintf (jasminfp, Mindent1 "%s Label%dOR\n",
            root->astnode.expression.opcode,
            root->astnode.expression.label);

      else
        fprintf (jasminfp, Mindent1 "%s Label%d\n",
            root->astnode.expression.opcode,
            root->astnode.expression.label);
      break;
    default:
      fprintf(stderr,"Unknown node type in jas_expr_emit()\n");
  }
}

/*****************************************************************************
 *                                                                           *
 * jas_name_emit                                                             *
 *                                                                           *
 * This function emits an identifier.                                        *
 * There is a really nasty segfault occurring in this routine,               *
 * and it is screwing up a bunch of stuff.  I have no idea where             *
 * it is or why it is occurring.                                             *
 *                                                                           *
 *****************************************************************************/

void
jas_name_emit (AST * root)
{
  AST *temp;

  HASHNODE *hashtemp;

  /* By the time I get to here, I should have tested whether
   * the name is in the static or virtual tables, etc.  
   */

  /* I also need to test for lists of array or functions 
   * arguments in here too.  
   */

    

  /* Testing for NULL qualifies as a hack because it
   * doesn't work right without it and I don't understand
   * why not.  
   */

  if (root->astnode.ident.opcode != NULL)
    fprintf (jasminfp, Mindent1 "%s ", root->astnode.ident.opcode);

  /* Seriously badly hacked here... I have no idea what is
   * going on and why it may be happening.  Basically, it
   * seems that there is some binary crapola posing as an
   * identifier, which causes the symtable lookup to puke.
   * I have trapped the error to keep the program from
   * segfaulting and dying.  
   */

  if (root->astnode.ident.name != NULL)
  {
    hashtemp = type_lookup (type_table, root->astnode.ident.name);

    /* Also in here have to check for array access stuff. */
    if (hashtemp != NULL)
    {
      if (root->astnode.ident.arraylist == NULL)
      {
        fprintf (jasminfp, "%d", hashtemp->variable->astnode.ident.localvnum);
        fprintf (jasminfp, "\t; %s\n", root->astnode.ident.name);
      }
      else
      {
        fprintf (jasminfp, "%d", hashtemp->variable->astnode.ident.localvnum);
        fprintf (jasminfp, "\t; %s\n", root->astnode.ident.name);

        temp = root->astnode.ident.arraylist;
        jas_expr_emit(temp);

        if (temp->nextstmt != NULL)
        {
          temp = temp->nextstmt;
          jas_expr_emit(temp);

          /* changed this line.  not sure why it is passing
           *leaddim and I'm not sure what it is supposed to pass.
           *for now, we just pass root.
           *
           *jas_expr_emit(root->astnode.ident.leaddim); 
           */

          jas_expr_emit(root);
          fprintf(jasminfp, "imult\n");
          fprintf(jasminfp, "iadd\n");
        }
      }
    }
    else
    {
      printf ("Seems to be a problem in opcode.c (name_emit...)\n");
      printf ("Tried to find something nasty in the hash table.\n");
      return;
    }
  }
}


/*****************************************************************************
 *                                                                           *
 * jas_assign_emit                                                           *
 *                                                                           *
 * This function emits an assignment statement                               *
 *                                                                           *
 *****************************************************************************/

void
jas_assign_emit (AST * root)
{
  HASHNODE *hashtemp;
  char *javaname;

  javaname = root->astnode.assignment.lhs->astnode.ident.name;
  jas_expr_emit (root->astnode.assignment.rhs);
  fprintf (jasminfp, Mindent1 "%s ", root->astnode.assignment.opcode);

  hashtemp = type_lookup (type_table, javaname);
  if (hashtemp != NULL)
  {
    fprintf (jasminfp, "%d", hashtemp->variable->astnode.ident.localvnum);
    fprintf (jasminfp, "\t; = %s\n", javaname);
  }
}				/* Close assign_emit(). */


/*****************************************************************************
 *                                                                           *
 * jas_constant_emit                                                         *
 *                                                                           *
 * This function emits a constant.                                           *
 *                                                                           *
 *****************************************************************************/

void
jas_constant_emit (AST * root)
{
  /*  Need to check for arrays here also.  */
  switch (root->vartype)
  {
    case Integer:
      fprintf (jasminfp, Mindent1 "%s %s",
            root->astnode.constant.opcode,
            root->astnode.constant.number);
      fprintf (jasminfp, "\t; %s\n", root->astnode.constant.number);
      break;
    default:
      break;   /* unnecessary break for ANSI compliance */
  }
}				/* Close constant_emit()  */


/*****************************************************************************
 *                                                                           *
 * jas_forloop_emit                                                          *
 *                                                                           *
 * This function emits a forloop-type structure in Jasmine.  Of course, it   *
 * must be implemented using labels and gotos.                               *
 *                                                                           *
 *****************************************************************************/

void
jas_forloop_emit (AST * root)
{
  fprintf (jasminfp, "\n; do loop.\n; Initialize counter.\n");
  jas_assign_emit (root->astnode.forloop.start);

  fprintf (jasminfp, Mindent1 "goto Label%d\n",
    root->astnode.forloop.stoplabel);

  fprintf (jasminfp, "\nLabel%d:\n",
    root->astnode.forloop.startlabel);

  fprintf (jasminfp, "; Executable statements.\n");

/* stmts removed from forloop struct when I updated the
 * handling of do loops.  this routine needs to be updated
 * also.  -Keith 2/9/98
 * 
 * jas_emit (root->astnode.forloop.stmts); 
 *
 */

  fprintf (jasminfp, "\n; Increment counter.\n");

  /*  Put the increment counter in here.  */
  jas_incr_emit (root->astnode.forloop.counter);

  fprintf (jasminfp, "\nLabel%d:\n", root->astnode.forloop.stoplabel);

  fprintf (jasminfp, "; Compare, jump to Label%d to iterate.\n",
    root->astnode.forloop.startlabel);

  jas_expr_emit (root->astnode.forloop.stop);

  /*  Need to get the variable name and number to load here. */
  jas_name_emit (root->astnode.forloop.counter);

  fprintf (jasminfp, Mindent1 "if_icmplt Label%d\n",
    root->astnode.forloop.startlabel);
}

/*****************************************************************************
 *                                                                           *
 * jas_incr_emit                                                             *
 *                                                                           *
 * This function generates code to increment a variable.                     *
 *                                                                           *
 *****************************************************************************/

void
jas_incr_emit (AST * root)
{
  HASHNODE *hashtemp;

  hashtemp = type_lookup (type_table, root->astnode.ident.name);
  if (hashtemp != NULL)
  {
    fprintf (jasminfp, Mindent1 "iinc %d 1", hashtemp->variable->astnode.ident.localvnum);
    fprintf (jasminfp, "\t; Increment counter %s.\n",
        root->astnode.ident.name);
  }
  else
  {
    /* Handle the error. */
    printf ("Error in incr_emit() routine.\n");
    fclose (jasminfp);
    exit (-1);
  }
}

/*****************************************************************************
 *                                                                           *
 * jas_goto_emit                                                             *
 *                                                                           *
 * This function generates a Jasmin goto instruction.                        *
 *                                                                           *
 *****************************************************************************/

void
jas_goto_emit (AST * root)
{
  fprintf (jasminfp, Mindent1 "goto S_label%d\n", root->astnode.go_to.label);
}

/*****************************************************************************
 *                                                                           *
 * jas_label_emit                                                            *
 *                                                                           *
 * This function generates a Jasmin label.                                   *
 *                                                                           *
 *****************************************************************************/

void
jas_label_emit (AST * root)
{
  fprintf (jasminfp, "\nS_label%d:\n", root->astnode.label.number);
  jas_emit (root->astnode.label.stmt);
}


/*****************************************************************************
 *                                                                           *
 * jas_return_emit                                                           *
 *                                                                           *
 * This function generates a return instruction.                             *
 *                                                                           *
 *****************************************************************************/

void
jas_return_emit (AST * root)
{
  if (returnname)
  {
    jas_name_emit (returnname);
    fprintf (jasminfp, Mindent1 "ireturn\n\n");
  }
  else
    fprintf (jasminfp, Mindent1 "return\n\n");
}


/*****************************************************************************
 *                                                                           *
 * jas_logicalop_emit                                                        *
 *                                                                           *
 * This function generates a logical operation.                              *
 *                                                                           *
 *****************************************************************************/

void
jas_logicalop_emit (AST * root)
{
  fprintf (jasminfp, "; Logical operation.\n");
  jas_expr_emit (root->astnode.expression.lhs);
  jas_expr_emit (root->astnode.expression.rhs);
}
