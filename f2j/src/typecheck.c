/*
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */


/*****************************************************************************
 * typecheck.c                                                               *
 *                                                                           *
 * Traverses the AST to determine the data type for all expressions.         *
 *                                                                           *
 *****************************************************************************/


#include<stdio.h>
#include<string.h>
#include<ctype.h>
#include"f2j.h"
#include"f2jparse.tab.h"

/*****************************************************************************
 * Function prototypes:                                                      *
 *****************************************************************************/

char 
  * strdup ( const char * ),
  * merge_names(AST *),
  * print_nodetype ( AST * );

METHODTAB
  * methodscan (METHODTAB *, char *);

void 
  print_eqv_list(AST *, FILE *),
  remove_duplicates(AST *),
  typecheck (AST *),
  elseif_check(AST *),
  func_array_check(AST *, HASHNODE *),
  else_check (AST *),
  expr_check (AST *),
  assign_check (AST *),
  name_check (AST *),
  data_check(AST *),
  common_check(AST *),
  call_check (AST *),
  forloop_check (AST *),
  blockif_check (AST *),
  logicalif_check (AST *),
  check_implied_loop(AST *),
  read_write_check (AST *),
  merge_equivalences(AST *),
  check_equivalences(AST *),
  insertEquivalences(AST *),
  type_insert(SYMTABLE *, AST *, enum returntype, char *),
  external_check(AST *),
  typedec_check(AST *),
  intrinsic_check(AST *),
  array_check(AST *, HASHNODE *),
  subcall_check(AST *);

SYMTABLE 
  * new_symtable(int);

extern METHODTAB intrinsic_toks[];

/*****************************************************************************
 * Global variables.                                                         *
 *****************************************************************************/

int checkdebug = TRUE;              /* set to TRUE for debugging output     */

AST *cur_unit;                       /* program unit currently being checked */

SYMTABLE 
  * chk_type_table,                  /* ptr to this unit's symbol table      */
  * chk_external_table,              /* ptr to table of external functions   */
  * chk_intrinsic_table,             /* ptr to table of intrinsics           */
  * chk_array_table;                 /* ptr to array table                   */

char bitfields[] = {                 /* for typechecking intrinsics          */
  STRING_ARG,CHAR_ARG,COMPLEX_ARG,DOUBLE_ARG,REAL_ARG,INT_ARG,LOGICAL_ARG
};

/*****************************************************************************
 *                                                                           *
 * typecheck                                                                 *
 *                                                                           *
 * This is the main typechecking function.  We traverse the                  *
 * AST and recursively call typecheck() on each node.  This                  *
 * function figures out what kind of node it's looking at and                *
 * calls the appropriate function to handle the typechecking.                *
 *                                                                           *
 *****************************************************************************/

void
typecheck (AST * root)
{
  switch (root->nodetype)
  {
    case 0:
      if (checkdebug)
        printf ("typecheck(): Bad node\n");
      typecheck (root->nextstmt);
      break;
    case Progunit:
      if (checkdebug)
        printf ("typecheck(): Source.\n");

      chk_type_table = root->astnode.source.type_table;
      chk_external_table = root->astnode.source.external_table;
      chk_intrinsic_table = root->astnode.source.intrinsic_table;
      chk_array_table = root->astnode.source.array_table;

      merge_equivalences(root->astnode.source.equivalences);

      /* now that the equivalences have been merged and duplicates
       * removed, we insert the variable names into a symbol table.
       */

      root->astnode.source.equivalence_table = new_symtable(211);
      insertEquivalences(root);

      check_equivalences(root->astnode.source.equivalences);

      typecheck (root->astnode.source.progtype);
      typecheck (root->astnode.source.typedecs);
      typecheck (root->astnode.source.statements);

      break;
    case Subroutine:
    case Function:
    case Program:
      {
        AST *temp;

        cur_unit = root;

        for(temp = root->astnode.source.args;temp!=NULL;temp=temp->nextstmt)
          if(type_lookup(chk_external_table,temp->astnode.ident.name) != NULL)
            cur_unit->astnode.source.needs_reflection = TRUE;

      }
      break;
    case End:
      if (checkdebug)
        printf ("typecheck(): %s.\n", print_nodetype(root));
      break;
    case DataList:
      data_check(root);
      if(root->nextstmt != NULL)
        typecheck(root->nextstmt);
      break;
    case Label:
      if(root->astnode.label.stmt != NULL) 
        typecheck(root->astnode.label.stmt);

      if(root->nextstmt != NULL)
        typecheck(root->nextstmt);
      break;
    case Equivalence:
      if(checkdebug)
        printf("ignoring equivalence in typechecking\n");

      if(root->nextstmt != NULL)
        typecheck(root->nextstmt);
      break;
    case Arithmeticif:
      if(checkdebug)
        printf("typecheck(): ArithmeticIf.\n");

      if (root->astnode.arithmeticif.cond != NULL)
        expr_check (root->astnode.arithmeticif.cond);

      if(root->nextstmt != NULL)
        typecheck(root->nextstmt);
      break;
    case ComputedGoto:
      if (root->astnode.computed_goto.name)
        expr_check(root->astnode.computed_goto.name);

      if(root->nextstmt != NULL)
        typecheck(root->nextstmt);
      break;
    case Typedec:
      typedec_check(root);
      
      if (root->nextstmt != NULL)
        typecheck (root->nextstmt);
      break;
    case Specification:
    case Dimension:
    case Statement:
    case Return:
    case Goto:
    case Format:
    case Stop:
    case Save:
    case MainComment:
    case Unimplemented:

      if (checkdebug)
        printf ("typecheck(): %s.\n", print_nodetype(root));

      if (root->nextstmt != NULL)
        typecheck (root->nextstmt);
      break;
    case Comment:
      /* we're looking at a comment - possibly several lines
       * of comments.  Here we count the number of lines in
       * this comment.  If this is the biggest (ie, longest
       * in terms of number of lines), then we make it the
       * MainComment which is generated in javadoc format.
       *
       * Deciding that the longest comment must be the description
       * of the function is definitely a hack and is specific to
       * BLAS/LAPACK.  we should find a more elegant solution.
       */
  
      /* if the previous statement is NULL (and we already know
       * that the current statement is a comment) then this must
       * be the first line of the comment block.
       *   OR
       * if the previous statement is non-NULL and is not Comment,
       * then this must be the first line of the comment block.
       */
      if(genJavadoc) {
        if( (root->prevstmt == NULL) ||
            (root->prevstmt != NULL &&
             root->prevstmt->nodetype != Comment &&
             root->prevstmt->nodetype != MainComment))
        {
          AST *ctemp;
  
          ctemp = root;
          root->astnode.ident.len = 0;
  
          while(ctemp != NULL && ctemp->nodetype == Comment) {
            root->astnode.ident.len++;
            ctemp = ctemp->nextstmt;
          }

          ctemp = cur_unit->astnode.source.javadocComments;

          if(ctemp == NULL) {
            root->nodetype = MainComment;
            cur_unit->astnode.source.javadocComments = root;
          } else if(root->astnode.ident.len > ctemp->astnode.ident.len) {
            ctemp->nodetype = Comment;
            root->nodetype = MainComment;
            cur_unit->astnode.source.javadocComments = root;
          }
        }
      }

      if (root->nextstmt != NULL)
        typecheck (root->nextstmt);
      break;
    case Common:
      fprintf(stderr,"Warning: hit case Common in typecheck()\n");
    case CommonList:
      common_check(root);
      if (root->nextstmt != NULL)
        typecheck (root->nextstmt);
      break;
    case Assignment:
      if (checkdebug)
        printf ("typecheck(): Assignment.\n");

      assign_check (root);

      if (root->nextstmt != NULL)
        typecheck (root->nextstmt);
      break;
    case Call:
      if (checkdebug)
        printf ("typecheck(): Call.\n");

      call_check (root);

      if (root->nextstmt != NULL)	/* End of typestmt list. */
        typecheck (root->nextstmt);
      break;
    case Forloop:
      if (checkdebug)
        printf ("typecheck(): Forloop.\n");

      forloop_check (root);

      if (root->nextstmt != NULL)	/* End of typestmt list. */
        typecheck (root->nextstmt);
      break;

    case Blockif:
      if (checkdebug)
        printf ("typecheck(): Blockif.\n");

      blockif_check (root);

      if (root->nextstmt != NULL)	/* End of typestmt list. */
        typecheck (root->nextstmt);
      break;
    case Elseif:
      if (checkdebug)
        printf ("typecheck(): Elseif.\n");

      elseif_check (root);

      if (root->nextstmt != NULL)	/* End of typestmt list. */
        typecheck (root->nextstmt);
      break;
    case Else:
      if (checkdebug)
        printf ("typecheck(): Else.\n");

      else_check (root);

      if (root->nextstmt != NULL)	/* End of typestmt list. */
        typecheck (root->nextstmt);
      break;
    case Logicalif:
      if (checkdebug)
        printf ("typecheck(): Logicalif.\n");

      logicalif_check (root);

      if (root->nextstmt != NULL)	/* End of typestmt list. */
        typecheck (root->nextstmt);
      break;
    case Write:
      if (checkdebug)
        printf ("typecheck(): Write statement.\n");
      read_write_check (root);
      if (root->nextstmt != NULL)
        typecheck (root->nextstmt);
      break;
    case Read:
      if (checkdebug)
        printf ("typecheck(): Read statement.\n");

      cur_unit->astnode.source.needs_input = TRUE;

      read_write_check (root);
      if (root->nextstmt != NULL)
        typecheck (root->nextstmt);
      break;
    case Constant:
    default:
      fprintf(stderr,"typecheck(): Error, bad nodetype (%s)\n",
         print_nodetype(root));
  }				/* switch on nodetype.  */
}

void
typedec_check (AST * root)
{
  AST *temp, *temp2;

  for(temp=root->astnode.typeunit.declist; temp != NULL; temp = temp->nextstmt)
  {
    if(temp->astnode.ident.arraylist != NULL) {

      temp2 = temp->astnode.ident.arraylist;
      for( ;temp2!=NULL;temp2=temp2->nextstmt) {
        if(temp2->nodetype == ArrayIdxRange) {
          expr_check(temp2->astnode.expression.lhs);
          expr_check(temp2->astnode.expression.rhs);
        }
        else
          expr_check(temp2);
      }
    }
  }
}

/*****************************************************************************
 *                                                                           *
 * merge_equivalences                                                        *
 *                                                                           *
 * ok, this is a very poorly written subroutine.  I admit it.                *
 * but I dont think that most programs will have a ton of equivalences       *
 * to merge, so it should not impose too much of a performance               *
 * penalty.  basically what we're doing here is looking at all               *
 * the equivalences in the unit and determining if some variable             *
 * is contained within more than one equivalence.   If so, we                *
 * merge those two equivalence statements.                                   *
 *                                                                           *
 *****************************************************************************/

void
merge_equivalences(AST *root)
{
  AST *temp, *ctemp;
  AST *temp2, *ctemp2;
  int needsMerge = FALSE;

  if(checkdebug)
    printf("M_EQV  Equivalences:\n");

  /* foreach equivalence statement... */
  for(temp=root; temp != NULL; temp = temp->nextstmt) {

    if(checkdebug)
      printf("M_EQV (%d)", temp->token);

    /* foreach variable in the equivalence statement... */
    for(ctemp=temp->astnode.equiv.clist;ctemp!=NULL;ctemp=ctemp->nextstmt) {
      if(checkdebug)
        printf(" %s, ", ctemp->astnode.ident.name);

      /* foreach equivalence statement (again)... */
      for(temp2=root;temp2!=NULL;temp2=temp2->nextstmt) {

        /* foreach variable in the second equivalence statement... */
        for(ctemp2=temp2->astnode.equiv.clist;ctemp2!=NULL;ctemp2=ctemp2->nextstmt) {

          if(!strcmp(ctemp->astnode.ident.name,ctemp2->astnode.ident.name) &&
            temp->token != temp2->token) 
          {
            /* the two names are the same, but arent in the same node.
             * the two equivalences pointed to by temp and temp2 should
             * be merged.
             */

            temp2->token = temp->token;
            needsMerge = TRUE;
          }
        }
      }
    }
    if(checkdebug)
      printf("\n");
  }

  /* if we dont need to merge anything, go ahead and return, skipping
   * this last chunk of code.
   */

  if(!needsMerge)
    return;

  /* 
   * Now we do the actual merging. 
   */

  /* foreach equivalence statement... */

  for(temp=root; temp != NULL; temp = temp->nextstmt) {

    /* foreach equivalence statement (again)... */
    for(temp2=root;temp2!=NULL;temp2=temp2->nextstmt) {

      if((temp->token == temp2->token) && (temp != temp2)) {

        /* the token pointers are equal and the nodes are distinct */

        /* loop until the end of the first equivalence list */

        ctemp=temp->astnode.equiv.clist;
        while(ctemp->nextstmt != NULL)
          ctemp = ctemp->nextstmt;

        /* add the second equivalence list to the end of the first */

        ctemp->nextstmt = temp2->astnode.equiv.clist;

        /* now remove the second equivalence list from the list of 
         * equivalences.
         */

        ctemp = root;
        while(ctemp->nextstmt != temp2)
          ctemp = ctemp->nextstmt;

        ctemp->nextstmt = temp2->nextstmt;

      }
    }

    /* the merging process may produce duplicate entries.  remove
     * them now.
     */

    remove_duplicates(temp->astnode.equiv.clist);
  }
}

/*****************************************************************************
 *                                                                           *
 * remove_duplicates                                                         *
 *                                                                           *
 * This function removes duplicate names from a list of idents.              *
 *                                                                           *
 *****************************************************************************/

void remove_duplicates(AST *root)
{
  AST *temp, *temp2, *prev;

  for(temp = root; temp != NULL; temp = temp->nextstmt) {
    prev = root;
    for(temp2 = root; temp2 != NULL; temp2 = temp2->nextstmt) {
      if(!strcmp(temp->astnode.ident.name,temp2->astnode.ident.name) &&
          temp != temp2) {
        prev->nextstmt = temp2->nextstmt;
      }
      prev = temp2;
    }
  }
}

/*****************************************************************************
 *                                                                           *
 * insertEquivalences                                                        *
 *                                                                           *
 * This function inserts the equivalenced variable names into the symbol     *
 * table.                                                                    *
 *                                                                           *
 *****************************************************************************/

void 
insertEquivalences(AST *root)
{
  AST *temp, *ctemp;
  AST *eqvList = root->astnode.source.equivalences;
  SYMTABLE *eqvSymTab = root->astnode.source.equivalence_table;
  char *merged_name;

  /* foreach equivalence statement... */
  for(temp = eqvList; temp != NULL; temp = temp->nextstmt) {

    /* merge the names in this list into one name */
    merged_name = merge_names(temp->astnode.equiv.clist);

    for(ctemp = temp->astnode.equiv.clist;ctemp!=NULL;ctemp = ctemp->nextstmt) {

      /* store the merged name into the node before sticking the node into
       * the symbol table.
       */

      ctemp->astnode.ident.merged_name = merged_name;

      type_insert(eqvSymTab, ctemp, Float, ctemp->astnode.ident.name);
    }
  }
}

/*****************************************************************************
 *                                                                           *
 * merge_names                                                               *
 *                                                                           *
 * This function merges a list of variable names into one name.  Basically   *
 * it just concatenates the names together, separated by an underscore.      *
 *                                                                           *
 *****************************************************************************/

char *
merge_names(AST *root)
{
  AST *temp;
  char *newName;
  unsigned int len = 0, num = 0;

  /* determine how long the merged name will be */

  for(temp = root;temp != NULL;temp=temp->nextstmt, num++)
    len += strlen(temp->astnode.ident.name);
  
  /* the length of the merged name is the sum of:
   *
   *  - the sum of the lengths of the variable names
   *  - the number of variables
   *  - one
   */

  newName = (char *)f2jalloc(len + num + 1);

  newName[0] = 0;

  /* foreach name in the list... */
  for(temp = root;temp != NULL;temp=temp->nextstmt, num++) {
    strcat(newName,temp->astnode.ident.name);
    if(temp->nextstmt != NULL)
      strcat(newName,"_");
  }

  return newName;
}

/*****************************************************************************
 *                                                                           *
 * check_equivalences                                                        *
 *                                                                           *
 * Perform typechecking on equivalences.  Loop through the equivalences and  *
 * look up the type in the symbol table.                                     *
 *                                                                           *
 *****************************************************************************/

void
check_equivalences(AST *root)
{
  AST *temp, *ctemp;
  enum returntype curType;
  HASHNODE *hashtemp;
  int mismatch = FALSE;

  for(temp=root; temp != NULL; temp = temp->nextstmt) {
    if(temp->astnode.equiv.clist != NULL) {
      hashtemp = type_lookup(chk_type_table,
                    temp->astnode.equiv.clist->astnode.ident.name);
      if(hashtemp)
        curType = hashtemp->variable->vartype;
      else
        continue;
    }
    else
      continue;

    for(ctemp=temp->astnode.equiv.clist;ctemp!=NULL;ctemp=ctemp->nextstmt) {
      hashtemp = type_lookup(chk_type_table,ctemp->astnode.ident.name);
      if(hashtemp) {
        if(hashtemp->variable->vartype != curType)
          mismatch = TRUE;
      }
      else
        continue;

      curType = hashtemp->variable->vartype;
    }
 
    if(mismatch) {
      fprintf(stderr, "Error with equivalenced variables: ");
      print_eqv_list(temp,stderr);
      fprintf(stderr,
       "...I can't handle equivalenced variables with differing types.\n");
    }
  }
}

/*****************************************************************************
 *                                                                           *
 * data_check                                                                *
 *                                                                           *
 * Perform typechecking of DATA statements.  Set the needs_declaration flag  *
 * of the node depending on whether it is an array or not.                   *
 *                                                                           *
 *****************************************************************************/

void 
data_check(AST * root)
{
  HASHNODE *hashtemp;
  AST *Dtemp, *Ntemp, *var;

  for(Dtemp = root->astnode.label.stmt; Dtemp != NULL; Dtemp = Dtemp->prevstmt)
  {
    for(Ntemp = Dtemp->astnode.data.nlist;Ntemp != NULL;Ntemp=Ntemp->nextstmt)
    {
      if(Ntemp->nodetype == DataImpliedLoop)
        var = Ntemp->astnode.forloop.Label;
      else
        var = Ntemp;

      name_check(var);

      hashtemp = type_lookup(chk_type_table,var->astnode.ident.name);

      if(hashtemp != NULL)
      {
        if((var->astnode.ident.arraylist != NULL) && 
           (type_lookup(chk_array_table,var->astnode.ident.name) != NULL))
          hashtemp->variable->astnode.ident.needs_declaration = TRUE;
        else
          hashtemp->variable->astnode.ident.needs_declaration = FALSE;

        var->vartype = hashtemp->variable->vartype;
      }
    }
  }
}

/*****************************************************************************
 *                                                                           *
 * common_check                                                              *
 *                                                                           *
 * Perform typechecking of COMMON statements.                                *
 *                                                                           *
 *****************************************************************************/

void
common_check(AST *root)
{
  HASHNODE *ht;
  AST *Ctemp, *Ntemp;
  int i;
  char **names;

  for(Ctemp=root->astnode.common.nlist;Ctemp!=NULL;Ctemp=Ctemp->nextstmt)
  {
    if(Ctemp->astnode.common.name != NULL)
    {
      if((ht=type_lookup(common_block_table, Ctemp->astnode.common.name))==NULL)
      {
        fprintf(stderr,"typecheck: can't find common block %s in table\n",
           Ctemp->astnode.common.name);
        continue;
      }

      names = (char **)ht->variable;

      i=0;
      for(Ntemp=Ctemp->astnode.common.nlist;Ntemp!=NULL;Ntemp=Ntemp->nextstmt,i++)
      {
        if (checkdebug)
        {
          printf("typecheck:Common block %s -- %s\n",Ctemp->astnode.common.name,
            Ntemp->astnode.ident.name);
          printf("typecheck:Looking up %s in the type table\n",
            Ntemp->astnode.ident.name);
        }

        if((ht=type_lookup(chk_type_table,Ntemp->astnode.ident.name)) == NULL)
        {
          fprintf(stderr,"typecheck Error: can't find type for common %s\n",
            Ntemp->astnode.ident.name);
          if (checkdebug)
            printf("Not Found\n");
          continue;
        }

        ht->variable->astnode.ident.merged_name = names[i];

        if(checkdebug)
          printf("# @#Typecheck: inserting %s into the type table, merged = %s\n",
            ht->variable->astnode.ident.name, 
            ht->variable->astnode.ident.merged_name);

        type_insert(chk_type_table,ht->variable,ht->variable->vartype,
          ht->variable->astnode.ident.name);
      }
    }
  }
}

/*****************************************************************************
 *                                                                           *
 * name_check                                                                *
 *                                                                           *
 * Perform typechecking of identifiers.                                      *
 *                                                                           *
 *****************************************************************************/

void
name_check (AST * root)
{
  HASHNODE *hashtemp;
  HASHNODE *ht;
  char * tempname;

  if (checkdebug)
    printf("here checking name %s\n",root->astnode.ident.name);

  tempname = strdup(root->astnode.ident.name);
  uppercase(tempname);

  /* If the name is in the external table, then check to see if
     it is an intrinsic function instead (e.g. SQRT, ABS, etc).  */

  if (checkdebug)
    printf("tempname = %s\n", tempname);

  if (type_lookup (chk_external_table, root->astnode.ident.name) != NULL)
  {
    if (checkdebug)
      printf("going to external_check\n");
    external_check(root);  /* handles LSAME, LSAMEN */
  }
  else if(( methodscan (intrinsic_toks, tempname) != NULL) 
    &&   ((type_lookup(chk_intrinsic_table,root->astnode.ident.name) != NULL)
       || (type_lookup(chk_type_table,root->astnode.ident.name) == NULL)))
  {
    if (checkdebug)
      printf("going to intrinsic_check\n");
    intrinsic_check(root);
  }
  else
  {
    if (checkdebug)
      printf("NOt intrinsic or external (%s)\n", root->astnode.ident.name);

    switch (root->token)
    {
      case STRING:
      case CHAR:
        if(checkdebug)
          printf("typecheck(): ** I am going to check a String/char literal!\n");
        break;
      case INTRINSIC: 
        /* do nothing */
        break;
      case NAME:
      default:
        hashtemp = type_lookup (chk_array_table, root->astnode.ident.name);

        if(checkdebug)
          printf("looking for %s in the type table\n",root->astnode.ident.name);

        if((ht = type_lookup(chk_type_table,root->astnode.ident.name)) != NULL)
        {
          if(checkdebug)
            printf("@# Found!\n");
          root->vartype = ht->variable->vartype;
        }
        else if( (cur_unit->nodetype == Function) &&
                 !strcmp(cur_unit->astnode.source.name->astnode.ident.name,
                         root->astnode.ident.name))
        {
          if(checkdebug)
          {
            printf("@# this is the implicit function var\n");
            printf("@# ...setting vartype = %s\n", 
               returnstring[cur_unit->astnode.source.returns]);
          }
          root->vartype = cur_unit->astnode.source.returns;
        }
        else
        {
          /* this is a hack for typechecking expressions within
           * an array declaration - just set type of * to Integer.
           */
          if(!strcmp(root->astnode.ident.name,"*")) {
            root->vartype = Integer;
          }
          else {
            fprintf(stderr,"Undeclared variable: %s\n",root->astnode.ident.name);
            root->vartype = 0;
          }
        }

        if (root->astnode.ident.arraylist == NULL)
          ; /* nothin for now */
        else if (hashtemp != NULL)
          array_check(root, hashtemp);
        else if (root->nodetype == Substring)
          root->vartype = String;
        else
          subcall_check(root);
    }
  }
}

/*****************************************************************************
 *                                                                           *
 * subcall_check                                                             *
 *                                                                           *
 *  This function checks a subroutine call.                                  *
 *                                                                           *
 *****************************************************************************/

void 
subcall_check(AST *root)
{
  AST *temp;
  char *tempstr;

  tempstr = strdup (root->astnode.ident.name);
  *tempstr = toupper (*tempstr);

  temp = root->astnode.ident.arraylist;

  for (; temp != NULL; temp = temp->nextstmt)
    if (*temp->astnode.ident.name != '*')
    {
      if(temp == NULL)
        fprintf(stderr,"subcall_check: calling expr_check with null pointer!\n");
      expr_check (temp);
    }
 
  /* 
   * here we need to figure out if this is a function
   * call and if so, what the return type is.  this will
   * require keeping track of all the functions/subroutines
   * during parsing.  and there will still be some that
   * we can't figure out.  
   *
   *  for now, we'll just assign integer to every call
   */ 

  root->vartype = Integer;
}

/*****************************************************************************
 *                                                                           *
 * func_array_check                                                          *
 *                                                                           *
 * Typecheck an array access.  This could be merged with array_check()...    *
 *                                                                           *
 *****************************************************************************/

void
func_array_check(AST *root, HASHNODE *hashtemp)
{

  if(root == NULL)
    fprintf(stderr,"func_array_check1: calling expr_check with null pointer!\n");

  expr_check (root);

  if(   (hashtemp->variable->astnode.ident.leaddim != NULL)
     && (hashtemp->variable->astnode.ident.leaddim[0] != '*')
     && (root->nextstmt != NULL))
  {
    expr_check (root->nextstmt);

    /* go up to 3 dimensions if necessary */
    if(root->nextstmt->nextstmt)
      expr_check (root->nextstmt->nextstmt);
  }
}

/*****************************************************************************
 *                                                                           *
 * array_check                                                               *
 *                                                                           *
 * Typecheck an array access.                                                *
 *                                                                           *
 *****************************************************************************/

void
array_check(AST *root, HASHNODE *hashtemp)
{
  AST *temp;

  if (checkdebug)
    printf ("typecheck(): Array... %s, My node type is %s\n", 
      root->astnode.ident.name,
      print_nodetype(root));

  temp = root->astnode.ident.arraylist;

  func_array_check(temp, hashtemp);
}

/*****************************************************************************
 *                                                                           *
 * external_check                                                            *
 *                                                                           *
 * Check an external variable.  If it is LSAME or LSAMEN, go ahead and       *
 * set the type to Logical.  else, go to call_check().
 *                                                                           *
 *****************************************************************************/
void
external_check(AST *root)
{
  char *tempname;
  AST *temp;

  tempname = strdup(root->astnode.ident.name);
  uppercase(tempname);

  /* first, make sure this isn't in the list of intrinsic functions... */

  if (methodscan(intrinsic_toks,tempname) == NULL)
  {
    if (root->astnode.ident.arraylist != NULL)
      call_check (root);
    return;
  }

  if (root->astnode.ident.arraylist != NULL)
  {
    /* this is some sort of intrinsic.  maybe it's LSAME or LSAMEN, which
     * are declared EXTERNAL since they really aren't intrinsics, but we
     * treat them as such since there is a corresponding Java function to
     * handle them.
     */

    if (!strcmp (tempname, "LSAME"))
    {
      temp = root->astnode.ident.arraylist;
      root->vartype = Logical;
      return;
    }
    else if (!strcmp (tempname, "LSAMEN"))
    {
      temp = root->astnode.ident.arraylist;

      name_check (temp->nextstmt->nextstmt);

      if(temp == NULL)
        fprintf(stderr,"external_check: calling expr_check with null pointer!\n");

      expr_check (temp);
      root->vartype = Logical;
      return;
    }
    else if( !strcmp(tempname, "ETIME") ) {
      expr_check (root->astnode.ident.arraylist);
      root->vartype = Double;
    }
    else if( !strcmp(tempname, "SECOND") ) {
      root->vartype = Double;
    }
  }
}

/*****************************************************************************
 *                                                                           *
 * intrinsic_check                                                           *
 *                                                                           *
 * Here we have an intrinsic to check.  We have to explicitly handle all     *
 * the intrinsics that we know about.  First determine which one we're       *
 * looking at and then assign a type depending on the return type of the     *
 * actual Java function (e.g. SQRT will return double because Math.sqrt()    *
 * returns double).                                                          *
 *                                                                           *
 *****************************************************************************/

void
intrinsic_check(AST *root)
{
  AST *temp;
  METHODTAB *entry;
  char *tempname, *javaname;
  enum _intrinsics id;
  enum returntype min_type = Integer;

  tempname = strdup(root->astnode.ident.name);
  uppercase(tempname);

  entry = methodscan (intrinsic_toks, tempname);

  if(!entry) {
    fprintf(stderr,"Error: not expecting null entry at this point.\n");
    exit(-1);
  }

  javaname = entry->java_method;
  id = entry->intrinsic;

  if(root->astnode.ident.arraylist == NULL)
    fprintf(stderr,"WARNING: intrinsic with no args!\n");

  /* check each argument to this intrinsic and determine the widest type
   * in case this is a generic intrinsic (so we may correctly determine
   * which typecasts to make).
   */
  if(root->astnode.ident.arraylist->nodetype != EmptyArgList) {
    for(temp = root->astnode.ident.arraylist;temp != NULL;temp=temp->nextstmt) {

      expr_check (temp);

      if(temp->vartype < min_type)
        min_type = temp->vartype;

      if(! (bitfields[temp->vartype] & entry->args)) {
        fprintf(stderr,"Error: bad argument type to intrinsic %s\n", 
                entry->fortran_name);
        exit(-1);
      }
    }
  }

  /* if this is a generic intrinsic, then set the return type of the
   * intrinsic to the type of the widest argument.
   */

  if(type_lookup(generic_table, intrinsic_toks[id].fortran_name) != NULL) {

    /* we must make a special case for type conversion intrinsics because
     * they always have the same return type regardless of whether the
     * generic form is used.
     */

    switch(id) {
      case ifunc_INT:
        root->vartype = Integer;
        break;
      case ifunc_REAL:
        root->vartype = Float;
        break;
      case ifunc_DBLE:
        root->vartype = Double;
        break;
      case ifunc_CMPLX:
        root->vartype = Complex;
        break;
      case ifunc_NINT:
        root->vartype = Integer;
        break;
      default:
        root->vartype = min_type;
        break; /* ansi c */
    }

  }
  else
    root->vartype = intrinsic_toks[id].ret;
}

/*****************************************************************************
 *                                                                           *
 * expr_check                                                                *
 *                                                                           *
 * Recursive procedure to check expressions.                                 *
 *                                                                           *
 *****************************************************************************/

void
expr_check (AST * root)
{
  if(root == NULL) {
    fprintf(stderr,"expr_check(): NULL root!\n");
    return;
  }

  switch (root->nodetype)
  {
    case Identifier:
      name_check (root);

      if (checkdebug)
        printf("hit case identifier (%s), now type is %s\n",
           root->astnode.ident.name,returnstring[root->vartype]);
      break;
    case Expression:
      if (root->astnode.expression.lhs != NULL)
        expr_check (root->astnode.expression.lhs);

      if(root->astnode.expression.rhs == NULL)
        fprintf(stderr,"expr_check: calling expr_check with null pointer!\n");

      expr_check (root->astnode.expression.rhs);

      root->vartype = root->astnode.expression.rhs->vartype;
      break;
    case Power:
      if(root->astnode.expression.lhs == NULL)
        fprintf(stderr,"expr_check: calling expr_check with null pointer!\n");

      expr_check (root->astnode.expression.lhs);

      if(root->astnode.expression.rhs == NULL)
        fprintf(stderr,"expr_check: calling expr_check with null pointer!\n");

      expr_check (root->astnode.expression.rhs);

      /*  vartype should always be double for pow since it is 
       *  translated to Math.pow(), which returns double.
       */

      root->vartype = Double;
      break;
    case Binaryop:
      if(root->astnode.expression.lhs == NULL)
        fprintf(stderr,"expr_check: calling expr_check with null pointer!\n");

      expr_check (root->astnode.expression.lhs);

      if(root->astnode.expression.rhs == NULL)
        fprintf(stderr,"expr_check: calling expr_check with null pointer!\n");

      expr_check (root->astnode.expression.rhs);

      if (checkdebug) {
         printf("here checking binaryOp...\n");
         printf("lhs type: %s\n", returnstring[root->astnode.expression.lhs->vartype]);
         printf("rhs type: %s\n", returnstring[root->astnode.expression.rhs->vartype]);
      }

      root->vartype = MIN(root->astnode.expression.lhs->vartype,
                          root->astnode.expression.rhs->vartype);
      break;
    case Unaryop:
      if(root->astnode.expression.rhs == NULL)
        fprintf(stderr,"expr_check: calling expr_check with null pointer!\n");

      expr_check (root->astnode.expression.rhs);

      root->vartype = root->astnode.expression.rhs->vartype;
      break;
    case Constant:
      /* constant's type is already known */
      break;
    case Logicalop:
      if (root->astnode.expression.lhs != NULL)
        expr_check (root->astnode.expression.lhs);

      if(root->astnode.expression.rhs == NULL)
        fprintf(stderr,"expr_check: calling expr_check with null pointer!\n");

      expr_check (root->astnode.expression.rhs);

      root->vartype = Logical;
      break;
    case Relationalop:
      if(root->astnode.expression.lhs == NULL)
        fprintf(stderr,"expr_check: calling expr_check with null pointer!\n");

      expr_check (root->astnode.expression.lhs);

      if(root->astnode.expression.rhs == NULL)
        fprintf(stderr,"expr_check: calling expr_check with null pointer!\n");

      expr_check (root->astnode.expression.rhs);

      root->vartype = Logical;
      break;
    case Substring:
      if(root->astnode.ident.arraylist == NULL)
        fprintf(stderr,"expr_check: calling expr_check with null pointer!\n");

      expr_check(root->astnode.ident.arraylist);

      if(root->astnode.ident.arraylist->nextstmt == NULL)
        fprintf(stderr,"expr_check: calling expr_check with null pointer!\n");

      expr_check(root->astnode.ident.arraylist->nextstmt);

      root->vartype = String;
      break;
    case EmptyArgList:
      /* do nothing */
      break;
    default:
      fprintf(stderr,"Warning: Unknown nodetype in expr_check(): %s\n",
        print_nodetype(root));
  }
}

/*****************************************************************************
 *                                                                           *
 * forloop_check                                                             *
 *                                                                           *
 * Check a DO loop.                                                          *
 *                                                                           *
 *****************************************************************************/

void
forloop_check (AST * root)
{

  expr_check (root->astnode.forloop.iter_expr);
  assign_check (root->astnode.forloop.incr_expr);

  assign_check (root->astnode.forloop.start);

  if(root->astnode.forloop.stop == NULL)
    fprintf(stderr,"forloop_check: calling expr_check with null pointer!\n");

  expr_check (root->astnode.forloop.stop);

  if (root->astnode.forloop.incr != NULL)
    expr_check (root->astnode.forloop.incr);
}


/*****************************************************************************
 *                                                                           *
 * logicalif_check                                                           *
 *                                                                           *
 * Check a Logical IF statement.                                             *
 *                                                                           *
 *****************************************************************************/

void
logicalif_check (AST * root)
{
  if (root->astnode.logicalif.conds != NULL)
    expr_check (root->astnode.logicalif.conds);

  typecheck (root->astnode.logicalif.stmts);
}

/*****************************************************************************
 *                                                                           *
 * read_write_check                                                          *
 *                                                                           *
 * Performs typechecking on READ and WRITE statements.                       *
 *                                                                           *
 *****************************************************************************/

void
read_write_check (AST * root)
{
  AST *temp;

  for(temp=root->astnode.io_stmt.arg_list;temp!=NULL;temp=temp->nextstmt)
  {
    if(temp->nodetype == IoImpliedLoop)
      check_implied_loop(temp);
    else
      expr_check (temp);
  }
}

/*****************************************************************************
 *                                                                           *
 * check_implied_loop                                                        *
 *                                                                           *
 * Performs typechecking on an implied DO loop.                              *
 *                                                                           *
 *****************************************************************************/

void 
check_implied_loop(AST *node)
{
  expr_check(node->astnode.forloop.Label);
  expr_check(node->astnode.forloop.iter_expr);
  assign_check(node->astnode.forloop.incr_expr);
}

/*****************************************************************************
 *                                                                           *
 * blockif_check                                                             *
 *                                                                           *
 * Check a block IF statement, including elseif and else blocks.             *
 *                                                                           *
 *****************************************************************************/

void
blockif_check (AST * root)
{
  AST *temp;

  if (root->astnode.blockif.conds != NULL)
    expr_check (root->astnode.blockif.conds);

  if (root->astnode.blockif.stmts != NULL)
    typecheck (root->astnode.blockif.stmts);

  for(temp = root->astnode.blockif.elseifstmts; temp != NULL; temp = temp->nextstmt)
    elseif_check (temp);

  if (root->astnode.blockif.elsestmts != NULL)
    else_check (root->astnode.blockif.elsestmts);
}

/*****************************************************************************
 *                                                                           *
 * elseif_check                                                              *
 *                                                                           *
 * Check the "else if" of a block IF statement.  This is short enough to     *
 * be inlined with blockif_check at some point.                              *
 *                                                                           *
 *****************************************************************************/

void
elseif_check (AST * root)
{
  if (root->astnode.blockif.conds != NULL)
    expr_check (root->astnode.blockif.conds);
  typecheck (root->astnode.blockif.stmts);
}

/*****************************************************************************
 *                                                                           *
 * elseif_check                                                              *
 *                                                                           *
 * Check the "else if" of a block IF statement.  This is definitely short    *
 * enough to be inlined with blockif_check at some point.                    *
 *                                                                           *
 *****************************************************************************/

void
else_check (AST * root)
{
  typecheck (root->astnode.blockif.stmts);
}

/*****************************************************************************
 *                                                                           *
 * call_check                                                                *
 *                                                                           *
 * Check a function/subroutine call.  This node's type is based on the       *
 * declaration in the original Fortran code.                                 *
 *                                                                           *
 *****************************************************************************/

void
call_check (AST * root)
{
  AST *temp;
  HASHNODE *ht;

  assert (root != NULL);
  if(root->astnode.ident.arraylist == NULL)
    return;

  if(checkdebug)
    printf("the name of this function/subroutine is %s\n",
         root->astnode.ident.name);

  /* now is a convenient time to determine whether we should import the
   * BLAS library.
   */

  if(type_lookup(blas_routine_table,root->astnode.ident.name))
    cur_unit->astnode.source.needs_blas = TRUE;

  if( (ht = type_lookup(chk_type_table,root->astnode.ident.name)) != NULL)
  {
    if(checkdebug)
      printf("SETting type to %s\n", returnstring[ht->variable->vartype]);

    root->vartype = ht->variable->vartype;
  }

  temp = root->astnode.ident.arraylist;
  while (temp->nextstmt != NULL)
  {
    if(temp == NULL)
      fprintf(stderr,"call_check: calling expr_check with null pointer!\n");

    expr_check (temp);
    temp = temp->nextstmt;
  }

  if(temp == NULL)
    fprintf(stderr,"call_check: calling expr_check with null pointer!\n");

  expr_check (temp);

}

/*****************************************************************************
 *                                                                           *
 * assign_check                                                              *
 *                                                                           *
 * Check an assignment statement.  This info is very important to the code   *
 * generator.                                                                *
 *                                                                           *
 *****************************************************************************/

void
assign_check (AST * root)
{
  name_check (root->astnode.assignment.lhs);

  if(root->astnode.assignment.rhs == NULL)
    fprintf(stderr,"assign_check: calling expr_check with null pointer!\n");

  expr_check (root->astnode.assignment.rhs);
}
