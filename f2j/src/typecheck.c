/* typecheck.c */

#include<stdio.h>
#include<string.h>
#include<ctype.h>
#include"f2j.h"
#include"f2jparse.tab.h"
#include"list.h"

char * strdup ( const char * );
char * print_nodetype ( AST * ); 
void elseif_check(AST *);
void else_check (AST *);

int checkdebug = 1;

#define MIN(x,y) ((x)<(y)?(x):(y))

SYMTABLE *chk_type_table;
SYMTABLE *chk_external_table;
SYMTABLE *chk_array_table;

char *methodscan (METHODTAB *, char *);

extern char *returnstring[]; 

AST *cur_unit;

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
      chk_array_table = root->astnode.source.array_table;

      typecheck (root->astnode.source.progtype);
      typecheck (root->astnode.source.typedecs);
      typecheck (root->astnode.source.statements);

      break;
    case Subroutine:
    case Function:
    case Program:
      cur_unit = root;
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
    case Typedec:
    case Specification:
    case Statement:
    case Return:
    case Goto:
    case ComputedGoto:
    case Format:
    case Stop:
    case Save:
    case Common:
    case Unimplemented:
      if (checkdebug)
        printf ("typecheck(): %s.\n", print_nodetype(root));

      if (root->nextstmt != NULL)	/* End of typestmt list. */
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
      write_check (root);
      if (root->nextstmt != NULL)
        typecheck (root->nextstmt);
      break;
    case Constant:
    default:
      fprintf(stderr,"typecheck(): Error, bad nodetype (%s)\n",
         print_nodetype(root));
  }				/* switch on nodetype.  */
}

int 
data_check(AST * root)
{
  HASHNODE *hashtemp;
  AST *Dtemp,*Ntemp;

  for(Dtemp = root->astnode.label.stmt; Dtemp != NULL; Dtemp = Dtemp->prevstmt)
  {
    for(Ntemp = Dtemp->astnode.data.nlist;Ntemp != NULL;Ntemp=Ntemp->nextstmt)
    {
      hashtemp = type_lookup(chk_type_table,Ntemp->astnode.ident.name);

      if(hashtemp != NULL)
      {
        if((Ntemp->astnode.ident.arraylist != NULL) && 
           (type_lookup(chk_array_table,Ntemp->astnode.ident.name) != NULL))
          hashtemp->variable->astnode.ident.needs_declaration = TRUE;
        else
          hashtemp->variable->astnode.ident.needs_declaration = FALSE;
      }
    }
  }
}

int
name_check (AST * root)
{
  AST *temp;
  HASHNODE *hashtemp;
  HASHNODE *ht;
  char *javaname, * tempname;
  extern METHODTAB intrinsic_toks[];

  if (checkdebug)
    printf("here checking name %s\n",root->astnode.ident.name);

  tempname = strdup(root->astnode.ident.name);
  uppercase(tempname);

  /* If the name is in the external table, then check to see if
     it is an intrinsic function instead (e.g. SQRT, ABS, etc).  */

printf("tempname = %s\n", tempname);

  if (type_lookup (chk_external_table, root->astnode.ident.name) != NULL)
  {
printf("going to external_check\n");
    external_check(root);  /* handles LSAME, LSAMEN */
  }
  else if( methodscan (intrinsic_toks, tempname) != NULL) 
  {
printf("going to intrinsic_check\n");
    intrinsic_check(root);
  }
  else
  {
printf("NOt intrinsic or external\n");
    switch (root->token)
    {
      case STRING:
      case CHAR:
        if(checkdebug)
          printf("typecheck(): ** I am going to emit a String/char literal!\n");
        break;
      case INTRINSIC: 
        /* do nothing */
        break;
      case NAME:
      default:
        hashtemp = type_lookup (chk_array_table, root->astnode.ident.name);

        printf("@# looking for %s in the type table\n", root->astnode.ident.name);
        if( (ht = type_lookup(chk_type_table,root->astnode.ident.name)) != NULL )
        {
          printf("@# Found!\n");
          root->vartype = ht->variable->vartype;
        }
        else if( (cur_unit->nodetype == Function) &&
                 !strcmp(cur_unit->astnode.source.name->astnode.ident.name,
                         root->astnode.ident.name))
        {
          printf("@# this is the implicit function var\n");
          printf("@# ...setting vartype = %s\n", 
             returnstring[cur_unit->astnode.source.returns]);
          root->vartype = cur_unit->astnode.source.returns;
        }
        else
        {
          fprintf(stderr,"Undeclared variable: %s\n",root->astnode.ident.name);
          root->vartype = 0;
        }

        if (root->astnode.ident.arraylist == NULL)
          ; /* nothin for now */
        else if (hashtemp != NULL)
          array_check(root, hashtemp);
        else
          subcall_check(root);
    }
  }
}

/*  This function emits a subroutine call */

int 
subcall_check(AST *root)
{
  AST *temp;
  char *tempstr;

  tempstr = strdup (root->astnode.ident.name);
  *tempstr = toupper (*tempstr);

  temp = root->astnode.ident.arraylist;

  for (temp; temp != NULL; temp = temp->nextstmt)
    if (*temp->astnode.ident.name != '*')
      expr_check (temp);
 
   /* 
     here we need to figure out if this is a function
     call and if so, what the return type is.  this will
     require keeping track of all the functions/subroutines
     during parsing.  and there will still be some that
     we can't figure out.  

     for now, we'll just assign integer to every call
   */ 

  root->vartype = Integer;
}

int
func_array_check(AST *root, HASHNODE *hashtemp)
{
  expr_check (root);

  if (hashtemp->variable->astnode.ident.leaddim[0] != '*' && root->nextstmt != NULL)
  {
    root = root->nextstmt;
    expr_check (root);
  }
}

int
array_check(AST *root, HASHNODE *hashtemp)
{
  extern METHODTAB intrinsic_toks[];
  AST *temp;
  int is_arg=FALSE;

  if (checkdebug)
    printf ("typecheck(): Array... %s, My node type is %s\n", 
      root->astnode.ident.name,
      print_nodetype(root));

  temp = root->astnode.ident.arraylist;

  func_array_check(temp, hashtemp);
}

int
external_check(AST *root)
{
  extern METHODTAB intrinsic_toks[];
  char *tempname, *javaname;
  AST *temp;

  tempname = strdup(root->astnode.ident.name);
  uppercase(tempname);

  javaname = (char *) methodscan (intrinsic_toks, tempname);

  if (javaname == NULL)
  {
    if (root->astnode.ident.arraylist != NULL)
      call_check (root);
    return;
  }

  if (root->astnode.ident.arraylist != NULL)
  {
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
      expr_check (temp);
      root->vartype = Logical;
      return;
    }
  }
}

int
intrinsic_check(AST *root)
{
  extern METHODTAB intrinsic_toks[];
  AST *temp;
  char *tempname, *javaname;

  tempname = strdup(root->astnode.ident.name);
  uppercase(tempname);

  javaname = (char *)methodscan (intrinsic_toks, tempname);

  if (!strcmp (tempname, "MAX"))
  {

printf("here we are in MAX.  arraylist is %s\n", 
  (root->astnode.ident.arraylist == NULL) ? " NULL ": " non-NULL ");

    temp = root->astnode.ident.arraylist;

printf("temp is %s\n", (temp == NULL) ? " NULL ": " non-NULL ");

printf("temp->next is %s\n",
  (temp->nextstmt == NULL) ? " NULL ": " non-NULL ");

    expr_check (temp);
    expr_check (temp->nextstmt);
    root->vartype = Double;
    return;
  }

  if (!strcmp (tempname, "MIN"))
  {
    temp = root->astnode.ident.arraylist;
    expr_check (temp);
    expr_check (temp->nextstmt);
    root->vartype = Double;
    return;
  }

  if (!strcmp (tempname, "ABS"))
  {
    temp = root->astnode.ident.arraylist;
    expr_check (temp);
    root->vartype = Double;
    return;
  }

  if (!strcmp (tempname, "DABS"))
  {
    temp = root->astnode.ident.arraylist;
    expr_check (temp);
    root->vartype = Double;
    return;
  }

  if (!strcmp (tempname, "DSQRT"))
  {
    temp = root->astnode.ident.arraylist;
    expr_check (temp);
    root->vartype = Double;
    return;
  }

  if(!strcmp (tempname, "SQRT"))
  {
    temp = root->astnode.ident.arraylist;
    expr_check (temp);
    root->vartype = Double;
    return;
  }

  if (!strcmp (tempname, "MOD"))
  {
    temp = root->astnode.ident.arraylist;
    expr_check(temp);
    expr_check(temp->nextstmt);
    root->vartype = Integer;
    return;
  }

  if (!strcmp (tempname, "CHAR"))
  {
    temp = root->astnode.ident.arraylist;
    expr_check(temp);
    root->vartype = Character;
    return;
  }

  if (!strcmp (tempname, "ICHAR"))
  {
    temp = root->astnode.ident.arraylist;
    expr_check(temp);
    root->vartype = Integer;
    return;
  }

  if (!strcmp (tempname, "INT"))
  {
    temp = root->astnode.ident.arraylist;
    expr_check(temp);
    root->vartype = Integer;
    return;
  }

  if (!strcmp (tempname, "REAL"))
  {
    temp = root->astnode.ident.arraylist;
    expr_check(temp);
    root->vartype = Double;
    return;
  }
}

int
expr_check (AST * root)
{
  extern METHODTAB intrinsic_toks[];
  char *tempname;

  if(root == NULL) {
    fprintf(stderr,"expr_check(): NULL root!\n");
    return 0;
  }

  switch (root->nodetype)
  {
    case Identifier:
      name_check (root);
      printf("EXPR, root->vartype = %d\n", root->vartype);
      if (checkdebug)
        printf("hit case identifier (%s), now type is %s\n",
           root->astnode.ident.name,returnstring[root->vartype]);
      break;
    case Expression:
      if (root->astnode.expression.lhs != NULL)
        expr_check (root->astnode.expression.lhs);
      expr_check (root->astnode.expression.rhs);
      root->vartype = root->astnode.expression.rhs->vartype;
      break;
    case Power:
      expr_check (root->astnode.expression.lhs);
      expr_check (root->astnode.expression.rhs);
      root->vartype = MIN(root->astnode.expression.lhs->vartype,
                          root->astnode.expression.rhs->vartype);
      break;
    case Binaryop:
      expr_check (root->astnode.expression.lhs);
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
      expr_check (root->astnode.expression.rhs);
      root->vartype = root->astnode.expression.rhs->vartype;
      break;
    case Constant:
      /* constant's type is already known */
      break;
    case Logicalop:
      if (root->astnode.expression.lhs != NULL)
        expr_check (root->astnode.expression.lhs);
      expr_check (root->astnode.expression.rhs);
      root->vartype = Logical;
      break;
    case Relationalop:
      expr_check (root->astnode.expression.lhs);
      expr_check (root->astnode.expression.rhs);
      root->vartype = Logical;
      break;
    case Substring:
      expr_check(root->astnode.ident.arraylist);
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

int
forloop_check (AST * root)
{
  assign_check (root->astnode.forloop.start);

  expr_check (root->astnode.forloop.stop);

  if (root->astnode.forloop.incr != NULL)
    expr_check (root->astnode.forloop.incr);

  typecheck (root->astnode.forloop.stmts);
}


logicalif_check (AST * root)
{
  if (root->astnode.logicalif.conds != NULL)
    expr_check (root->astnode.logicalif.conds);

  typecheck (root->astnode.logicalif.stmts);
}

int
write_check (AST * root)
{
  AST *temp;

  for(temp=root->astnode.io_stmt.arg_list;temp!=NULL;temp=temp->nextstmt)
    expr_check (temp);
}

int
blockif_check (AST * root)
{

  if (root->astnode.blockif.conds != NULL)
    expr_check (root->astnode.blockif.conds);

  typecheck (root->astnode.blockif.stmts);

  if (root->astnode.blockif.elseifstmts != NULL)
    typecheck (root->astnode.blockif.elseifstmts);

  if (root->astnode.blockif.elsestmts != NULL)
    typecheck (root->astnode.blockif.elsestmts);
}

void
elseif_check (AST * root)
{
  if (root->astnode.blockif.conds != NULL)
    expr_check (root->astnode.blockif.conds);
  typecheck (root->astnode.blockif.stmts);
}

void
else_check (AST * root)
{
  typecheck (root->astnode.blockif.stmts);
}

int
call_check (AST * root)
{
  AST *temp;
  HASHNODE *ht;

  assert (root != NULL);
  if(root->astnode.ident.arraylist == NULL)
    return;

printf("the name of this function/subroutine is %s\n",
         root->astnode.ident.name);
if( (ht = type_lookup(chk_type_table,root->astnode.ident.name)) != NULL)
{
  printf("SETting type to %s\n", returnstring[ht->variable->vartype]);
  root->vartype = ht->variable->vartype;
}

  temp = root->astnode.ident.arraylist;
  while (temp->nextstmt != NULL)
  {
    expr_check (temp);
    temp = temp->nextstmt;
  }
  expr_check (temp);
}

int
assign_check (AST * root)
{
  name_check (root->astnode.assignment.lhs);
  expr_check (root->astnode.assignment.rhs);

printf("## ## typecheck: rtype = %s\n",
  returnstring[root->astnode.assignment.rhs->vartype]);
}
