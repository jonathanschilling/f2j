/* typecheck.c */

#include<stdio.h>
#include<string.h>
#include<ctype.h>
#include"f2j.h"
#include"f2jparse.tab.h"

char * strdup ( const char * );
char * print_nodetype ( AST * ); 
void elseif_check(AST *);
void else_check (AST *);

int checkdebug = FALSE;

#define MIN(x,y) ((x)<(y)?(x):(y))

SYMTABLE *chk_type_table;
SYMTABLE *chk_external_table;
SYMTABLE *chk_intrinsic_table;
SYMTABLE *chk_array_table;

char *methodscan (METHODTAB *, char *);

extern char *returnstring[]; 

AST *cur_unit;

void
typecheck (AST * root)
{
  void data_check(AST *);
  void common_check(AST *);
  void assign_check (AST *);
  void call_check (AST *);
  void forloop_check (AST *);
  void blockif_check (AST *);
  void logicalif_check (AST *);
  void write_check (AST *);
  void read_check (AST *);
  void merge_equivalences(AST *);
  void check_equivalences(AST *);
  void insertEquivalences(AST *);
  SYMTABLE *new_symtable(int);

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
    case Typedec:
    case Specification:
    case Statement:
    case Return:
    case Goto:
    case ComputedGoto:
    case Format:
    case Stop:
    case Save:
    case Comment:
    case Unimplemented:
      if (checkdebug)
        printf ("typecheck(): %s.\n", print_nodetype(root));

      if (root->nextstmt != NULL)
        typecheck (root->nextstmt);
      break;
    case Common:
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
      write_check (root);
      if (root->nextstmt != NULL)
        typecheck (root->nextstmt);
      break;
    case Read:
      if (checkdebug)
        printf ("typecheck(): Read statement.\n");

      cur_unit->astnode.source.needs_input = TRUE;

      read_check (root);
      if (root->nextstmt != NULL)
        typecheck (root->nextstmt);
      break;
    case Constant:
    default:
      fprintf(stderr,"typecheck(): Error, bad nodetype (%s)\n",
         print_nodetype(root));
  }				/* switch on nodetype.  */
}

/* merge_equivalences
 *
 *  ok, this is a very poorly written subroutine.  I admit it.
 * but I dont think that most programs will have a ton of equivalences
 * to merge, so it should not impose too much of a performance
 * penalty.  basically what we're doing here is looking at all
 * the equivalences in the unit and determining if some variable
 * is contained within more than one equivalence.   If so, we
 * merge those two equivalence statements.
 */

void
merge_equivalences(AST *root)
{
  AST *temp, *ctemp;
  AST *temp2, *ctemp2;
  int needsMerge = FALSE;
  void remove_duplicates(AST *);

  if(checkdebug)
    printf("M_EQV  Equivalences:\n");

  for(temp=root; temp != NULL; temp = temp->nextstmt) {
    if(checkdebug)
      printf("M_EQV (%d)", temp->token);

    for(ctemp=temp->astnode.equiv.clist;ctemp!=NULL;ctemp=ctemp->nextstmt) {
      if(checkdebug)
        printf(" %s, ", ctemp->astnode.ident.name);

      for(temp2=root;temp2!=NULL;temp2=temp2->nextstmt) {
        for(ctemp2=temp2->astnode.equiv.clist;ctemp2!=NULL;ctemp2=ctemp2->nextstmt) {
          if(!strcmp(ctemp->astnode.ident.name,ctemp2->astnode.ident.name) &&
            temp->token != temp2->token) {
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

  for(temp=root; temp != NULL; temp = temp->nextstmt) {
    for(temp2=root;temp2!=NULL;temp2=temp2->nextstmt) {
      if((temp->token == temp2->token) && (temp != temp2)) {
        ctemp=temp->astnode.equiv.clist;
        while(ctemp->nextstmt != NULL)
          ctemp = ctemp->nextstmt;

        ctemp->nextstmt = temp2->astnode.equiv.clist;

        ctemp = root;
        while(ctemp->nextstmt != temp2)
          ctemp = ctemp->nextstmt;

        ctemp->nextstmt = temp2->nextstmt;

      }
    }
    remove_duplicates(temp->astnode.equiv.clist);
  }
}

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

void 
insertEquivalences(AST *root)
{
  int idx;
  AST *temp, *ctemp;
  AST *eqvList = root->astnode.source.equivalences;
  SYMTABLE *eqvSymTab = root->astnode.source.equivalence_table;
  char *merged_name;
  int hash(char *);
  void type_insert (HASHNODE **, AST *, int, char *);
  char *merge_names(AST *);

  for(temp = eqvList; temp != NULL; temp = temp->nextstmt) {
    merged_name = merge_names(temp->astnode.equiv.clist);
    for(ctemp = temp->astnode.equiv.clist;ctemp!=NULL;ctemp = ctemp->nextstmt) {
      ctemp->astnode.ident.merged_name = merged_name;
      idx = hash(ctemp->astnode.ident.name) % eqvSymTab->num_entries;
      type_insert( &(eqvSymTab->entry[idx]), ctemp, Float, ctemp->astnode.ident.name);
    }
  }
}

char *
merge_names(AST *root)
{
  AST *temp;
  char *newName;
  int len = 0, num = 0;
  char * malloc(int);

  for(temp = root;temp != NULL;temp=temp->nextstmt, num++)
    len += strlen(temp->astnode.ident.name);
  
  newName = (char *)malloc(len + num + 1);

  if(!newName) {
    fprintf(stderr,"Unsuccessful malloc.\n");
    exit(-1);
  }

  newName[0] = 0;

  for(temp = root;temp != NULL;temp=temp->nextstmt, num++) {
    strcat(newName,temp->astnode.ident.name);
    if(temp->nextstmt != NULL)
      strcat(newName,"_");
  }

  return newName;
}

void
check_equivalences(AST *root)
{
  AST *temp, *ctemp;
  enum returntype curType;
  HASHNODE *hashtemp;
  int mismatch = FALSE;
  void print_eqv_list(AST *, FILE *);

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

void 
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

void
common_check(AST *root)
{
  HASHNODE *ht;
  AST *Ctemp, *Ntemp;
  int i,idx;
  char **names;
  int hash (char *);
  void type_insert (HASHNODE **, AST *, int, char *);

  for(Ctemp=root;Ctemp!=NULL;Ctemp=Ctemp->nextstmt)
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
        idx = hash(ht->variable->astnode.ident.name)%chk_type_table->num_entries;

        if(checkdebug)
          printf("# @#Typecheck: inserting %s into the type table, merged = %s\n",
            ht->variable->astnode.ident.name, 
            ht->variable->astnode.ident.merged_name);

        type_insert(&(chk_type_table->entry[idx]),ht->variable,ht->variable->vartype,
          ht->variable->astnode.ident.name);
      }
    }
  }
}

void
name_check (AST * root)
{
  HASHNODE *hashtemp;
  HASHNODE *ht;
  char * tempname;
  extern METHODTAB intrinsic_toks[];
  void external_check(AST *);
  void intrinsic_check(AST *);
  void array_check(AST *, HASHNODE *);
  void subcall_check(AST *);

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

void 
subcall_check(AST *root)
{
  AST *temp;
  char *tempstr;
  void expr_check (AST *);

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

void
func_array_check(AST *root, HASHNODE *hashtemp)
{
  void expr_check (AST *);

      if(root == NULL)
        fprintf(stderr,"func_array_check1: calling expr_check with null pointer!\n");
  expr_check (root);

  if(   (hashtemp->variable->astnode.ident.leaddim != NULL)
     && (hashtemp->variable->astnode.ident.leaddim[0] != '*')
     && (root->nextstmt != NULL))
  {
    root = root->nextstmt;
      if(root == NULL)
        fprintf(stderr,"func_array_check2: calling expr_check with null pointer!\n");
    expr_check (root);
  }
}

void
array_check(AST *root, HASHNODE *hashtemp)
{
  AST *temp;
  void func_array_check(AST *, HASHNODE *);

  if (checkdebug)
    printf ("typecheck(): Array... %s, My node type is %s\n", 
      root->astnode.ident.name,
      print_nodetype(root));

  temp = root->astnode.ident.arraylist;

  func_array_check(temp, hashtemp);
}

void
external_check(AST *root)
{
  extern METHODTAB intrinsic_toks[];
  char *tempname, *javaname;
  AST *temp;
  void name_check (AST *);
  void expr_check (AST *);
  void call_check (AST *);

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
      if(temp == NULL)
        fprintf(stderr,"external_check: calling expr_check with null pointer!\n");
      expr_check (temp);
      root->vartype = Logical;
      return;
    }
  }
}

void
intrinsic_check(AST *root)
{
  extern METHODTAB intrinsic_toks[];
  AST *temp;
  char *tempname, *javaname;
  void expr_check (AST *);

  tempname = strdup(root->astnode.ident.name);
  uppercase(tempname);

  javaname = (char *)methodscan (intrinsic_toks, tempname);

  if (!strcmp (tempname, "MAX"))
  {

    if(checkdebug)
      printf("here we are in MAX.  arraylist is %s\n", 
        (root->astnode.ident.arraylist == NULL) ? " NULL ": " non-NULL ");

    temp = root->astnode.ident.arraylist;

    if(checkdebug) {
      printf("temp is %s\n", (temp == NULL) ? " NULL ": " non-NULL ");

      printf("temp->next is %s\n",
         (temp->nextstmt == NULL) ? " NULL ": " non-NULL ");
    }

    if(temp == NULL)
      fprintf(stderr,"MAX1: calling expr_check with null pointer!\n");

    expr_check (temp);

    if(temp->nextstmt == NULL)
      fprintf(stderr,"MAX2: calling expr_check with null pointer!\n");

    expr_check (temp->nextstmt);
    root->vartype = Double;

    return;
  }

  if (!strcmp (tempname, "MIN"))
  {
    temp = root->astnode.ident.arraylist;
      if(temp == NULL)
        fprintf(stderr,"MIN: calling expr_check with null pointer!\n");
    expr_check (temp);
      if(temp->nextstmt == NULL)
        fprintf(stderr,"MIN2: calling expr_check with null pointer!\n");
    expr_check (temp->nextstmt);
    root->vartype = Double;
    return;
  }

  if (!strcmp (tempname, "ABS"))
  {
    temp = root->astnode.ident.arraylist;
      if(temp == NULL)
        fprintf(stderr,"ABS: calling expr_check with null pointer!\n");
    expr_check (temp);
    root->vartype = Double;
    return;
  }

  if (!strcmp (tempname, "DABS"))
  {
    temp = root->astnode.ident.arraylist;
      if(temp == NULL)
        fprintf(stderr,"DABS: calling expr_check with null pointer!\n");
    expr_check (temp);
    root->vartype = Double;
    return;
  }

  if (!strcmp (tempname, "DSQRT")
   || !strcmp (tempname, "SIN")
   || !strcmp (tempname, "EXP")
   || !strcmp (tempname, "COS"))
  {
    temp = root->astnode.ident.arraylist;
      if(temp == NULL)
        fprintf(stderr,"DSQRT,etc: calling expr_check with null pointer!\n");
    expr_check (temp);
    root->vartype = Double;
    return;
  }

  if (!strcmp (tempname, "SQRT")
   || !strcmp (tempname, "LOG")
   || !strcmp (tempname, "LOG10"))
  {
    temp = root->astnode.ident.arraylist;
      if(temp == NULL)
        fprintf(stderr,"SQRT,etc: calling expr_check with null pointer!\n");
    expr_check (temp);
    root->vartype = Double;
    return;
  }

  if(!strcmp (tempname, "MOD"))
  {
    temp = root->astnode.ident.arraylist;
      if(temp == NULL)
        fprintf(stderr,"MOD: calling expr_check with null pointer!\n");
    expr_check(temp);
      if(temp->nextstmt == NULL)
        fprintf(stderr,"MOD2: calling expr_check with null pointer!\n");
    expr_check(temp->nextstmt);

    root->vartype = Integer;
    return;
  }

  if(!strcmp (tempname, "SIGN"))
  {
    temp = root->astnode.ident.arraylist;
      if(temp == NULL)
        fprintf(stderr,"SIGN: calling expr_check with null pointer!\n");
    expr_check(temp);
      if(temp->nextstmt == NULL)
        fprintf(stderr,"SIGN2: calling expr_check with null pointer!\n");
    expr_check(temp->nextstmt);

    root->vartype = Double;
    return;
  }

  if (!strcmp (tempname, "CHAR"))
  {
    temp = root->astnode.ident.arraylist;
      if(temp == NULL)
        fprintf(stderr,"CHAR: calling expr_check with null pointer!\n");
    expr_check(temp);
    root->vartype = Character;
    return;
  }

  if (!strcmp (tempname, "ICHAR")
   || !strcmp (tempname, "INT")
   || !strcmp (tempname, "LEN")
   || !strcmp (tempname, "NINT"))
  {
    temp = root->astnode.ident.arraylist;
      if(temp == NULL)
        fprintf(stderr,"%s: calling expr_check with null pointer!\n",tempname);
    expr_check(temp);
    root->vartype = Integer;
    return;
  }

  if(!strcmp (tempname, "REAL") ||
     !strcmp (tempname, "DBLE"))
  {
    temp = root->astnode.ident.arraylist;
      if(temp == NULL)
        fprintf(stderr,"REAL: calling expr_check with null pointer!\n");
    expr_check(temp);
    root->vartype = Double;
    return;
  }
}

void
expr_check (AST * root)
{
  void name_check (AST *);

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

void
forloop_check (AST * root)
{
  void assign_check (AST *);
  void expr_check (AST *);

  assign_check (root->astnode.forloop.start);

      if(root->astnode.forloop.stop == NULL)
        fprintf(stderr,"forloop_check: calling expr_check with null pointer!\n");
  expr_check (root->astnode.forloop.stop);

  if (root->astnode.forloop.incr != NULL)
    expr_check (root->astnode.forloop.incr);

}


void
logicalif_check (AST * root)
{
  void expr_check (AST *);

  if (root->astnode.logicalif.conds != NULL)
    expr_check (root->astnode.logicalif.conds);

  typecheck (root->astnode.logicalif.stmts);
}

void
read_check (AST * root)
{
  AST *temp;
  void expr_check (AST *);
  void check_implied_loop(AST *);

  for(temp=root->astnode.io_stmt.arg_list;temp!=NULL;temp=temp->nextstmt)
  {
    if(temp->nodetype == ImpliedLoop)
      check_implied_loop(temp);
    else
      expr_check (temp);
  }
}

void 
check_implied_loop(AST *node)
{
  expr_check(node->astnode.forloop.start);
  expr_check(node->astnode.forloop.stop);
  if(node->astnode.forloop.incr != NULL)
    expr_check(node->astnode.forloop.incr);

  expr_check(node->astnode.forloop.Label);
}

void
write_check (AST * root)
{
  AST *temp;
  void expr_check (AST *);

  for(temp=root->astnode.io_stmt.arg_list;temp!=NULL;temp=temp->nextstmt)
  {
      if(temp == NULL)
        fprintf(stderr,"write_check: calling expr_check with null pointer!\n");
    if(temp->nodetype != ImpliedLoop)
      expr_check (temp);
  }
}

void
blockif_check (AST * root)
{
  void expr_check (AST *);

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
  void expr_check (AST *);

  if (root->astnode.blockif.conds != NULL)
    expr_check (root->astnode.blockif.conds);
  typecheck (root->astnode.blockif.stmts);
}

void
else_check (AST * root)
{
  typecheck (root->astnode.blockif.stmts);
}

void
call_check (AST * root)
{
  AST *temp;
  HASHNODE *ht;
  void expr_check (AST *);

  assert (root != NULL);
  if(root->astnode.ident.arraylist == NULL)
    return;

  if(checkdebug)
    printf("the name of this function/subroutine is %s\n",
         root->astnode.ident.name);

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

void
assign_check (AST * root)
{
  void name_check (AST *);
  void expr_check (AST *);

  name_check (root->astnode.assignment.lhs);
      if(root->astnode.assignment.rhs == NULL)
        fprintf(stderr,"assign_check: calling expr_check with null pointer!\n");

  expr_check (root->astnode.assignment.rhs);

}
