/*
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */


/*****************************************************************************
 * vcg_emitter.c                                                             *
 *                                                                           *
 * Emits a graph representing the syntax tree for the                        *
 * fortran program.  The file is compatible with the                         *
 * VCG tool (Visualization of Compiler Graphs).                              *
 * I'm afraid this routine is horribly out of date.                          *
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
  * strdup(const char *),
  * lowercase(char *);

void 
  start_vcg(AST *),
  emit_vcg(AST *,int),
  vcg_elseif_emit(AST *,int),
  vcg_else_emit(AST *,int),
  vcg_typedec_emit (AST *, int),
  vcg_spec_emit (AST *, int),
  vcg_assign_emit (AST *, int),
  vcg_call_emit (AST *, int),
  vcg_forloop_emit (AST *, int),
  vcg_blockif_emit (AST *, int),
  vcg_logicalif_emit (AST *, int),
  vcg_label_emit (AST *, int),
  vcg_expr_emit (AST *, int);

int
  vcg_name_emit (AST *, int);

METHODTAB 
  * methodscan (METHODTAB *, char *);

/*****************************************************************************
 * Global variables.                                                         *
 *****************************************************************************/

int 
  vcg_debug = FALSE,            /* set to TRUE to get debugging output       */
  node_num = 1;                 /* initialize node counter                   */

char 
  temp_buf[200],                /* temporary buffer for node titles          */
  * returnname;                 /* return type of the current program unit   */

extern METHODTAB intrinsic_toks[];

/*****************************************************************************
 *                                                                           *
 * start_vcg                                                                 *
 *                                                                           *
 * Print graph header (width, height, etc.) and call emit_vcg() to generate  *
 * the rest of the graph.                                                    *
 *                                                                           *
 *****************************************************************************/

void 
start_vcg(AST *root)
{
  /* print header information */
  print_vcg_header(vcgfp, "SYNTAX TREE");

  emit_vcg(root, 0);

  print_vcg_trailer(vcgfp);
}
  
/*****************************************************************************
 *                                                                           *
 * print_vcg_header                                                          *
 *                                                                           *
 * this function prints the VCG header, with the given title.                *
 *                                                                           *
 *****************************************************************************/

void
print_vcg_header(FILE *gfp, char *title)
{
  fprintf(gfp,"graph: { title: \"%s\"\n", title);

  fprintf(gfp,"x: 30\n");
  fprintf(gfp,"y: 30\n");
  fprintf(gfp,"width:  850\n");
  fprintf(gfp,"height: 800\n");
  fprintf(gfp,"color: lightcyan\n");

  fprintf(gfp,"stretch: 4\n");
  fprintf(gfp,"shrink: 10\n");
  fprintf(gfp,"layout_upfactor: 10\n");
  fprintf(gfp,"manhatten_edges: yes\n");
  fprintf(gfp,"smanhatten_edges: yes\n");
  fprintf(gfp,"layoutalgorithm: tree\n\n");

  fprintf(gfp,"node: {color: black textcolor: white title:\"f2j\"\n");
  fprintf(gfp,"label: \"Nothing should hang here\"\n");
  fprintf(gfp,"}\n\n");
}

/*****************************************************************************
 *                                                                           *
 * print_vcg_trailer                                                         *
 *                                                                           *
 * this function prints the VCG trailer.                                     *
 *                                                                           *
 *****************************************************************************/

void
print_vcg_trailer(FILE *gfp)
{
  fprintf(gfp,"}\n");
}

/*****************************************************************************
 *                                                                           *
 * print_vcg_node                                                            *
 *                                                                           *
 * Given a number and a label, this function prints a node specification.    *
 *                                                                           *
 *****************************************************************************/

void
print_vcg_node(FILE *gfp, int num, char *label)
{
  if(vcg_debug)
    printf("creating node \"%s\"\n",label);

  fprintf(gfp,
    "node: {color: black textcolor: white title:\"%d\"\n",num);

  fprintf(gfp,
    "label: \"%s\"\n",label);

  fprintf(gfp,
    "}\n\n");

  node_num++;
}

/*****************************************************************************
 *                                                                           *
 * print_vcg_typenode                                                        *
 *                                                                           *
 * Similar to print_vcg_node except that this function prints a special      *
 * "typenode", which acts as as annotation to the graph (showing type info). *
 *                                                                           *
 *****************************************************************************/

void
print_vcg_typenode(FILE *gfp, int num, char *label)
{
  if(vcg_debug)
    printf("creating typenode \"%s\"\n",label);

  fprintf(gfp, "node: { title: \"%d\"\n",num);
  fprintf(gfp, " label: \"%s\"\n",label);
  fprintf(gfp, "}\n\n");

  node_num++;
}

/*****************************************************************************
 *                                                                           *
 * print_vcg_edge                                                            *
 *                                                                           *
 * Given the source and destination node numbers, this function emits an     *
 * edge to connect them.                                                     *
 *                                                                           *
 *****************************************************************************/

void
print_vcg_edge(FILE *gfp, int source, int dest)
{
  fprintf(gfp,
    "edge: { thickness: 6 color: red sourcename: \"%d\" targetname: \"%d\"}\n\n",
    source, dest);
}

/*****************************************************************************
 *                                                                           *
 * print_vcg_nearedge                                                        *
 *                                                                           *
 * Similar to print_vcg_edge except that this function emits a "nearedge",   *
 * which tells VCG to try to keep the nodes close together.                  *
 *                                                                           *
 *****************************************************************************/

void 
print_vcg_nearedge(FILE *gfp, int source, int dest)
{
  fprintf(gfp,"nearedge: { sourcename: \"%d\" targetname: \"%d\"\n",
      source, dest);
  fprintf(gfp,"color: blue thickness: 6\n}\n\n");
}

/*****************************************************************************
 *                                                                           *
 * emit_vcg                                                                  *
 *                                                                           *
 * This is the main VCG generation function.  We traverse the                *
 * AST and recursively call emit_vcg() on each node.  This                   *
 * function figures out what kind of node it's looking at and                *
 * calls the appropriate function to handle the graph generation.            *
 *                                                                           *
 *****************************************************************************/

void
emit_vcg (AST * root, int parent)
{
  int my_node = node_num;

  switch (root->nodetype)
  {
    case 0:
      fprintf(stderr,"Bad node in emit_vcg()\n");
      emit_vcg (root->nextstmt,node_num);
    case Progunit:
      if(vcg_debug)
        printf("case Source\n");

      print_vcg_node(vcgfp, node_num,"Progunit");

      if(vcg_debug)
        printf("case Source: Going to emit PROGTYPE\n");

      emit_vcg (root->astnode.source.progtype, my_node);

      if(vcg_debug)
        printf("case Source: Going to emit TYPEDECS\n");

      emit_vcg (root->astnode.source.typedecs, my_node);

      if(vcg_debug)
        printf("case Source: Going to emit STATEMENTS\n");

      emit_vcg (root->astnode.source.statements, my_node);

      break;
    case Subroutine:
      if(vcg_debug)
        printf("case Subroutine\n");

      print_vcg_node(vcgfp, node_num,"Subroutine");
      print_vcg_edge(vcgfp, parent, my_node);

      returnname = NULL;	/* Subroutines return void. */
      break;
    case Function:
      if(vcg_debug)
        printf("case Function\n");

      sprintf (temp_buf,"Function: %s\n", 
        root->astnode.source.name->astnode.ident.name);
      print_vcg_node(vcgfp, node_num,temp_buf);
      print_vcg_edge(vcgfp, parent, my_node);
      returnname = root->astnode.source.name->astnode.ident.name;
      break;
    case Typedec:
      if(vcg_debug)
        printf("case Typedec\n");

      vcg_typedec_emit (root, parent);
      if (root->nextstmt != NULL)	/* End of typestmt list. */
        emit_vcg (root->nextstmt, my_node);
      break;
    case Specification:
      if(vcg_debug)
        printf("case Specification\n");

      vcg_spec_emit (root, parent);
      if (root->nextstmt != NULL)	/* End of typestmt list. */
        emit_vcg (root->nextstmt, my_node);
      break;
    case Statement:
      if(vcg_debug)
        printf("case Statement\n");

      print_vcg_node(vcgfp, node_num,"Statement");
      print_vcg_edge(vcgfp, parent, my_node);
      if (root->nextstmt != NULL)	/* End of typestmt list. */
        emit_vcg (root->nextstmt, my_node);
      break;

    case Assignment:
      print_vcg_node(vcgfp, node_num,"Assignment");
      print_vcg_edge(vcgfp, parent, my_node);
      vcg_assign_emit (root, my_node);
      if (root->nextstmt != NULL)
        emit_vcg (root->nextstmt, my_node);
      break;
    case Call:
      vcg_call_emit (root, parent);
      if (root->nextstmt != NULL)	/* End of typestmt list. */
        emit_vcg (root->nextstmt, my_node);
      break;
    case Forloop:
      print_vcg_node(vcgfp, node_num,"For loop");
      print_vcg_edge(vcgfp, parent, my_node);

      vcg_forloop_emit (root, my_node);

      if (root->nextstmt != NULL)	/* End of typestmt list. */
        emit_vcg (root->nextstmt, my_node);
      break;
    case Blockif:
      print_vcg_node(vcgfp, node_num,"Block if");
      print_vcg_edge(vcgfp, parent, my_node);

      vcg_blockif_emit (root, my_node);

      if (root->nextstmt != NULL)	/* End of typestmt list. */
        emit_vcg (root->nextstmt, my_node);
      break;
    case Elseif:
      print_vcg_node(vcgfp, node_num,"Else if");
      print_vcg_edge(vcgfp, parent, my_node);

      vcg_elseif_emit (root, my_node);

      if (root->nextstmt != NULL)	/* End of typestmt list. */
        emit_vcg (root->nextstmt, my_node);
      break;
    case Else:
      print_vcg_node(vcgfp, node_num,"Else");
      print_vcg_edge(vcgfp, parent, my_node);

      vcg_else_emit (root, my_node);

      if (root->nextstmt != NULL)	/* End of typestmt list. */
        emit_vcg (root->nextstmt, my_node);
      break;
    case Logicalif:
      print_vcg_node(vcgfp, node_num,"Logical If");
      print_vcg_edge(vcgfp, parent, my_node);

      vcg_logicalif_emit (root, my_node);

      if (root->nextstmt != NULL)	/* End of typestmt list. */
        emit_vcg (root->nextstmt, my_node);
      break;
    case Return:
      if (returnname != NULL)
        sprintf (temp_buf, "Return (%s)", returnname);
      else
        sprintf (temp_buf, "Return");

      print_vcg_node(vcgfp, node_num,temp_buf);
      print_vcg_edge(vcgfp, parent, my_node);

      if (root->nextstmt != NULL)	/* End of typestmt list. */
        emit_vcg (root->nextstmt, my_node);
      break;
    case Goto:
      sprintf (temp_buf,"Goto (%d)", root->astnode.go_to.label);
      print_vcg_node(vcgfp, node_num,temp_buf);
      print_vcg_edge(vcgfp, parent, my_node);

      if (root->nextstmt != NULL)
        emit_vcg (root->nextstmt, my_node);
      break;
    case Label:
      vcg_label_emit (root, parent);

      if (root->nextstmt != NULL)	/* End of typestmt list. */
        emit_vcg (root->nextstmt, my_node);
      break;
    case End:
      print_vcg_node(vcgfp, node_num,"End");
      print_vcg_edge(vcgfp, parent, my_node);

      /* end of the program */
      break;
    case Unimplemented:
      print_vcg_node(vcgfp, node_num,"UNIMPLEMENTED");
      print_vcg_edge(vcgfp, parent, my_node);

      if (root->nextstmt != NULL)
        emit_vcg (root->nextstmt, my_node);
      break;
    case Constant:
      sprintf(temp_buf,"Constant(%s)",
      root->astnode.constant.number);
      
      print_vcg_node(vcgfp, node_num,temp_buf);
      print_vcg_edge(vcgfp, parent, my_node);
    default:
      fprintf (stderr,"vcg_emitter: Default case reached!\n");
  }				/* switch on nodetype.  */
}

/*****************************************************************************
 *                                                                           *
 * vcg_typedec_emit                                                          *
 *                                                                           *
 * Emit all the type declaration nodes.                                      *
 *                                                                           *
 *****************************************************************************/

void
vcg_typedec_emit (AST * root, int parent)
{
  AST *temp;
  enum returntype returns;
  int my_node = node_num;
  int name_nodenum = 0;
  int prev_node = 0;

  if(vcg_debug)
    printf("in vcg_typedec_emit\n");

  temp = root->astnode.typeunit.declist;

  /* This may have to be moved into the looop also.  Could be
   * why I have had problems with this stuff.  
   */

  if(type_lookup (external_table, temp->astnode.ident.name))
  {
    if(vcg_debug) {
      printf("returning from vcg_typedec_emit,");
      printf(" found something in hash table\n");
    }
    print_vcg_node(vcgfp, node_num,"External");
    print_vcg_edge(vcgfp, parent, my_node);
    return;
  } 

  returns = root->astnode.typeunit.returns;

  sprintf(temp_buf,"TypeDec (%s)", returnstring[returns]);
  print_vcg_node(vcgfp, node_num,temp_buf);
  print_vcg_edge(vcgfp, parent, my_node);

  prev_node = my_node;

  for (; temp != NULL; temp = temp->nextstmt) {
    if(vcg_debug)
      printf("in the loop\n");
    name_nodenum = vcg_name_emit (temp, parent);
    print_vcg_nearedge(vcgfp, prev_node,name_nodenum);
    prev_node = name_nodenum;
  }
  if(vcg_debug)
    printf("leaving vcg_typdec_emit\n");
}

/*****************************************************************************
 *                                                                           *
 * vcg_name_emit                                                             *
 *                                                                           *
 * Generate an identifier node.                                              *
 *                                                                           *
 *****************************************************************************/

int
vcg_name_emit (AST * root, int parent)
{
  AST *temp;
  HASHNODE *hashtemp;
  char *javaname, * tempname;
  int my_node = node_num;
  int temp_num;
  METHODTAB *entry;

  if(vcg_debug)
    printf("in vcg_name_emit\n");

  sprintf(temp_buf,"Name (%s)",root->astnode.ident.name);
  print_vcg_node(vcgfp, my_node,temp_buf);

  /* Check to see whether name is in external table.  Names are
   * loaded into the external table from the parser.   
   */

  /* If the name is in the external table, then check to see if
   * is an intrinsic function instead.  
   */

  if(type_lookup (external_table, root->astnode.ident.name))
  {

    /*  This block of code is only called if the identifier
     *  absolutely does not have an entry in any table,
     *  and corresponds to a method invocation of
     *  something in the blas or lapack packages.  
     */

    if (methodscan(intrinsic_toks,root->astnode.ident.name) == NULL) {
      if (root->astnode.ident.arraylist != NULL) {
        vcg_call_emit (root, my_node);
        return my_node;
      }
      return my_node;
    }

    if (root->astnode.ident.arraylist != NULL) {
      if (!strcmp (root->astnode.ident.name, "LSAME")) {
        temp = root->astnode.ident.arraylist;
        temp_num = vcg_name_emit (temp->nextstmt, my_node);
        print_vcg_edge(vcgfp, my_node,temp_num);
        return my_node;
       }
    }
  }

  tempname = strdup(root->astnode.ident.name);
  uppercase(tempname);

  if(vcg_debug)
    printf ("Tempname  %s\n", tempname);

  entry = methodscan (intrinsic_toks, tempname);
  javaname = entry->java_method;
	  
  if (javaname != NULL) {
    if (!strcmp (root->astnode.ident.name, "MAX")) {
      temp = root->astnode.ident.arraylist;

      vcg_expr_emit (temp, my_node);
      vcg_expr_emit (temp->nextstmt, my_node);
      return my_node;
    }

    if (!strcmp (root->astnode.ident.name, "MIN")) {
      temp = root->astnode.ident.arraylist;
      vcg_expr_emit (temp, my_node);
      vcg_expr_emit (temp->nextstmt, my_node);
      return my_node;
    }

    if (!strcmp (root->astnode.ident.name, "ABS")) {
      temp = root->astnode.ident.arraylist;
      vcg_expr_emit (temp, my_node);
      return my_node;
    }

    if (!strcmp (tempname, "DABS")) {
      temp = root->astnode.ident.arraylist;
      vcg_expr_emit (temp, my_node);
      return my_node;
    }

    if (!strcmp (tempname, "DSQRT")) {
      temp = root->astnode.ident.arraylist;
      vcg_expr_emit (temp, my_node);
      return my_node;
    }
  }

  hashtemp = type_lookup (array_table, root->astnode.ident.name);

  switch (root->token)
  {
    case STRING:
      /*fprintf (javafp, "\"%s\"", root->astnode.ident.name); */
      break;

    case CHAR:
      /*fprintf (javafp, "\"%s\"", root->astnode.ident.name); */
      break;

    case NAME:

    default:
      /* At some point in here I will have to switch on the
         token type check whether it is a variable or
         string or character literal. Also have to look up whether
         name is intrinsic or external.  */

      if (root->astnode.ident.arraylist == NULL) {
        /* null */   ;
        /* fprintf (javafp, "%s", root->astnode.ident.name); */
      }
      else if (hashtemp != NULL) {
        if(vcg_debug)
          printf ("Array... %s\n", root->astnode.ident.name);

        temp = root->astnode.ident.arraylist;

        /* Now, what needs to happen here is the context of the
         * array needs to be determined.  If the array is being
         * passed as a parameter to a method, then the array index
         * needs to be passed separately and the array passed as
         * itself.  If not, then an array value is being set,
         * so dereference with index arithmetic.  
         */

        /*fprintf (javafp, "["); */

        vcg_expr_emit (temp, my_node);

        if (hashtemp->variable->astnode.ident.leaddim[0] != '*' &&
                 temp->nextstmt != NULL) {
          temp = temp->nextstmt;

          /*fprintf (javafp, "+"); */

          vcg_expr_emit (temp, my_node);

          /*
            fprintf (javafp, "*"); 
            fprintf(javafp,  "%s", hashtemp->variable->astnode.ident.leaddim);
          */
        }
        /*fprintf(javafp, "]"); */
      }
      else {
        /*fprintf (javafp, "%s", root->astnode.ident.name); */
        temp = root->astnode.ident.arraylist;

        for (; temp != NULL; temp = temp->nextstmt) {
          /*fprintf (javafp, "["); */

          if (*temp->astnode.ident.name != '*')
            vcg_expr_emit (temp, my_node);

          /*fprintf (javafp, "]"); */
        }
      }
    break;
  }
  return my_node;
}

/*****************************************************************************
 *                                                                           *
 * vcg_expr_emit                                                             *
 *                                                                           *
 * Recursive function to generate an expression graph.                       *
 *                                                                           *
 *****************************************************************************/

void
vcg_expr_emit (AST * root, int parent)
{
  int my_node = node_num;
  int temp_num;

  switch (root->nodetype)
  {
    case Identifier:
      print_vcg_node(vcgfp, my_node,"Ident");
      print_vcg_edge(vcgfp, parent,my_node);

      temp_num = vcg_name_emit (root, my_node);

      print_vcg_edge(vcgfp, my_node,temp_num);
      break;
    case Expression:
      if (root->astnode.expression.lhs != NULL)
        vcg_expr_emit (root->astnode.expression.lhs, parent);

      vcg_expr_emit (root->astnode.expression.rhs, parent);
      break;
    case Power:
      print_vcg_node(vcgfp, my_node,"pow()");
      print_vcg_edge(vcgfp, parent,my_node);

      vcg_expr_emit (root->astnode.expression.lhs, my_node);
      vcg_expr_emit (root->astnode.expression.rhs, my_node);
      break;
    case Binaryop:
      sprintf(temp_buf,"%c", root->astnode.expression.optype);

      print_vcg_node(vcgfp, my_node,temp_buf);
      print_vcg_edge(vcgfp, parent,my_node);

      vcg_expr_emit (root->astnode.expression.lhs, my_node);
      vcg_expr_emit (root->astnode.expression.rhs, my_node);
      break;
    case Unaryop:
      sprintf(temp_buf,"%c", root->astnode.expression.minus);

      print_vcg_node(vcgfp, my_node,temp_buf);
      print_vcg_edge(vcgfp, parent,my_node);

      vcg_expr_emit (root->astnode.expression.rhs, my_node);
      break;
    case Constant:
      sprintf(temp_buf,"Constant(%s)", root->astnode.constant.number);

      print_vcg_node(vcgfp, node_num,temp_buf);
      print_vcg_edge(vcgfp, parent, my_node);
      break;
    case Logicalop:
      if(root->token == AND)
        print_vcg_node(vcgfp, my_node,"AND");
      else if(root->token == OR)
        print_vcg_node(vcgfp, my_node,"OR");
           
      if (root->astnode.expression.lhs == NULL)
        print_vcg_node(vcgfp, my_node,"NOT");

      print_vcg_edge(vcgfp, parent,my_node);

      if (root->astnode.expression.lhs != NULL)
        vcg_expr_emit (root->astnode.expression.lhs, my_node);

      vcg_expr_emit (root->astnode.expression.rhs, my_node);
      break;
    case Relationalop:
      switch (root->token)
      {
        case rel_eq:
          print_vcg_node(vcgfp, my_node,"==");
          break;
        case rel_ne:
          print_vcg_node(vcgfp, my_node,"!=");
          break;
        case rel_lt:
          print_vcg_node(vcgfp, my_node,"<");
          break;
        case rel_le:
          print_vcg_node(vcgfp, my_node,"<=");
          break;
        case rel_gt:
          print_vcg_node(vcgfp, my_node,">");
          break;
        case rel_ge:
          print_vcg_node(vcgfp, my_node,">=");
          break;
        default:
          print_vcg_node(vcgfp, my_node,"Unknown RelationalOp");
      }
      print_vcg_edge(vcgfp, parent,my_node);

      vcg_expr_emit (root->astnode.expression.lhs, my_node);
      vcg_expr_emit (root->astnode.expression.rhs, my_node);
      break;
    default:
          fprintf(stderr,"vcg_emitter: Bad node in vcg_expr_emit\n");
  }
}

/*****************************************************************************
 *                                                                           *
 * vcg_forloop_emit                                                          *
 *                                                                           *
 * Generate the graph for a DO loop.                                         *
 *                                                                           *
 *****************************************************************************/

void
vcg_forloop_emit (AST * root, int parent)
{
  vcg_assign_emit (root->astnode.forloop.start, parent);
  vcg_expr_emit (root->astnode.forloop.stop, parent);

  if (root->astnode.forloop.incr != NULL) {
    vcg_expr_emit (root->astnode.forloop.incr, parent);
  }

/*  emit_vcg (root->astnode.forloop.stmts, parent); */
}

/*****************************************************************************
 *                                                                           *
 * vcg_logicalif_emit                                                        *
 *                                                                           *
 * Generates the graph nodes for a logical IF statement.                     *
 *                                                                           *
 *****************************************************************************/

void
vcg_logicalif_emit (AST * root, int parent)
{
  if (root->astnode.logicalif.conds != NULL)
    vcg_expr_emit (root->astnode.logicalif.conds, parent);

  emit_vcg (root->astnode.logicalif.stmts,parent);
}

/*****************************************************************************
 *                                                                           *
 * vcg_label_emit                                                            *
 *                                                                           *
 * Generate the node for a label.                                            *
 *                                                                           *
 *****************************************************************************/

void
vcg_label_emit (AST * root, int parent)
{
  int my_node = node_num;

  sprintf(temp_buf,"Label (%d)",root->astnode.label.number);

  print_vcg_node(vcgfp, node_num,temp_buf);
  print_vcg_edge(vcgfp, parent, my_node);

  if (root->astnode.label.stmt != NULL)
    emit_vcg (root->astnode.label.stmt,my_node);
}

/*****************************************************************************
 *                                                                           *
 * vcg_blockif_emit                                                          *
 *                                                                           *
 * Generates the nodes for a Block IF statement.                             *
 *                                                                           *
 *****************************************************************************/

void
vcg_blockif_emit (AST * root, int parent)
{
  AST *temp;

  if (root->astnode.blockif.conds != NULL)
    vcg_expr_emit (root->astnode.blockif.conds, parent);

  if (root->astnode.blockif.stmts != NULL)
    emit_vcg (root->astnode.blockif.stmts,parent);

  for(temp = root->astnode.blockif.elseifstmts; temp != NULL; temp = temp->nextstmt)
    vcg_elseif_emit (root->astnode.blockif.elseifstmts,parent);

  if (root->astnode.blockif.elsestmts != NULL)
    vcg_else_emit (root->astnode.blockif.elsestmts,parent);
}

/*****************************************************************************
 *                                                                           *
 * vcg_elseif_emit                                                           *
 *                                                                           *
 * Generates the nodes for an else if block.                                 *
 *                                                                           *
 *****************************************************************************/

void
vcg_elseif_emit (AST * root, int parent)
{
  if (root->astnode.blockif.conds != NULL)
    vcg_expr_emit (root->astnode.blockif.conds, parent);

  emit_vcg (root->astnode.blockif.stmts,parent);
}

/*****************************************************************************
 *                                                                           *
 * vcg_else_emit                                                             *
 *                                                                           *
 * Generates the nodes for an else if block.                                 *
 *                                                                           *
 *****************************************************************************/

void
vcg_else_emit (AST * root, int parent)
{
  emit_vcg (root->astnode.blockif.stmts,parent);
}

/*****************************************************************************
 *                                                                           *
 * vcg_call_emit                                                             *
 *                                                                           *
 * Generate the nodes for a function/subroutine call.                        *
 *                                                                           *
 *****************************************************************************/

void
vcg_call_emit (AST * root, int parent)
{
  AST *temp;
  char *tempname;
  int my_node = node_num;

  assert (root != NULL);

  lowercase (root->astnode.ident.name);
  tempname = strdup (root->astnode.ident.name);
  *tempname = toupper (*tempname);

  sprintf(temp_buf,"Call (%s)",root->astnode.ident.name);
  print_vcg_node(vcgfp, node_num,temp_buf);
  print_vcg_edge(vcgfp, parent, my_node);

  assert (root->astnode.ident.arraylist != NULL);

  temp = root->astnode.ident.arraylist;

  while (temp->nextstmt != NULL) {
    vcg_expr_emit (temp, parent);
    temp = temp->nextstmt;
  }

  vcg_expr_emit (temp, parent);
}

/*****************************************************************************
 *                                                                           *
 * vcg_spec_emit                                                             *
 *                                                                           *
 * Generate the nodes for a specification statement.                         *
 *                                                                           *
 *****************************************************************************/

void
vcg_spec_emit (AST * root, int parent)
{
  AST *assigntemp;
  int my_node = node_num;
  int temp_num;

  if(vcg_debug)
    printf("in vcg_spec_emit, my_node = %d, parent = %d\n",
      my_node,parent);

  print_vcg_node(vcgfp, node_num,"Specification");
  print_vcg_edge(vcgfp, parent, my_node);

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
      for (; assigntemp; assigntemp = assigntemp->nextstmt)
        vcg_assign_emit (assigntemp, parent);
      break;

    case Intrinsic:
      temp_num = vcg_name_emit (root, parent);
      print_vcg_edge(vcgfp, my_node, temp_num);
      break;
    case External:
    case Implicit:
      /* do nothing */
      break;
  }
}

/*****************************************************************************
 *                                                                           *
 * vcg_assign_emit                                                           *
 *                                                                           *
 * Generate the nodes for an assignment statement.                           *
 *                                                                           *
 *****************************************************************************/

void
vcg_assign_emit (AST * root, int parent)
{
  int temp_num;

  temp_num = vcg_name_emit (root->astnode.assignment.lhs, parent);
  print_vcg_edge(vcgfp, parent,temp_num);
  vcg_expr_emit (root->astnode.assignment.rhs, parent);
}
