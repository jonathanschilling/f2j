
/*****************************************************************************
 * constant_pool.c                                                           *
 *                                                                           *
 * This file contains routines for manipulating the constant pool list.      *
 *                                                                           *
 *****************************************************************************/

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include"constant_pool.h"


/*****************************************************************************
 *                                                                           *
 * cp_lookup                                                                 *
 *                                                                           *
 * Searches for the given node in the specified constant pool list.          *
 * If found, return a pointer to the node.  Return NULL otherwise.           *
 *                                                                           *
 *****************************************************************************/

CPNODE *
cp_lookup(Dlist list, char *tag) {
  Dlist temp;

  dl_traverse(temp,list) {
    if( ((CPNODE *)temp->val)->id  && !strcmp( ((CPNODE *)temp->val)->id, tag) )
      return temp->val;
  }

  return NULL;
}

/*****************************************************************************
 *                                                                           *
 * cp_insert                                                                 *
 *                                                                           *
 * Inserts the given node into the constant pool list.                       *
 *   list  - list into which the node should be inserted                     *
 *   node  - the node to be inserted                                         *
 *   tag   - string identification tag for this node (used for searching)    *
 *   width - number of constant pool entries occupied by this node (for      *
 *            double & long, this should be 2, all else should be 1).        *
 *                                                                           *
 *****************************************************************************/

int
cp_insert(Dlist list, struct cp_info *node, char *tag, char width) {
  char *strdup(const char *);
  CPNODE * n;

  n = (CPNODE *)malloc(sizeof(CPNODE));

  n->id = (tag == NULL) ? NULL : strdup(tag);
  n->val = node;
  n->index = dl_empty(list) ? 1 : ((CPNODE *) dl_last(list)->val)->next_idx;
  n->next_idx = n->index + width;

  dl_insert_b(list, n);

  return n->index;
}

/*****************************************************************************
 *                                                                           *
 * cp_dump                                                                   *
 *                                                                           *
 * Prints the contents of the constant pool list.                            *
 *                                                                           *
 *****************************************************************************/

void
cp_dump(Dlist list)
{
  extern char *constant_tags[NUM_CONSTANT_TAGS];
  CPNODE * tmpconst;
  Dlist tmpPtr;
  double x;

  dl_traverse(tmpPtr,list) {
    tmpconst = (CPNODE *) tmpPtr->val;

    printf("Constant pool entry %d:\n", tmpconst->index);
    printf("\ttag: %s\n", constant_tags[tmpconst->val->tag]);
    switch(tmpconst->val->tag) {
      case CONSTANT_Utf8:
        printf("\tstring: %s\n",tmpconst->val->cpnode.Utf8.bytes);
        break;
      case CONSTANT_Integer:
        printf("\tint: %d\n",tmpconst->val->cpnode.Integer.bytes);
        break;
      case CONSTANT_Float:
        printf("\tfloat: %f\n",(float)tmpconst->val->cpnode.Float.bytes);
        break;
      case CONSTANT_Long:
        printf("\tlong: no long value, shouldn't hit this case!\n");
        break;
      case CONSTANT_Double:
        memcpy(&x,&tmpconst->val->cpnode.Double.high_bytes,sizeof(u4));
        memcpy((char*)&x+4,&tmpconst->val->cpnode.Double.low_bytes,sizeof(u4));
        printf("\tdouble: %f (high: %d, low: %d)\n",x,
           tmpconst->val->cpnode.Double.high_bytes, 
           tmpconst->val->cpnode.Double.low_bytes);
        break;
      case CONSTANT_Class:
        printf("\tclass index: %d\n",tmpconst->val->cpnode.Class.name_index);
        break;
      case CONSTANT_String:
        printf("\tstring index: %d\n",tmpconst->val->cpnode.String.string_index);
        break;
      case CONSTANT_Fieldref:
        printf("\tclass index(declaring this field): %d\n",
            tmpconst->val->cpnode.Fieldref.class_index);
        printf("\tname and type index(of this field): %d\n",
            tmpconst->val->cpnode.Fieldref.name_and_type_index);
        break;
      case CONSTANT_Methodref:
        printf("\tclass index(declaring this method): %d\n",
            tmpconst->val->cpnode.Methodref.class_index);
        printf("\tname and type index(of this method): %d\n",
            tmpconst->val->cpnode.Methodref.name_and_type_index);
        break;
      case CONSTANT_InterfaceMethodref:
        printf("\tclass index(declaring this interface): %d\n",
            tmpconst->val->cpnode.InterfaceMethodref.class_index);
        printf("\tname and type index(of this interface): %d\n",
            tmpconst->val->cpnode.InterfaceMethodref.name_and_type_index);
        break;
      case CONSTANT_NameAndType:
        printf("\tname index: %d\n",tmpconst->val->cpnode.NameAndType.name_index);
        printf("\tdescriptor index: %d\n",
           tmpconst->val->cpnode.NameAndType.descriptor_index);
        break;
      default:
        fprintf(stderr,"cp_dump(): Unknown tag!\n");
        break;    /* unnecessary break for ANSI compliance */
    }
  }
}

/*****************************************************************************
 *                                                                           *
 * cp_initialize                                                             *
 *                                                                           *
 * This function stores into the constant pool some default entries that we  *
 * know ahead of time that the class file will need to reference:            *
 *  - an entry for this class                                                *
 *  - an entry for the superclass if necessary                               *
 *  - entries for all class variables                                        *
 *                                                                           *
 *****************************************************************************/

void
cp_initialize(AST *root, Dlist list)
{
  struct cp_info *newnode;
  char *thisname;
  int idx;

  void cp_dump(Dlist);
  int  cp_insert(Dlist, struct cp_info *, char *, char);


  /* first create an entry for 'this'.  the class file variable this_class
   * points to a CONSTANT_Class_info entry in the constant pool, which in
   * turn points to a CONSTANT_Utf8_info entry representing the name of
   * this class.  so, first we create the Utf8 entry, then the Class entry.
   */
  thisname = root->astnode.source.progtype->astnode.source.name->astnode.ident.name;

  printf("inserting entry for %s\n", thisname);

  newnode = (struct cp_info *)malloc(sizeof(struct cp_info));
  newnode->tag = CONSTANT_Utf8;
  newnode->cpnode.Utf8.length = strlen(thisname);
  newnode->cpnode.Utf8.bytes = (u1 *)malloc(newnode->cpnode.Utf8.length);
  strncpy((char *)newnode->cpnode.Utf8.bytes, thisname, newnode->cpnode.Utf8.length);

  idx = cp_insert(list,newnode,NULL,1);

  newnode = (struct cp_info *)malloc(sizeof(struct cp_info));
  newnode->tag = CONSTANT_Class;
  newnode->cpnode.Class.name_index = idx;

  cp_insert(list,newnode,NULL,1);

  printf("List of Constants for program unit: %s\n",
    root->astnode.source.progtype->astnode.source.name->astnode.ident.name);
  printf("\n");
  cp_dump(list);

}
