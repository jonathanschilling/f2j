
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
#include"f2jparse.tab.h"


/*****************************************************************************
 *                                                                           *
 * cp_lookup                                                                 *
 *                                                                           *
 * Searches for the given node in the specified constant pool list.          *
 * If found, return a pointer to the node.  Return NULL otherwise.           *
 *                                                                           *
 *****************************************************************************/

CPNODE *
cp_lookup(Dlist list, enum _constant_tags tag, void *value) {
  Dlist temp;
  extern BOOLEAN bigEndian;
  u4 u4BigEndian(u4);
  struct cp_info * ctemp;

  switch(tag) {
    case CONSTANT_Utf8:
      dl_traverse(temp,list) {
        ctemp = ((CPNODE *)(temp->val))->val;

        if( (ctemp->tag == CONSTANT_Utf8)
          && !strncmp((char*)ctemp->cpnode.Utf8.bytes, (char*)value, ctemp->cpnode.Utf8.length) )
            return temp->val;
      }
      break;
    case CONSTANT_Integer:
      dl_traverse(temp,list) {
        ctemp = ((CPNODE *)(temp->val))->val;

        if( ctemp->tag == CONSTANT_Integer) {
           u4 ival = u4BigEndian( *((u4*)value) );

           if(!memcmp((void *)&ival, (void*)&ctemp->cpnode.Integer.bytes, sizeof(u4)))
             return temp->val;
        }
      }
      break;
    case CONSTANT_Float:
    case CONSTANT_Long:
      /* currently we shouldn't hit either of these cases. */
      fprintf(stderr,"cp_lookup(): WARNING - should not hit Float/Long case\n");
      break;
    case CONSTANT_Double:
      dl_traverse(temp,list) {
        ctemp = ((CPNODE *)(temp->val))->val;

        if( ctemp->tag == CONSTANT_Double) {
           u4 hi_bytes, lo_bytes;

           memcpy(&hi_bytes,value,sizeof(u4));
           memcpy(&lo_bytes,(char*)value+4,sizeof(u4));

           /* convert byte order if necessary, then compare, and return */
           if(!bigEndian) {
             u4 bytetemp = hi_bytes;
             hi_bytes = u4BigEndian(lo_bytes);
             lo_bytes = u4BigEndian(bytetemp);
           }

           if( !memcmp(&hi_bytes, 
                    (void *)&ctemp->cpnode.Double.high_bytes,
                    sizeof(u4))
            && !memcmp(&lo_bytes, 
                    (void *)&ctemp->cpnode.Double.low_bytes,
                    sizeof(u4)))
             return temp->val;
        }
      }
      break;
    case CONSTANT_Class:
    case CONSTANT_String:
    case CONSTANT_Fieldref:
    case CONSTANT_Methodref:
    case CONSTANT_InterfaceMethodref:
    case CONSTANT_NameAndType:
      fprintf(stderr,"cp_lookup: WARNING - tag not yet implemented!\n");
      break;
    default:
      fprintf(stderr,"cp_lookup: WARNING - hit default case!\n");
      return NULL;
  }
  return NULL;
}

/*****************************************************************************
 *                                                                           *
 * insert_constant                                                           *
 *                                                                           *
 * This function inserts a Constant into the constants_table.  We're keeping *
 * track of constants in order to build the constant pool for bytecode       *
 * generation.                                                               *
 *                                                                           *
 *****************************************************************************/

void
insert_constant(AST * nodeToInsert, char * key)
{
  extern Dlist constants_table;
  struct cp_info * newnode = NULL;
  char *tag;
  int idx;
  extern BOOLEAN bigEndian;
  u4 u4BigEndian(u4);

  if(nodeToInsert == NULL)
    return;

  tag = nodeToInsert->astnode.constant.number;

  switch(nodeToInsert->token) {
    case INTEGER:
      {
        /* if integer value is between -1 and 5 inclusive, then
         * we can use the iconst_<i> opcode.  Thus, there's no
         * need to create a constant pool entry.
         */
        int intVal = atoi(tag);

        if( !cp_lookup(constants_table, CONSTANT_Integer, (void *)&intVal)
          && (intVal < -1 || intVal > 5) )
        {
            newnode = (struct cp_info *)malloc(sizeof(struct cp_info));
            newnode->tag = CONSTANT_Integer;
            newnode->cpnode.Integer.bytes = u4BigEndian(intVal);

            cp_insert(constants_table, newnode, tag, 1);
        }
      }
      break;
    case EXPONENTIAL:
    case DOUBLE:
      {
        /* if double value is 0.0 or 1.0, then we can use
         * the dconst_<i> opcode.  Thus, there's no
         * need to create a constant pool entry.
         */
        double doubleVal = atof(tag);
        unsigned int tmp1, tmp2;

        if( !cp_lookup(constants_table, CONSTANT_Double, (void *)&doubleVal)
          && ( doubleVal != 0.0 && doubleVal != 1.0 ) ) 
        {
          newnode = (struct cp_info *)malloc(sizeof(struct cp_info));
          newnode->tag = CONSTANT_Double;
          memcpy(&tmp1,&doubleVal,sizeof(tmp1));
          memcpy(&tmp2,(char*)&doubleVal+4,sizeof(tmp2));
          if(bigEndian) {
            newnode->cpnode.Double.high_bytes = tmp1;
            newnode->cpnode.Double.low_bytes = tmp2;
          }
          else {
            newnode->cpnode.Double.high_bytes = u4BigEndian(tmp2);
            newnode->cpnode.Double.low_bytes = u4BigEndian(tmp1);
          }

          cp_insert(constants_table, newnode, tag, 2);
        }
      }
      break;
    case TrUE:
    case FaLSE:
        /* boolean literals do not need constant pool entries because
         * we can use the iconst_1 opcode for TRUE and iconst_0 for FALSE.
         */
      break;
    case STRING:
        /* unique string literals always go into the constant pool.
         * first, we have to create a CONSTANT_Utf8 entry for the
         * string itself.  then we create a CONSTANT_String entry
         * whose string_index points to the Utf8 string.
         *
         * Note that we only malloc enough for the string itself
         * since the Utf8 string should not be null-terminated.
         */
      if( !cp_lookup(constants_table, CONSTANT_Utf8, (void *)tag))
      {
        newnode = (struct cp_info *)malloc(sizeof(struct cp_info));
        newnode->tag = CONSTANT_Utf8;
        newnode->cpnode.Utf8.length = strlen(tag);
        newnode->cpnode.Utf8.bytes = (u1 *) malloc(newnode->cpnode.Utf8.length);
        strncpy((char *)newnode->cpnode.Utf8.bytes, tag, newnode->cpnode.Utf8.length);

        idx = cp_insert(constants_table, newnode, NULL, 1);

        newnode = (struct cp_info *)malloc(sizeof(struct cp_info));
        newnode->tag = CONSTANT_String;
        newnode->cpnode.String.string_index = idx;

        cp_insert(constants_table, newnode, tag, 1);
      }

      break;
  }
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
  u4 u4BigEndian(u4);
  extern BOOLEAN bigEndian;

  dl_traverse(tmpPtr,list) {
    tmpconst = (CPNODE *) tmpPtr->val;

    printf("Constant pool entry %d:\n", tmpconst->index);
    printf("\ttag: %s\n", constant_tags[tmpconst->val->tag]);
    switch(tmpconst->val->tag) {
      case CONSTANT_Utf8:
        printf("\tstring: %s\n",tmpconst->val->cpnode.Utf8.bytes);
        break;
      case CONSTANT_Integer:
        if(bigEndian)
          printf("\tint: %d\n",tmpconst->val->cpnode.Integer.bytes);
        else
          printf("\tint: %d (conv. to little endian)\n",
              u4BigEndian(tmpconst->val->cpnode.Integer.bytes));
        break;
      case CONSTANT_Float:
        printf("\tfloat: %f\n",(float)tmpconst->val->cpnode.Float.bytes);
        break;
      case CONSTANT_Long:
        printf("\tlong: no long value, shouldn't hit this case!\n");
        break;
      case CONSTANT_Double:
        if(bigEndian) {
          memcpy(&x,&tmpconst->val->cpnode.Double.high_bytes,sizeof(u4));
          memcpy((char*)&x+4,&tmpconst->val->cpnode.Double.low_bytes,sizeof(u4));
          printf("\tdouble: %f (high: %d, low: %d)\n",x,
             tmpconst->val->cpnode.Double.high_bytes, 
             tmpconst->val->cpnode.Double.low_bytes);
        }
        else {
          u4 t1,t2;

          t1 = u4BigEndian(tmpconst->val->cpnode.Double.high_bytes);
          t2 = u4BigEndian(tmpconst->val->cpnode.Double.low_bytes);

          memcpy(&x, &t2, sizeof(u4));
          memcpy((char*)&x+4, &t1, sizeof(u4));

          printf("\tdouble: %f (high: %d, low: %d) (conv to little endian)\n",x,
             u4BigEndian(tmpconst->val->cpnode.Double.high_bytes), 
             u4BigEndian(tmpconst->val->cpnode.Double.low_bytes));
        }
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

u4
u4BigEndian(u4 num)
{
  extern BOOLEAN bigEndian;

  if(bigEndian)
    return num;
  else
    return ((num & 0xFF)<<24) +
           ((num >> 8 & 0xFF)<<16) +
           ((num >> 16 & 0xFF)<<8) +
            (num >> 24);
}
