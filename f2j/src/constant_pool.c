
/*****************************************************************************
 * constant_pool.c                                                           *
 *                                                                           *
 * This file contains routines for manipulating the constant pool list.      *
 *                                                                           *
 *****************************************************************************/

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>
#include"constant_pool.h"
#include"f2jparse.tab.h"

#define NUM_CONSTANT_TAGS 13

int cp_debug = FALSE;    /* set to TRUE to generate deubugging output        */

char * constant_tags [NUM_CONSTANT_TAGS] = {
  "Unknown CONSTANT",
  "CONSTANT_Utf8",
  "Unknown CONSTANT",
  "CONSTANT_Integer",
  "CONSTANT_Float",
  "CONSTANT_Long",
  "CONSTANT_Double",
  "CONSTANT_Class",
  "CONSTANT_String",
  "CONSTANT_Fieldref",
  "CONSTANT_Methodref",
  "CONSTANT_InterfaceMethodref",
  "CONSTANT_NameAndType"
};

u4 u4BigEndian(u4);
u2 u2BigEndian(u2);

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
  struct cp_info * ctemp;

  if(cp_debug)
    printf("&&in cp_lookup\n");

  switch(tag) {
    case CONSTANT_Utf8:
      if(cp_debug) {
        printf("&&hit utf8 constant\n");
        printf("&&value = %s\n",(char *)value);
      }

      dl_traverse(temp,list) {
        ctemp = ((CPNODE *)(temp->val))->val;


        if(ctemp->tag == CONSTANT_Utf8) {
          if(strlen((char*)value) == ctemp->cpnode.Utf8.length)
            if(!strncmp((char*)ctemp->cpnode.Utf8.bytes, (char*)value, ctemp->cpnode.Utf8.length) )
              return temp->val;
        }
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
      {
        int this_len;

        if(cp_debug) {
          printf("&&hit class constant\n");
          printf("&&value = %s\n",(char *)value);
        }

        dl_traverse(temp,list) {
          ctemp = ((CPNODE *)(temp->val))->val;

          if(ctemp->tag == CONSTANT_Class) {
            this_len = cp_entry_by_index(list, 
                ctemp->cpnode.Class.name_index)->val->cpnode.Utf8.length;

            if(this_len == strlen((char*) value))
              if(!strncmp( (char *) (cp_entry_by_index(list, 
                    ctemp->cpnode.Class.name_index)->val->cpnode.Utf8.bytes),
                    (char *)value, strlen((char*)value)))
                return temp->val;
          }
        }
      }
      break;
    case CONSTANT_Fieldref:
    case CONSTANT_InterfaceMethodref:
    case CONSTANT_Methodref:
      {
        METHODREF *mref = (METHODREF *)value;
        CPNODE *nameref;

        if(cp_debug) {
          printf("&&looking up Method/field ref\n");
          printf("&&  mref->classname = '%s'\n",mref->classname);
          printf("&&  mref->methodname = '%s'\n",mref->methodname);
          printf("&&  mref->descriptor = '%s'\n",mref->descriptor);
        }

        /* for the methodref to match, we need to check that the class, method,
         * and descriptor strings all match.
         */

        dl_traverse(temp,list) {
          ctemp = ((CPNODE *)(temp->val))->val;
 
          if(ctemp->tag == tag) {
            char *tmpC, *tmpM, *tmpD;
  
            nameref = cp_entry_by_index(list,ctemp->cpnode.Methodref.class_index);
            nameref = cp_entry_by_index(list,nameref->val->cpnode.Class.name_index);

            tmpC = null_term(nameref->val->cpnode.Utf8.bytes,
               nameref->val->cpnode.Utf8.length);

            if(cp_debug)
               printf("&& name_nad_type_index = %d\n",
                   ctemp->cpnode.Methodref.name_and_type_index);

            nameref = cp_entry_by_index(list,
                         ctemp->cpnode.Methodref.name_and_type_index);

            if(cp_debug)
               printf("&& name index = %d\n",
                 nameref->val->cpnode.NameAndType.name_index);

            nameref = cp_entry_by_index(list,
               nameref->val->cpnode.NameAndType.name_index);

            if(cp_debug) {
               printf("&& ok, nodetype of nameref is %s\n",
                   constant_tags[nameref->val->tag]);
               printf("&& name[0] = %c\n",nameref->val->cpnode.Utf8.bytes[0]);
            }

            tmpM = null_term(nameref->val->cpnode.Utf8.bytes,
                       nameref->val->cpnode.Utf8.length);

            nameref = cp_entry_by_index(list,
                         ctemp->cpnode.Methodref.name_and_type_index);
            nameref = cp_entry_by_index(list,
                         nameref->val->cpnode.NameAndType.descriptor_index);
            tmpD = null_term(nameref->val->cpnode.Utf8.bytes,
                         nameref->val->cpnode.Utf8.length);
        
            if( !strcmp(tmpC, mref->classname) 
              && !strcmp(tmpM, mref->methodname)
              && !strcmp(tmpD, mref->descriptor) )
               return temp->val;
          }
        }
      }
      break;
    case CONSTANT_NameAndType:
      {
        METHODREF *mref = (METHODREF *)value;
        CPNODE *nref, *dref;
        char *tmpM, *tmpD;

        if(cp_debug) {
          printf("&& up NameAndType\n");
          printf("&& mref->classname = '%s'\n",mref->classname);
          printf("&& mref->methodname = '%s'\n",mref->methodname);
          printf("&& mref->descriptor = '%s'\n",mref->descriptor);
        }

        dl_traverse(temp,list) {
          ctemp = ((CPNODE *)(temp->val))->val;

          if(ctemp->tag == CONSTANT_NameAndType) {
            nref = cp_entry_by_index(list,ctemp->cpnode.NameAndType.name_index);
            dref = cp_entry_by_index(list,ctemp->cpnode.NameAndType.descriptor_index);

            tmpM = null_term(nref->val->cpnode.Utf8.bytes,
                      nref->val->cpnode.Utf8.length);
            tmpD = null_term(dref->val->cpnode.Utf8.bytes,
                      dref->val->cpnode.Utf8.length);

            if( !strcmp(tmpM, mref->methodname)
              && !strcmp(tmpD, mref->descriptor))
               return temp->val;
          }
        }
      }
      break;
    case CONSTANT_String:
      {
        CPNODE *sref;
        char *tmpS;

        dl_traverse(temp,list) {
          ctemp = ((CPNODE *)(temp->val))->val;

          if(ctemp->tag == CONSTANT_String) {
            sref = cp_entry_by_index(list,ctemp->cpnode.String.string_index);
            tmpS = null_term(sref->val->cpnode.Utf8.bytes,sref->val->cpnode.Utf8.length);
            if(!strcmp(tmpS,(char *)value))
              return temp->val;
          }
        }
      }
      break;
    default:
      fprintf(stderr,"cp_lookup: WARNING - hit default case!\n");
      return NULL;
  }
  return NULL;
}

/*****************************************************************************
 *                                                                           *
 * cp_find_or_insert                                                         *
 *                                                                           *
 * return a pointer to the node if it exists in the constant pool.  if not,  *
 * create a new entry and return a pointer to it.                            *
 *                                                                           *
 *****************************************************************************/

CPNODE *
cp_find_or_insert(Dlist list, enum _constant_tags tag, void *value) {
  CPNODE *temp;

  if(cp_debug)
    printf("&& cp_find_or_insert\n");

  /* First, check to see if it's already in the list.  */

  if( (temp = cp_lookup(list,tag,value)) != NULL ) {
    if(cp_debug)
       printf("&& found entry, returning\n");

    return temp;
  }

  if(cp_debug)
    printf("&& entry not found, continuing...\n");

  /* It's not in the list, so we insert it and return a pointer to
   * the new node
   */
  switch(tag) {
    struct cp_info *newnode;

    case CONSTANT_Class:
      if(cp_debug)
        printf("&& find/insert Class %s...\n",(char*)value);

      temp = cp_find_or_insert(list,CONSTANT_Utf8,value);
      
      newnode = (struct cp_info *)f2jalloc(sizeof(struct cp_info));
      newnode->tag = CONSTANT_Class;
      newnode->cpnode.Class.name_index = temp->index;

      /* now return the CPNODE pointer created by cp_insert */
      return cp_insert(list,newnode,1);
    case CONSTANT_Fieldref:
    case CONSTANT_InterfaceMethodref:
    case CONSTANT_Methodref:
      {
        METHODREF *mref = (METHODREF *)value;

        if(cp_debug)
          printf("&& ok.. going to find/insert a method reference...\n");

        newnode = (struct cp_info *)f2jalloc(sizeof(struct cp_info));
        newnode->tag = tag;

        if(cp_debug)
          printf("&& first find/insert %s...\n",mref->classname);

        temp = cp_find_or_insert(list,CONSTANT_Class,mref->classname);
        newnode->cpnode.Methodref.class_index = temp->index;

        if(cp_debug)
          printf("&& then find/insert the name_and_type...\n");

        temp = cp_find_or_insert(list,CONSTANT_NameAndType,mref);
        newnode->cpnode.Methodref.name_and_type_index = temp->index;

        return cp_insert(list,newnode,1);
      }
      break;
    case CONSTANT_NameAndType:
      {
        METHODREF *mref = (METHODREF *)value;

        if(cp_debug)
          printf("&& find/insert NameAndType...\n");

        newnode = (struct cp_info *)f2jalloc(sizeof(struct cp_info));
        newnode->tag = CONSTANT_NameAndType;

        temp = cp_find_or_insert(list,CONSTANT_Utf8,mref->methodname);
        newnode->cpnode.NameAndType.name_index = temp->index;

        temp = cp_find_or_insert(list,CONSTANT_Utf8,mref->descriptor);
        newnode->cpnode.NameAndType.descriptor_index = temp->index;
 
        return cp_insert(list,newnode,1);
      }
      break;
    case CONSTANT_Utf8:
      newnode = (struct cp_info *)f2jalloc(sizeof(struct cp_info));
      newnode->tag = CONSTANT_Utf8;
      newnode->cpnode.Utf8.length = strlen(value);
      newnode->cpnode.Utf8.bytes = (u1 *) f2jalloc(newnode->cpnode.Utf8.length);
      strncpy((char*)newnode->cpnode.Utf8.bytes,value,newnode->cpnode.Utf8.length);

      return cp_insert(list, newnode, 1);
      break;
    case CONSTANT_Integer:
      return insert_constant(list, INTEGER,value);
      break;
    case CONSTANT_Float:
    case CONSTANT_Long:
      fprintf(stderr,"cp_find_or_insert():WARNING: should not hit float/long case!\n");
      break;
    case CONSTANT_Double:
      return insert_constant(list, DOUBLE,value);
    case CONSTANT_String:
      return insert_constant(list, STRING,value);
    default:
      fprintf(stderr,"cp_find_or_insert: WARNING - tag not yet implemented!\n");
      break;   /* for ansi compliance */
  }
  
  /* should never hit this return stmt once this function is fully-implemented.
   * still might return NULL if insert_constant returns NULL, though.
   */
  return NULL;
}

/*****************************************************************************
 *                                                                           *
 * cp_entry_by_index                                                         *
 *                                                                           *
 * given an index into the constant pool, return the CPNODE at that index.   *
 *                                                                           *
 *****************************************************************************/

CPNODE *
cp_entry_by_index(Dlist list, unsigned int idx)
{
  Dlist temp;

  if(list == NULL)
    return NULL;

  dl_traverse(temp,list) {
    if( ((CPNODE*)temp->val)->index == idx )
      return temp->val;
  }

  fprintf(stderr,"cp_entry_by_index() WARNING: looking for non-existent cp index!\n");
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

CPNODE *
insert_constant(Dlist list, int tok, void * tag)
{
  struct cp_info * newnode = NULL;
  int idx;
  CPNODE *c;

  switch(tok) {
    case INTEGER:
      {
        /* if integer value is between JVM_SHORT_MIN and JVM_SHORT_MAX,
         * then we do not need to use the ldc opcode.  Thus, there's no
         * need to create a constant pool entry.
         */
        int intVal = *((int*)tag);

        if( !cp_lookup(list, CONSTANT_Integer, (void *)&intVal)
          && (intVal < JVM_SHORT_MIN || intVal > JVM_SHORT_MAX) )
        {
            newnode = (struct cp_info *)f2jalloc(sizeof(struct cp_info));
            newnode->tag = CONSTANT_Integer;
            newnode->cpnode.Integer.bytes = u4BigEndian((u4)intVal);

            return cp_insert(list, newnode, 1);
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
        double doubleVal = *((double *)tag);
        unsigned int tmp1, tmp2;

        if( !cp_lookup(list, CONSTANT_Double, (void *)&doubleVal)
          && ( doubleVal != 0.0 && doubleVal != 1.0 ) ) 
        {
          newnode = (struct cp_info *)f2jalloc(sizeof(struct cp_info));
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

          return cp_insert(list, newnode, 2);
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
printf("inserting a string... '%s'\n",(char *)tag);

      c = cp_lookup(list, CONSTANT_Utf8, tag);

      if(c)
        idx = c->index;
      else
      {
        if(cp_debug)
          printf("&& in insert_constant, inserting '%s'\n",(char *)tag);
printf("&& in insert_constant, inserting '%s'\n",(char *)tag);

        newnode = (struct cp_info *)f2jalloc(sizeof(struct cp_info));
        newnode->tag = CONSTANT_Utf8;
        newnode->cpnode.Utf8.length = strlen(tag);
        newnode->cpnode.Utf8.bytes = (u1 *) f2jalloc(newnode->cpnode.Utf8.length);
        strncpy((char *)newnode->cpnode.Utf8.bytes, tag, newnode->cpnode.Utf8.length);

        idx = cp_insert(list, newnode, 1)->index;
      }

      newnode = (struct cp_info *)f2jalloc(sizeof(struct cp_info));
      newnode->tag = CONSTANT_String;
      newnode->cpnode.String.string_index = idx;

      return cp_insert(list, newnode, 1);

      break;
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

CPNODE *
cp_insert(Dlist list, struct cp_info *node, unsigned int width) {
  CPNODE * n;

  if(cp_debug)
    printf("&& in cp_insert, inserting node w/tag = %s\n", constant_tags[node->tag]);

  n = (CPNODE *)f2jalloc(sizeof(CPNODE));

  n->val = node;
  n->index = dl_empty(list) ? 1 : ((CPNODE *) dl_last(list)->val)->next_idx;
  n->next_idx = n->index + width;

  dl_insert_b(list, n);

  return n;
}

/*****************************************************************************
 *                                                                           *
 * fields_dump                                                               *
 *                                                                           *
 * Dumps a list of the class variables.                                      *
 *                                                                           *
 *****************************************************************************/

void
fields_dump(Dlist flist, Dlist clist)
{
  struct field_info *tmpfield;
  CPNODE * tmpfield2;
  Dlist tmpPtr;
  int count=1;

  dl_traverse(tmpPtr,flist) {
    tmpfield = (struct field_info *) tmpPtr->val;
    printf("Field #%d\n", count++);
    printf("\taccess flags: %d\n",tmpfield->access_flags);
    tmpfield2 = cp_entry_by_index(clist,tmpfield->name_index);
    printf("\tname idx: %d (%s)\n", tmpfield->name_index,
           null_term(tmpfield2->val->cpnode.Utf8.bytes,
                     tmpfield2->val->cpnode.Utf8.length));
    tmpfield2 = cp_entry_by_index(clist,tmpfield->descriptor_index);
    printf("\tdesc idx: %d (%s)\n", tmpfield->descriptor_index,
           null_term(tmpfield2->val->cpnode.Utf8.bytes,
                     tmpfield2->val->cpnode.Utf8.length));
  }
}

/*****************************************************************************
 *                                                                           *
 * cp_quickdump                                                              *
 *                                                                           *
 * Less verbose version of cp_dump.  this function just prints the constant  *
 * pool index and the tag.                                                   *
 *                                                                           *
 *****************************************************************************/

void
cp_quickdump(Dlist list)
{
  CPNODE * tmpconst;
  Dlist tmpPtr;

  dl_traverse(tmpPtr,list) {
    tmpconst = (CPNODE *) tmpPtr->val;
    printf("Constant pool entry %d, ", tmpconst->index);
    printf("tag: %s\n", constant_tags[tmpconst->val->tag]);
  }
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
  CPNODE * tmpconst, * tmpconst2;
  Dlist tmpPtr;
  double x;

  dl_traverse(tmpPtr,list) {
    tmpconst = (CPNODE *) tmpPtr->val;

    printf("Constant pool entry %d:\n", tmpconst->index);
    printf("\ttag: %s\n", constant_tags[tmpconst->val->tag]);
    switch(tmpconst->val->tag) {
      case CONSTANT_Utf8:
        printf("\tstring: %s\n",null_term(tmpconst->val->cpnode.Utf8.bytes,tmpconst->val->cpnode.Utf8.length));
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
        tmpconst2 = cp_entry_by_index(list,tmpconst->val->cpnode.Class.name_index);

        printf("\tclass index: %d -> %s\n",tmpconst->val->cpnode.Class.name_index,
           null_term(tmpconst2->val->cpnode.Utf8.bytes,tmpconst2->val->cpnode.Utf8.length));

        break;
      case CONSTANT_String:
        printf("\tstring index: %d\n",tmpconst->val->cpnode.String.string_index);
        break;
      case CONSTANT_Fieldref:
        printf("\tclass index(declaring this field): %d\n",
            tmpconst->val->cpnode.Methodref.class_index);
        printf("\tname and type index(of this field): %d\n",
            tmpconst->val->cpnode.Methodref.name_and_type_index);
        break;
      case CONSTANT_Methodref:
        printf("\tclass index(declaring this method): %d\n",
            tmpconst->val->cpnode.Methodref.class_index);
        printf("\tname and type index(of this method): %d\n",
            tmpconst->val->cpnode.Methodref.name_and_type_index);
        break;
      case CONSTANT_InterfaceMethodref:
        printf("\tclass index(declaring this interface): %d\n",
            tmpconst->val->cpnode.Methodref.class_index);
        printf("\tname and type index(of this interface): %d\n",
            tmpconst->val->cpnode.Methodref.name_and_type_index);
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

char *
null_term(u1 * str, unsigned int len)
{
  char * temp = (char *)f2jalloc(len + 1);

  strncpy(temp,(char *)str,len);
  temp[len] = '\0';
 
  return temp;
}

/*****************************************************************************
 *                                                                           *
 * u2BigEndian                                                               *
 *                                                                           *
 * This function converts a u2 (unsigned short) to big endian format.  if the*
 * machine is big endian already, we do nothing.  otherwise, we reverse the  *
 * byte order and return the reversed number.                                *
 *                                                                           *
 *****************************************************************************/

u2
u2BigEndian(u2 num)
{
  if(bigEndian)
    return num;
  else
    return (num>>8)+((num&0xFF)<<8); 
}

/*****************************************************************************
 *                                                                           *
 * u4BigEndian                                                               *
 *                                                                           *
 * This function converts a u4 (unsigned int) to big endian format.  if the  *
 * machine is big endian already, we do nothing.  otherwise, we reverse the  *
 * byte order and return the reversed number.                                *
 *                                                                           *
 *****************************************************************************/

u4
u4BigEndian(u4 num)
{
  if(bigEndian)
    return num;
  else
    return ((num & 0xFF)<<24) +
           ((num >> 8 & 0xFF)<<16) +
           ((num >> 16 & 0xFF)<<8) +
            (num >> 24);
}

/*****************************************************************************
 *                                                                           *
 * newMethodref                                                              *
 *                                                                           *
 * This function creates a new method reference and inserts it into the      *
 * constant pool if necessary.  The return value is a pointer to the         *
 * constant pool node containing the method reference.                       *
 *                                                                           *
 *****************************************************************************/

CPNODE *
newMethodref(Dlist list, char *cname, char *mname, char *dname)
{
  METHODREF *methodref;

  methodref = (METHODREF *)f2jalloc(sizeof(METHODREF));
  methodref->classname = cname;
  methodref->methodname = mname;
  methodref->descriptor = dname;

  return cp_find_or_insert(list, CONSTANT_Methodref, methodref);
}

/*****************************************************************************
 *                                                                           *
 * newFieldref                                                               *
 *                                                                           *
 * This function creates a new field reference and inserts it into the       *
 * constant pool if necessary.  The return value is a pointer to the         *
 * constant pool node containing the field reference.                        *
 *                                                                           *
 *****************************************************************************/

CPNODE *
newFieldref(Dlist list, char *cname, char *mname, char *dname)
{
  METHODREF *fieldref;

  fieldref = (METHODREF *)f2jalloc(sizeof(METHODREF));
  fieldref->classname = cname;
  fieldref->methodname = mname;
  fieldref->descriptor = dname;

  return cp_find_or_insert(list, CONSTANT_Fieldref, fieldref);
}
