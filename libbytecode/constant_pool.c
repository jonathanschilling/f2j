/** @file constant_pool.c
 * This file contains routines for manipulating the constant pool list.
 */ 

#include "constant_pool.h"

/**
 * Searches for the given node in the specified constant pool list.     
 *
 * @param class -- The class containing the constant pool to be searched.
 * @param tag -- The type of constant contained in the 'value' argument.
 * @param value -- The constant value to be searched for.
 *
 * @returns If the node is found, return its constant pool index.  
 *     Return -1 otherwise.
 **/

int
cp_lookup(JVM_CLASS *class, JVM_CONSTANT tag, const void *value) {
  int retval;

  if(!class || !value) {
    BAD_ARG();
    return -1;
  }

  switch(tag) {
    case CONSTANT_Utf8:
      retval = cp_lookup_utf8(class, value);
      break;
    case CONSTANT_Integer:
      retval = cp_lookup_int(class, value);
      break;
    case CONSTANT_Float:
      retval = cp_lookup_float(class, value);
      break;
    case CONSTANT_Long:
      retval = cp_lookup_long(class, value);
      break;
    case CONSTANT_Double:
      retval = cp_lookup_double(class, value);
      break;
    case CONSTANT_Class:
      retval = cp_lookup_class(class, value);
      break;
    case CONSTANT_Fieldref:
    case CONSTANT_InterfaceMethodref:
    case CONSTANT_Methodref:
      retval = cp_lookup_ref(class, tag, value);
      break;
    case CONSTANT_NameAndType:
      retval = cp_lookup_nameandtype(class, value);
      break;
    case CONSTANT_String:
      retval = cp_lookup_string(class, value);
      break;
    default:
      debug_err("cp_lookup: WARNING - hit default case!\n");
      retval = -1;
  }

  return retval;
}

/**
 * Find the constant pool index for the given constant if it exists in 
 * the constant pool.  If not, create a new entry and return its index.
 * This function will not insert constant values for which there exist
 * shorthand instructions for pushing those values onto the stack.  For
 * example floating point values 0.0, 1.0, and 2.0 can be pushed using
 * shorthand instructions fconst_0, fconst_1, and fconst_2 respectively.
 * Similar instructions exist for integer, long, and double.  Therefore
 * these values usually do not need to be inserted into the constant pool.
 * If you need one of these values inserted, use the cp_manual_insert() 
 * function.
 *
 * @param class -- The class containing the constant pool to be searched.
 * @param tag -- The type of constant contained in the 'value' argument.
 * @param value -- The constant value to be searched for.
 *
 * @returns The constant pool index for the given constant value.
 *   If the value was not inserted because it was a special value
 *   as mentioned above or if an error occurred then -1 is returned.
 **/

int
cp_find_or_insert(JVM_CLASS *class, JVM_CONSTANT tag, const void *value) {
  int temp;

  if(!class || !value) {
    BAD_ARG();
    return -1;
  }

  temp = cp_find_function_body(class, tag, value, FALSE);
  
  if(temp < 0)
    CP_CHECK_NONZERO("cp_find_or_insert", temp);

  return temp;
}

/**
 * Identical to cp_find_or_insert(), except that integer and double precision
 * constants that would normally be excluded are inserted.
 *
 * @param class -- The class containing the constant pool to be searched.
 * @param tag -- The type of constant contained in the 'value' argument.
 * @param value -- The constant value to be inserted.
 *
 * @returns The constant pool index for the given constant value.
 *   On error, returns -1.
 **/

int
cp_manual_insert(JVM_CLASS *class, JVM_CONSTANT tag, const void *value) {
  int temp;

  if(!class || !value) {
    BAD_ARG();
    return -1;
  }

  temp = cp_find_function_body(class, tag, value, TRUE);
 
  if(temp < 0)
    CP_CHECK_NONZERO("cp_manual_insert", temp);

  return temp;
}


/**
 * Given an index into the constant pool, return a pointer to 
 * the CP_NODE at that index.
 *
 * @param class -- The class containing the constant pool to be searched.
 * @param idx -- The constant pool index to be returned.
 *
 * @returns The CP_NODE at the given index.  Returns NULL if the 
 *   specified index is not found in the constant pool.
 **/

CP_NODE *
cp_entry_by_index(JVM_CLASS *class, unsigned int idx)
{
  Dlist temp;

  if(!class) {
    BAD_ARG();
    return NULL;
  }

  dl_traverse(temp,class->constant_pool) {
    if( ((CP_NODE*)temp->val)->index == idx )
      return temp->val;
  }

  debug_err("cp_entry_by_index() WARNING: looking for non-existent cp index!\n");
  return NULL;
}

/**
 * Dumps a list of the class variables to stdout.
 *
 * @param class -- The class containing the constant pool to be printed.
 **/

void
cp_fields_dump(JVM_CLASS *class)
{
  JVM_FIELD *tmpfield;
  CP_NODE * tmpfield2;
  Dlist tmpPtr;
  int count=1;

  if(!class) {
    BAD_ARG();
    return;
  }

  dl_traverse(tmpPtr,class->fields) {
    tmpfield = (JVM_FIELD *) tmpPtr->val;

    printf("Field #%d\n", count++);
    printf("\taccess flags: %d\n",tmpfield->access_flags);

    tmpfield2 = cp_entry_by_index(class, tmpfield->name_index);
    printf("\tname idx: %d (%s)\n", tmpfield->name_index,
           cp_null_term_utf8(tmpfield2->val));

    tmpfield2 = cp_entry_by_index(class, tmpfield->descriptor_index);
    printf("\tdesc idx: %d (%s)\n", tmpfield->descriptor_index,
           cp_null_term_utf8(tmpfield2->val));
  }
}

/**
 * Less verbose version of cp_dump().  This function just prints the constant
 * pool index and the tag.
 *
 * @param class -- The class containing the constant pool to be printed.
 **/

void
cp_quickdump(JVM_CLASS *class)
{
  CP_NODE * tmpconst;
  Dlist tmpPtr;

  if(!class) {
    BAD_ARG(); 
    return;
  }

  dl_traverse(tmpPtr,class->constant_pool) {
    tmpconst = (CP_NODE *) tmpPtr->val;
    printf("Constant pool entry %d, ", tmpconst->index);
    printf("tag: %s\n", jvm_constant_tags[tmpconst->val->tag]);
  }
}

/**
 * Prints the contents of the constant pool to stdout.
 *
 * @param class -- The class containing the constant pool to be printed.
 **/

void
cp_dump(JVM_CLASS *class)
{
  CP_NODE * tmpconst, * tmpconst2;
  Dlist tmpPtr;
  double x;
  float f;
  u8 l;
  char *tmp_str;

  if(!class) {
    BAD_ARG(); 
    return;
  }

  dl_traverse(tmpPtr,class->constant_pool) {
    tmpconst = (CP_NODE *) tmpPtr->val;

    printf("Constant pool entry %d:\n", tmpconst->index);
    printf("\ttag: %s\n", jvm_constant_tags[tmpconst->val->tag]);
    switch(tmpconst->val->tag) {
      case CONSTANT_Utf8:
        tmp_str = cp_null_term_utf8(tmpconst->val);
        printf("\tstring: %s\n",tmp_str);

        free(tmp_str);
        break;
      case CONSTANT_Integer:
        if(isBigEndian())
          printf("\tint: %d\n",tmpconst->val->cpnode.Integer.bytes);
        else
          printf("\tint: %d (conv. to little endian)\n",
              cp_big_endian_u4(tmpconst->val->cpnode.Integer.bytes));
        break;
      case CONSTANT_Float:
        if(isBigEndian())
          printf("\tfloat: %f\n",(float)tmpconst->val->cpnode.Float.bytes);
        else {
          u4 tmp;

          tmp = cp_big_endian_u4(tmpconst->val->cpnode.Float.bytes);
          memcpy(&f, &tmp, sizeof(u4));

          printf("\tfloat: %f (conv. to little endian)\n", f);
        }
        break;
      case CONSTANT_Long:
        if(isBigEndian()) {
          memcpy(&l,&tmpconst->val->cpnode.Long.high_bytes,sizeof(u4));
          memcpy((char*)&l+4,&tmpconst->val->cpnode.Long.low_bytes,sizeof(u4));
          printf("\tlong: %ld (high: %d, low: %d)\n", (long) l,
             (int)tmpconst->val->cpnode.Long.high_bytes,
             (int)tmpconst->val->cpnode.Long.low_bytes);
        }
        else {
          u4 t1,t2;

          t1 = cp_big_endian_u4(tmpconst->val->cpnode.Long.high_bytes);
          t2 = cp_big_endian_u4(tmpconst->val->cpnode.Long.low_bytes);

          memcpy(&l, &t2, sizeof(u4));
          memcpy((char*)&l+4, &t1, sizeof(u4));

          printf("\tlong: %ld (high: %d, low: %d) (conv to little endian)\n",(long)l,
             cp_big_endian_u4(tmpconst->val->cpnode.Long.high_bytes),
             cp_big_endian_u4(tmpconst->val->cpnode.Long.low_bytes));
        }
        break;
      case CONSTANT_Double:
        if(isBigEndian()) {
          memcpy(&x,&tmpconst->val->cpnode.Double.high_bytes,sizeof(u4));
          memcpy((char*)&x+4,&tmpconst->val->cpnode.Double.low_bytes,sizeof(u4));
          printf("\tdouble: %f (high: %d, low: %d)\n",x,
             tmpconst->val->cpnode.Double.high_bytes, 
             tmpconst->val->cpnode.Double.low_bytes);
        }
        else {
          u4 t1,t2;

          t1 = cp_big_endian_u4(tmpconst->val->cpnode.Double.high_bytes);
          t2 = cp_big_endian_u4(tmpconst->val->cpnode.Double.low_bytes);

          memcpy(&x, &t2, sizeof(u4));
          memcpy((char*)&x+4, &t1, sizeof(u4));

          printf("\tdouble: %f (high: %d, low: %d) (conv to little endian)\n",x,
             cp_big_endian_u4(tmpconst->val->cpnode.Double.high_bytes), 
             cp_big_endian_u4(tmpconst->val->cpnode.Double.low_bytes));
        }
        break;
      case CONSTANT_Class:
        tmpconst2 = cp_entry_by_index(class,tmpconst->val->cpnode.Class.name_index);
        tmp_str = cp_null_term_utf8(tmpconst2->val);

        printf("\tclass index: %d -> %s\n",tmpconst->val->cpnode.Class.name_index,
          tmp_str);

        free(tmp_str);

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
        debug_err("cp_dump(): Unknown tag!\n");
        break;    /* unnecessary break for ANSI compliance */
    }
  }
}

/**
 * Creates a null-terminated version of the given utf8 constant pool entry.
 *
 * @param val -- The utf8 entry.
 *
 * @returns A null-terminated string.  On error returns NULL.
 **/

char *
cp_null_term_utf8(CP_INFO *val)
{
  char * temp;

  if(!val) {
    BAD_ARG(); 
    return NULL;
  }

  temp = (char *)malloc(val->cpnode.Utf8.length + 1);

  if(!temp) return NULL;

  strncpy(temp,(char *)val->cpnode.Utf8.bytes,val->cpnode.Utf8.length);
  temp[val->cpnode.Utf8.length] = '\0';
 
  return temp;
}

/**
 * This function converts a u2 (unsigned short) to big endian format.  if the
 * machine is big endian already, we do nothing.  otherwise, we reverse the
 * byte order and return the reversed number.
 *
 * @param num -- The unsigned short to be converted.
 *
 * @returns Big endian version of the specified number.
 **/

u2
cp_big_endian_u2(u2 num)
{
  if(isBigEndian())
    return num;
  else
    return (num>>8)+((num&0xFF)<<8); 
}

/**
 * This function converts a u4 (unsigned int) to big endian format.  if the
 * machine is big endian already, we do nothing.  otherwise, we reverse the
 * byte order and return the reversed number.
 *
 * @param num -- The unsigned int to be converted.
 *
 * @returns Big endian version of the specified number.
 **/

u4
cp_big_endian_u4(u4 num)
{
  if(isBigEndian())
    return num;
  else
    return ((num & 0xFF)<<24) +
           ((num >> 8 & 0xFF)<<16) +
           ((num >> 16 & 0xFF)<<8) +
            (num >> 24);
}

/*****************************************************************************
 *****************************************************************************
 **                                                                         **
 ** Functions after this point are not exposed as part of the API.          **
 **                                                                         **
 *****************************************************************************
 *****************************************************************************/

/** 
 * Inserts the given CP_INFO node into the constant pool list.
 *
 * @param class -- The class containing the constant pool into which the
 *    node will be inserted.
 * @param node -- The node to be inserted.
 *
 * @returns The constant pool index of the node after insertion.  
 *    Returns -1 on error.
 **/

static int
cp_insert(JVM_CLASS *class, CP_INFO *node) {
  CP_NODE * n;
  Dlist cp;

  if(!class || !node) {
    BAD_ARG();
    return -1;
  }

  cp = class->constant_pool;

  debug_msg("&& in cp_insert, inserting node w/tag = %s\n",
      jvm_constant_tags[node->tag]);
    
  n = (CP_NODE *)malloc(sizeof(CP_NODE));

  if(!n) return -1;

  n->val = node;
  n->index = dl_empty(cp) ? 1 : ((CP_NODE *) dl_last(cp)->val)->next_idx;
  n->next_idx = n->index + cp_entry_width[node->tag];
  
  dl_insert_b(cp, n);

  return n->index;
}

/**
 * This function inserts a Constant into the constants_table.  We're keeping
 * track of constants in order to build the constant pool for bytecode
 * generation.
 *
 * @param class -- The class containing the constant pool to be searched.
 * @param tok -- The type of constant contained in the 'val' argument.
 * @param val -- The constant value to be inserted.
 * @param force_insert -- If FALSE, certain constants will be excluded
 *     depending on the data type/value (see cp_find_or_insert()).  If TRUE,
 *     the constant will be inserted regardless of its value.
 *
 * @returns The constant pool index of the value after insertion.  
 *    Returns -1 on error.
 **/

static int
insert_constant(JVM_CLASS *class, int tok, const void *val, BOOL force_insert)
{
  if(!class || !val) {
    BAD_ARG();
    return -1;
  }

  switch(tok) {
    case CP_INTEGER_CONST:
      return insert_int_constant(class, val, force_insert);
    case CP_FLOAT_CONST:
      return insert_float_constant(class, val, force_insert);
    case CP_LONG_CONST:
      return insert_long_constant(class, val, force_insert);
    case CP_EXPONENTIAL_CONST:
    case CP_DOUBLE_CONST:
      return insert_double_constant(class, val, force_insert);
    case CP_TRUE_CONST:
    case CP_FALSE_CONST:
        /* boolean literals do not need constant pool entries because
         * we can use the iconst_1 opcode for TRUE and iconst_0 for FALSE.
         */
      return -1;
    case CP_STRING_CONST:
      return insert_string_constant(class, val, force_insert);
  }
 
  return -1;
}

/**
 * This function returns the endianness of the machine we're running on.
 * Such information is used during bytecode generation since the numerical
 * constants are always stored in big endian format.
 *
 * @returns TRUE if this machine is big endian, FALSE otherwise.
 **/

static BOOL
isBigEndian()
{ 

#ifdef WORDS_BIGENDIAN
  return TRUE;
#else
  return FALSE;
#endif

}

/**
 * Searches the constant pool for a UTF8 string.
 *
 * @param class -- The class containing the constant pool to be searched.
 * @param value -- The UTF8 constant value to be searched for.
 *
 * @returns If the value is found, return its constant pool index.  
 *     Return -1 otherwise.
 **/

static int
cp_lookup_utf8(JVM_CLASS *class, const void *value) {
  Dlist temp;
  CP_INFO * ctemp;

  if(!class || !value) {
    BAD_ARG();
    return -1;
  }

  debug_msg("&&hit utf8 constant\n");
  debug_msg("&&value = %s\n",(char *)value);

  dl_traverse(temp,class->constant_pool) {
    ctemp = ((CP_NODE *)(temp->val))->val;

    if(ctemp->tag == CONSTANT_Utf8) {
      if(strlen((char*)value) == (unsigned int)ctemp->cpnode.Utf8.length)
        if(!strncmp((char*)ctemp->cpnode.Utf8.bytes, (char*)value, 
             ctemp->cpnode.Utf8.length) )
          return ((CP_NODE *)(temp->val))->index;
    }
  }

  return -1;
}

/**
 * Searches the constant pool for an integer constant.
 *
 * @param class -- The class containing the constant pool to be searched.
 * @param value -- The constant value to be searched for.
 *
 * @returns If the value is found, return its constant pool index.  
 *     Return -1 otherwise.
 **/

static int
cp_lookup_int(JVM_CLASS *class, const void *value) {
  Dlist temp;
  CP_INFO * ctemp;

  if(!class || !value) {
    BAD_ARG();
    return -1;
  }

  dl_traverse(temp,class->constant_pool) {
    ctemp = ((CP_NODE *)(temp->val))->val;

    if( ctemp->tag == CONSTANT_Integer) {
       u4 ival = cp_big_endian_u4( *((u4*)value) );

       if(!memcmp((void *)&ival, (void*)&ctemp->cpnode.Integer.bytes, sizeof(u4)))
         return ((CP_NODE *)(temp->val))->index;
    }
  }

  return -1;
}

/**
 * Searches the constant pool for a float constant.
 *
 * @param class -- The class containing the constant pool to be searched.
 * @param value -- The constant value to be searched for.
 *
 * @returns If the value is found, return its constant pool index.  
 *     Return -1 otherwise.
 **/

static int
cp_lookup_float(JVM_CLASS *class, const void *value) {
  Dlist temp;
  CP_INFO * ctemp;

  if(!class || !value) {
    BAD_ARG();
    return -1;
  }

  dl_traverse(temp,class->constant_pool) {
    ctemp = ((CP_NODE *)(temp->val))->val;

    if( ctemp->tag == CONSTANT_Float) {
       u4 fval = cp_big_endian_u4( *((u4*)value) );

       if(!memcmp((void *)&fval, (void*)&ctemp->cpnode.Float.bytes, sizeof(u4)))
         return ((CP_NODE *)(temp->val))->index;
    }
  }

  return -1;
}

/**
 * Searches the constant pool for a long constant.
 *
 * @param class -- The class containing the constant pool to be searched.
 * @param value -- The constant value to be searched for.
 *
 * @returns If the value is found, return its constant pool index.  
 *     Return -1 otherwise.
 **/

static int
cp_lookup_long(JVM_CLASS *class, const void *value) {
  Dlist temp;
  CP_INFO * ctemp;

  if(!class || !value) {
    BAD_ARG();
    return -1;
  }

  dl_traverse(temp,class->constant_pool) {
    ctemp = ((CP_NODE *)(temp->val))->val;

    if( ctemp->tag == CONSTANT_Long) {
       u4 hi_bytes, lo_bytes;

       memcpy(&hi_bytes,value,sizeof(u4));
       memcpy(&lo_bytes,(char*)value+4,sizeof(u4));

       /* convert byte order if necessary, then compare, and return */
       if(!isBigEndian()) {
         u4 bytetemp = hi_bytes;
         hi_bytes = cp_big_endian_u4(lo_bytes);
         lo_bytes = cp_big_endian_u4(bytetemp);
       }

       if( !memcmp(&hi_bytes,
                (void *)&ctemp->cpnode.Long.high_bytes,
                sizeof(u4))
        && !memcmp(&lo_bytes,
                (void *)&ctemp->cpnode.Long.low_bytes,
                sizeof(u4)))
         return ((CP_NODE *)(temp->val))->index;
    }
  }

  return -1;
}

/**
 * Searches the constant pool for a double precision constant.
 *
 * @param class -- The class containing the constant pool to be searched.
 * @param value -- The constant value to be searched for.
 *
 * @returns If the value is found, return its constant pool index.  
 *     Return -1 otherwise.
 **/

static int
cp_lookup_double(JVM_CLASS *class, const void *value) {
  Dlist temp;
  CP_INFO * ctemp;

  if(!class || !value) {
    BAD_ARG();
    return -1;
  }

  dl_traverse(temp,class->constant_pool) {
    ctemp = ((CP_NODE *)(temp->val))->val;

    if( ctemp->tag == CONSTANT_Double) {
       u4 hi_bytes, lo_bytes;

       memcpy(&hi_bytes,value,sizeof(u4));
       memcpy(&lo_bytes,(char*)value+4,sizeof(u4));

       /* convert byte order if necessary, then compare, and return */
       if(!isBigEndian()) {
         u4 bytetemp = hi_bytes;
         hi_bytes = cp_big_endian_u4(lo_bytes);
         lo_bytes = cp_big_endian_u4(bytetemp);
       }

       if( !memcmp(&hi_bytes, 
                (void *)&ctemp->cpnode.Double.high_bytes,
                sizeof(u4))
        && !memcmp(&lo_bytes, 
                (void *)&ctemp->cpnode.Double.low_bytes,
                sizeof(u4)))
         return ((CP_NODE *)(temp->val))->index;
    }
  }

  return -1;
}

/**
 * Searches the constant pool for a class constant.
 *
 * @param class -- The class containing the constant pool to be searched.
 * @param value -- The constant value to be searched for.
 *
 * @returns If the value is found, return its constant pool index.  
 *     Return -1 otherwise.
 **/

static int
cp_lookup_class(JVM_CLASS *class, const void *value) {
  Dlist temp;
  CP_INFO * ctemp;
  int this_len;

  if(!class || !value) {
    BAD_ARG();
    return -1;
  }

  debug_msg("&&hit class constant\n");
  debug_msg("&&value = %s\n",(char *)value);

  dl_traverse(temp,class->constant_pool) {
    ctemp = ((CP_NODE *)(temp->val))->val;

    if(ctemp->tag == CONSTANT_Class) {
      this_len = cp_entry_by_index(class, 
          ctemp->cpnode.Class.name_index)->val->cpnode.Utf8.length;

      if(!this_len) continue;

      if((unsigned int)this_len == strlen((char*) value)) {
        CP_NODE *e = cp_entry_by_index(class, ctemp->cpnode.Class.name_index);

        if(!e) continue;

        if(!strncmp( (char *) (e->val->cpnode.Utf8.bytes),
              (char *)value, strlen((char*)value)))
          return ((CP_NODE *)(temp->val))->index;
      }
    }
  }

  return -1;
}

/**
 * Searches the constant pool for a method/field reference.
 *
 * @param class -- The class containing the constant pool to be searched.
 * @param tag -- The type of constant contained in the 'value' argument.
 * @param value -- The constant value to be searched for.
 *
 * @returns If the value is found, return its constant pool index.  
 *     Return -1 otherwise.
 **/

static int
cp_lookup_ref(JVM_CLASS *class, JVM_CONSTANT tag, const void *value) {
  Dlist temp;
  CP_INFO * ctemp;
  JVM_METHODREF *mref = (JVM_METHODREF *)value;
  CP_NODE *nameref;

  if(!class || !value) {
    BAD_ARG();
    return -1;
  }

#define err_lookup() \
        if(tmpC) free(tmpC); \
        if(tmpM) free(tmpM); \
        if(tmpM) free(tmpD);
  
  debug_msg("&&looking up Method/field ref\n");
  debug_msg("&&  mref->classname = '%s'\n",mref->classname);
  debug_msg("&&  mref->methodname = '%s'\n",mref->methodname);
  debug_msg("&&  mref->descriptor = '%s'\n",mref->descriptor);

  /* for the methodref to match, we need to check that the class, method,
   * and descriptor strings all match.
   */

  dl_traverse(temp,class->constant_pool) {
    ctemp = ((CP_NODE *)(temp->val))->val;
 
    if(ctemp->tag == tag) {
      char *tmpC, *tmpM, *tmpD;

      tmpC = tmpM = tmpD = NULL;

      nameref = cp_entry_by_index(class,ctemp->cpnode.Methodref.class_index);
      if(!nameref) continue;

      nameref = cp_entry_by_index(class,nameref->val->cpnode.Class.name_index);
      if(!nameref) continue;

      tmpC = cp_null_term_utf8(nameref->val);
      if(!tmpC) continue;

      debug_msg("&& name_nad_type_index = %d\n",
          ctemp->cpnode.Methodref.name_and_type_index);

      nameref = cp_entry_by_index(class,
                   ctemp->cpnode.Methodref.name_and_type_index);
      if(!nameref) {
        err_lookup();
        continue;
      }

      debug_msg("&& name index = %d\n",
         nameref->val->cpnode.NameAndType.name_index);

      nameref = cp_entry_by_index(class,
         nameref->val->cpnode.NameAndType.name_index);
      if(!nameref) {
        err_lookup();
        continue;
      }

      debug_msg("&& ok, nodetype of nameref is %s\n",
          jvm_constant_tags[nameref->val->tag]);
      debug_msg("&& name[0] = %c\n",nameref->val->cpnode.Utf8.bytes[0]);

      tmpM = cp_null_term_utf8(nameref->val);
      if(!tmpM) {
        err_lookup();
        continue;
      }

      nameref = cp_entry_by_index(class,
                   ctemp->cpnode.Methodref.name_and_type_index);
      if(!nameref) {
        err_lookup();
        continue;
      }

      nameref = cp_entry_by_index(class,
                   nameref->val->cpnode.NameAndType.descriptor_index);
      if(!nameref) {
        err_lookup();
        continue;
      }

      tmpD = cp_null_term_utf8(nameref->val);
      if(!tmpD) {
        err_lookup();
        continue;
      }
  
      if( !strcmp(tmpC, mref->classname) 
        && !strcmp(tmpM, mref->methodname)
        && !strcmp(tmpD, mref->descriptor) )
      {
        err_lookup();

        return ((CP_NODE *)(temp->val))->index;
      }
      else {
        err_lookup();
      }
    }
  }

#undef err_lookup

  return -1;
}

/**
 * Searches the constant pool for a name and type reference.
 *
 * @param class -- The class containing the constant pool to be searched.
 * @param value -- The constant value to be searched for.
 *
 * @returns If the value is found, return its constant pool index.  
 *     Return -1 otherwise.
 **/

static int
cp_lookup_nameandtype(JVM_CLASS *class, const void *value) {
  Dlist temp;
  CP_INFO * ctemp;
  JVM_METHODREF *mref = (JVM_METHODREF *)value;
  CP_NODE *nref, *dref;
  char *tmpM, *tmpD;

  if(!class || !value) {
    BAD_ARG();
    return -1;
  }

  debug_msg("&& up NameAndType\n");
  debug_msg("&& mref->classname = '%s'\n",mref->classname);
  debug_msg("&& mref->methodname = '%s'\n",mref->methodname);
  debug_msg("&& mref->descriptor = '%s'\n",mref->descriptor);

  dl_traverse(temp,class->constant_pool) {
    ctemp = ((CP_NODE *)(temp->val))->val;

    if(ctemp->tag == CONSTANT_NameAndType) {
      nref = cp_entry_by_index(class,ctemp->cpnode.NameAndType.name_index);
      if(!nref) continue;

      dref = cp_entry_by_index(class,ctemp->cpnode.NameAndType.descriptor_index);
      if(!dref) continue;

      tmpM = cp_null_term_utf8(nref->val);
      if(!tmpM) continue;

      tmpD = cp_null_term_utf8(dref->val);
      if(!tmpD) {
        free(tmpM);
        continue;
      }

      if( !strcmp(tmpM, mref->methodname)
        && !strcmp(tmpD, mref->descriptor))
      {
        free(tmpM);
        free(tmpD);
        return ((CP_NODE *)(temp->val))->index;
      }
      else {
        free(tmpM);
        free(tmpD);
      }
    }
  }

  return -1;
}

/**
 * Searches the constant pool for a String constant.
 *
 * @param class -- The class containing the constant pool to be searched.
 * @param value -- The constant value to be searched for.
 *
 * @returns If the value is found, return its constant pool index.  
 *     Return -1 otherwise.
 **/

static int
cp_lookup_string(JVM_CLASS *class, const void *value) {
  Dlist temp;
  CP_INFO * ctemp;
  CP_NODE *sref;
  char *tmpS;

  if(!class || !value) {
    BAD_ARG();
    return -1;
  }

  dl_traverse(temp,class->constant_pool) {
    ctemp = ((CP_NODE *)(temp->val))->val;

    if(ctemp->tag == CONSTANT_String) {
      sref = cp_entry_by_index(class,ctemp->cpnode.String.string_index);
      if(!sref) continue;

      tmpS = cp_null_term_utf8(sref->val);
      if(!tmpS) continue;

      if(!strcmp(tmpS,(char *)value)) {
        free(tmpS);
        return ((CP_NODE *)(temp->val))->index;
      }
      else
        free(tmpS);
    }
  }

  return -1;
}

/**
 * Inserts a class constant into the constant pool.
 *
 * @param class -- The class containing the constant pool to be searched.
 * @param value -- The constant value to be inserted.
 *
 * @returns The constant pool index for the given constant value.
 *   On error, returns -1.
 **/

static int
insert_class(JVM_CLASS *class, const void *value) {
  CP_INFO *newnode;
  int temp;
  char *t;
  int i;
 
  if(!class || !value) {
    BAD_ARG();
    return -1;
  }

  debug_msg("&& find/insert Class %s...\n",(char*)value);

  t = strdup((char *)value);
  if(!t) return -1;

  for(i=0;i<strlen(t);i++)
    if(t[i]=='.') t[i]='/';

  temp = cp_find_or_insert(class, CONSTANT_Utf8, t);

  free(t);

  newnode = (CP_INFO *)malloc(sizeof(CP_INFO));

  if(!newnode || (temp < 0)) return -1;

  newnode->tag = CONSTANT_Class;
  newnode->cpnode.Class.name_index = temp;
  
  /* now return the CP_NODE pointer created by cp_insert */
  return cp_insert(class,newnode);
}

/**
 * Inserts a class constant into the constant pool.
 *
 * @param class -- The class containing the constant pool to be searched.
 * @param tag -- The type of constant contained in the 'value' argument.
 * @param value -- The constant value to be inserted.
 *
 * @returns The constant pool index for the given constant value.
 *   On error, returns -1.
 **/

static int
insert_ref(JVM_CLASS *class, JVM_CONSTANT tag, const void *value) {
  JVM_METHODREF *mref = (JVM_METHODREF *)value;
  CP_INFO *newnode;
  int temp;
 
  if(!class || !value) {
    BAD_ARG();
    return -1;
  }

  debug_msg("&& ok.. going to find/insert a method reference...\n");

  newnode = (CP_INFO *)malloc(sizeof(CP_INFO));
  if(!newnode) return -1;

  newnode->tag = (u1) tag;

  debug_msg("&& first find/insert %s...\n",mref->classname);

  temp = cp_find_or_insert(class,CONSTANT_Class,mref->classname);
  if(temp < 0) {
    free(newnode);
    return -1;
  }

  newnode->cpnode.Methodref.class_index = temp;

  debug_msg("&& then find/insert the name_and_type...\n");

  temp = cp_find_or_insert(class,CONSTANT_NameAndType,mref);
  if(temp < 0) {
    free(newnode);
    return -1;
  }

  newnode->cpnode.Methodref.name_and_type_index = temp;

  return cp_insert(class,newnode);
}

/**
 * Inserts a class constant into the constant pool.
 *
 * @param class -- The class containing the constant pool to be searched.
 * @param value -- The constant value to be inserted.
 *
 * @returns The constant pool index for the given constant value.
 *   On error, returns -1.
 **/

static int
insert_nameandtype(JVM_CLASS *class, const void *value) {
  JVM_METHODREF *mref = (JVM_METHODREF *)value;
  CP_INFO *newnode;
  int temp;

  if(!class || !value) {
    BAD_ARG();
    return -1;
  }

  debug_msg("&& find/insert NameAndType...\n");

  newnode = (CP_INFO *)malloc(sizeof(CP_INFO));
  if(!newnode) return -1;

  newnode->tag = CONSTANT_NameAndType;
   
  temp = cp_find_or_insert(class,CONSTANT_Utf8,mref->methodname);
  if(temp < 0) {
    free(newnode);
    return -1;
  }

  newnode->cpnode.NameAndType.name_index = temp;
    
  temp = cp_find_or_insert(class,CONSTANT_Utf8,mref->descriptor);
  if(temp < 0) {
    free(newnode);
    return -1;
  }

  newnode->cpnode.NameAndType.descriptor_index = temp;

  return cp_insert(class,newnode);
}

/**
 * Inserts a class constant into the constant pool.
 *
 * @param class -- The class containing the constant pool to be searched.
 * @param value -- The constant value to be inserted.
 *
 * @returns The constant pool index for the given constant value.
 *   On error, returns -1.
 **/

static int
insert_utf8(JVM_CLASS *class, const void *value) {
  CP_INFO *newnode;

  if(!class || !value) {
    BAD_ARG();
    return -1;
  }

  newnode = (CP_INFO *)malloc(sizeof(CP_INFO));
  if(!newnode) return -1;

  newnode->tag = CONSTANT_Utf8; 
  newnode->cpnode.Utf8.length = strlen(value);
  newnode->cpnode.Utf8.bytes = (u1 *) malloc(newnode->cpnode.Utf8.length);
  if(!newnode->cpnode.Utf8.bytes) {
    free(newnode);
    return -1;
  }

  strncpy((char*)newnode->cpnode.Utf8.bytes,value,newnode->cpnode.Utf8.length); 

  return cp_insert(class, newnode);
}

/**
 * Inserts a class constant into the constant pool.
 *
 * @param class -- The class containing the constant pool to be searched.
 * @param val -- The constant value to be inserted.
 * @param force_insert -- If FALSE, certain constants will be excluded
 *     depending on the data type/value (see cp_find_or_insert()).  If TRUE,
 *     the constant will be inserted regardless of its value.
 *
 * @returns The constant pool index for the given constant value.
 *   On error, returns -1.
 **/

static int
insert_int_constant(JVM_CLASS *class, const void *val, BOOL force_insert) {
  CP_INFO * newnode;
  int intVal;

  if(!class || !val)  {
    BAD_ARG();
    return -1;
  }

  intVal = *((int*)val);

  /* if integer value is between JVM_SHORT_MIN and JVM_SHORT_MAX,
   * then we do not need to use the ldc opcode.  Thus, there's no
   * need to create a constant pool entry.
   */

  if(( (cp_lookup(class, CONSTANT_Integer, (void *)&intVal) < 0)
    && (intVal < JVM_SHORT_MIN || intVal > JVM_SHORT_MAX) )
    || force_insert)
  {
    newnode = (CP_INFO *)malloc(sizeof(CP_INFO));
    if(!newnode) return -1;

    newnode->tag = CONSTANT_Integer;
    newnode->cpnode.Integer.bytes = cp_big_endian_u4((u4)intVal);

    return cp_insert(class, newnode);
  }

  return -1;
}

/**
 * Inserts a class constant into the constant pool.
 *
 * @param class -- The class containing the constant pool to be searched.
 * @param val -- The constant value to be inserted.
 * @param force_insert -- If FALSE, certain constants will be excluded
 *     depending on the data type/value (see cp_find_or_insert()).  If TRUE,
 *     the constant will be inserted regardless of its value.
 *
 * @returns The constant pool index for the given constant value.
 *   On error, returns -1.
 **/

static int
insert_float_constant(JVM_CLASS *class, const void *val, BOOL force_insert) {
  CP_INFO * newnode;
  float floatVal;

  if(!class || !val)  {
    BAD_ARG();
    return -1;
  }

  floatVal = *((float *)val);

  /* if float value is 0.0, 1.0, or 2.0 then we can use
   * the fconst_<i> opcode.  Thus, there's no
   * need to create a constant pool entry.
   */

  if(( (cp_lookup(class, CONSTANT_Float, (void *)&floatVal) < 0)
    && ( floatVal != 0.0 && floatVal != 1.0 && floatVal != 2.0) )
    || force_insert)
  {
    u4 tmp;
    memcpy(&tmp,&floatVal,sizeof(tmp));

    newnode = (CP_INFO *)malloc(sizeof(CP_INFO));
    if(!newnode) return -1;

    newnode->tag = CONSTANT_Float;
    newnode->cpnode.Float.bytes = cp_big_endian_u4(tmp);

    return cp_insert(class, newnode);
  }

  return -1;
}

/**
 * Inserts a class constant into the constant pool.
 *
 * @param class -- The class containing the constant pool to be searched.
 * @param val -- The constant value to be inserted.
 * @param force_insert -- If FALSE, certain constants will be excluded
 *     depending on the data type/value (see cp_find_or_insert()).  If TRUE,
 *     the constant will be inserted regardless of its value.
 *
 * @returns The constant pool index for the given constant value.
 *   On error, returns -1.
 **/

static int
insert_long_constant(JVM_CLASS *class, const void *val, BOOL force_insert) {
  CP_INFO * newnode;
  u4 tmp1, tmp2;
  u8 longVal;

  if(!class || !val)  {
    BAD_ARG();
    return -1;
  }

  longVal = *((u8 *)val);

  /* if long value is 0 or 1, then we can use
   * the lconst_<i> opcode.  Thus, there's no
   * need to create a constant pool entry.
   */

  if(( (cp_lookup(class, CONSTANT_Long, (void *)&longVal) < 0)
    && ( longVal != 0 && longVal != 1 ) )
    || force_insert)
  {
    newnode = (CP_INFO *)malloc(sizeof(CP_INFO));
    if(!newnode) return -1;

    newnode->tag = CONSTANT_Long;
    memcpy(&tmp1,&longVal,sizeof(tmp1));
    memcpy(&tmp2,(char*)&longVal+4,sizeof(tmp2));
    if(isBigEndian()) {
      newnode->cpnode.Long.high_bytes = tmp1;
      newnode->cpnode.Long.low_bytes = tmp2;
    }
    else {
      newnode->cpnode.Long.high_bytes = cp_big_endian_u4(tmp2);
      newnode->cpnode.Long.low_bytes = cp_big_endian_u4(tmp1);
    }

    return cp_insert(class, newnode);
  }

  return -1;
}

/**
 * Inserts a class constant into the constant pool.
 *
 * @param class -- The class containing the constant pool to be searched.
 * @param val -- The constant value to be inserted.
 * @param force_insert -- If FALSE, certain constants will be excluded
 *     depending on the data type/value (see cp_find_or_insert()).  If TRUE,
 *     the constant will be inserted regardless of its value.
 *
 * @returns The constant pool index for the given constant value.
 *   On error, returns -1.
 **/

static int
insert_double_constant(JVM_CLASS *class, const void *val, BOOL force_insert) {
  unsigned int tmp1, tmp2;
  CP_INFO * newnode;
  double doubleVal;

  if(!class || !val)  {
    BAD_ARG();
    return -1;
  }

  doubleVal = *((double *)val);

  /* if double value is 0.0 or 1.0, then we can use
   * the dconst_<i> opcode.  Thus, there's no
   * need to create a constant pool entry.
   */

  if(( (cp_lookup(class, CONSTANT_Double, (void *)&doubleVal) < 0)
    && ( doubleVal != 0.0 && doubleVal != 1.0 ) )
    || force_insert)
  {
    newnode = (CP_INFO *)malloc(sizeof(CP_INFO));
    if(!newnode) return -1;

    newnode->tag = CONSTANT_Double;
    memcpy(&tmp1,&doubleVal,sizeof(tmp1));
    memcpy(&tmp2,(char*)&doubleVal+4,sizeof(tmp2));
    if(isBigEndian()) {
      newnode->cpnode.Double.high_bytes = tmp1;
      newnode->cpnode.Double.low_bytes = tmp2;
    }
    else {
      newnode->cpnode.Double.high_bytes = cp_big_endian_u4(tmp2);
      newnode->cpnode.Double.low_bytes = cp_big_endian_u4(tmp1);
    }

    return cp_insert(class, newnode);
  }

  return -1;
}

/**
 * Inserts a class constant into the constant pool.
 *
 * @param class -- The class containing the constant pool to be searched.
 * @param val -- The constant value to be inserted.
 * @param force_insert -- If FALSE, certain constants will be excluded
 *     depending on the data type/value (see cp_find_or_insert()).  If TRUE,
 *     the constant will be inserted regardless of its value.
 *
 * @returns The constant pool index for the given constant value.
 *   On error, returns -1.
 **/

static int
insert_string_constant(JVM_CLASS *class, const void *val, BOOL force_insert) {
  CP_INFO * newnode;
  int idx;

  if(!class || !val)  {
    BAD_ARG();
    return -1;
  }

  /* unique string literals always go into the constant pool.
   * first, we have to create a CONSTANT_Utf8 entry for the
   * string itself.  then we create a CONSTANT_String entry
   * whose string_index points to the Utf8 string.
   * 
   * Note that we only malloc enough for the string itself
   * since the Utf8 string should not be null-terminated.
   */

  debug_msg("inserting a string... '%s'\n",(char *)val);

  idx = cp_lookup(class, CONSTANT_Utf8, val);

  if(idx < 0)
  {

    debug_msg("&& in insert_constant, inserting '%s'\n",(char *)val);

    newnode = (CP_INFO *)malloc(sizeof(CP_INFO));
    if(!newnode) return -1;

    newnode->tag = CONSTANT_Utf8;
    newnode->cpnode.Utf8.length = strlen(val);
    newnode->cpnode.Utf8.bytes = (u1 *) malloc(newnode->cpnode.Utf8.length);
    if(!newnode->cpnode.Utf8.bytes) {
      free(newnode);
      return -1;
    }

    strncpy((char *)newnode->cpnode.Utf8.bytes, val, 
        newnode->cpnode.Utf8.length);

    idx = cp_insert(class, newnode);
  }
  else if(idx == 0) {
    debug_err("WARNING insert_constant(): idx is 0\n");
  }

  newnode = (CP_INFO *)malloc(sizeof(CP_INFO));
  if(!newnode) return -1;

  newnode->tag = CONSTANT_String;
  newnode->cpnode.String.string_index = (u2)idx;

  return cp_insert(class, newnode);
}

/**
 * Find the constant pool index for the given constant if it exists in 
 * the constant pool.  If not, create a new entry and return its index.
 * See cp_find_or_insert().
 *
 * @param class -- The class containing the constant pool to be searched.
 * @param tag -- The type of constant contained in the 'value' argument.
 * @param value -- The constant value to be searched for.
 * @param force_insert -- If FALSE, certain constants will be excluded
 *     depending on the data type/value (see cp_find_or_insert()).  If TRUE,
 *     the constant will be inserted regardless of its value.
 *
 * @returns The constant pool index for the given constant value.
 *   If the value was not inserted because it was a special value
 *   as mentioned above or if an error occurred then -1 is returned.
 **/

static int
cp_find_function_body(JVM_CLASS *class, JVM_CONSTANT tag, const void *value,
  BOOL force_insert) 
{
  int temp;

  if(!class || !value) {
    BAD_ARG();
    return -1;
  }

  debug_msg("&& cp_find_or_insert\n");

  /* First, check to see if it's already in the list.  */

  if( (temp = cp_lookup(class,tag,value)) >= 0 ) {

    debug_msg("&& found entry, returning\n");

    return temp;
  }

  debug_msg("&& entry not found, continuing...\n");

  /* It's not in the list, so we insert it and return a pointer to
   * the new node
   */
  switch(tag) {
    case CONSTANT_Class:
      return insert_class(class, value);
    case CONSTANT_Fieldref:
    case CONSTANT_InterfaceMethodref:
    case CONSTANT_Methodref:
      return insert_ref(class, tag, value);
    case CONSTANT_NameAndType:
      return insert_nameandtype(class, value);
    case CONSTANT_Utf8:
      return insert_utf8(class, value);
    case CONSTANT_Integer:
      return insert_constant(class, CP_INTEGER_CONST, value, force_insert);
    case CONSTANT_Float:
      return insert_constant(class, CP_FLOAT_CONST, value, force_insert);
    case CONSTANT_Long:
      return insert_constant(class, CP_LONG_CONST, value, force_insert);
    case CONSTANT_Double:
      return insert_constant(class, CP_DOUBLE_CONST, value, force_insert);
    case CONSTANT_String:
      return insert_constant(class, CP_STRING_CONST, value, force_insert);
    default:
      debug_err("cp_find_or_insert: WARNING - tag not yet implemented!\n");
      return -1;
  }
  
  /* should never hit this return stmt once this function is fully-implemented.
   * still might return NULL from elsewhere if insert_constant returns NULL, 
   * though (e.g. if trying to insert integer 0, etc).
   */

  return -1;
}

