/*
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */

/*****************************************************************************
 * f2jmem.c                                                                  *
 *                                                                           *
 * This file contains the memory management routines for f2j.                *
 *                                                                           *
 *****************************************************************************/

#include"f2jmem.h"

/*****************************************************************************
 *                                                                           *
 * f2jfree                                                                   *
 *                                                                           *
 * Wrapper around free which may overwrite the memory such that we can find  *
 * problems early (only if DEBUG_MEM is defined).                            *
 *                                                                           *
 *****************************************************************************/

void
f2jfree(void *p, size_t size)
{
#ifdef DEBUG_MEM
printf("going to free %p\n", p);
  memset(p, 0xA, size);
#endif

  free(p);
}

/*****************************************************************************
 *                                                                           *
 * f2jalloc                                                                  *
 *                                                                           *
 * Error-checking memory allocation routine for f2java.  we can't recover    *
 * from an out of memory condition, so we'll just call exit() which will     *
 * close all open streams for us.                                            *
 *                                                                           *
 *****************************************************************************/

void *
f2jalloc(size_t numbytes)
{
  void * mem = malloc(numbytes);

  if(mem == NULL)
    alloc_error(numbytes);

  return mem;
}

/*****************************************************************************
 *                                                                           *
 * f2jcalloc                                                                 *
 *                                                                           *
 * Error-checking memory allocation routine for f2java.  we can't recover    *
 * from an out of memory condition, so we'll just call exit() which will     *
 * close all open streams for us.                                            *
 *                                                                           *
 *****************************************************************************/

void *
f2jcalloc(size_t numitems, size_t numbytes)
{
  void * mem = calloc(numitems, numbytes);

  if(mem == NULL)
    alloc_error(numbytes);

  return mem;
}

/*****************************************************************************
 *                                                                           *
 * f2jrealloc                                                                *
 *                                                                           *
 * Error-checking memory allocation routine for f2java.  we can't recover    *
 * from an out of memory condition, so we'll just call exit() which will     *
 * close all open streams for us.                                            *
 *                                                                           *
 *****************************************************************************/

void *
f2jrealloc(void *ptr, size_t size)
{
  void *mem = realloc(ptr, size);

  if(mem == NULL)
    alloc_error(size);

  return mem;
}

/*****************************************************************************
 *                                                                           *
 * alloc_error                                                               *
 *                                                                           *
 * called when there is an error allocating memory.  this function prints    *
 * an error message and exits.                                               *
 *                                                                           *
 *****************************************************************************/

void
alloc_error(size_t size)
{
  fprintf(stderr,"f2java: Error allocating %d bytes of memory.  Stopping.\n",
     (int)size);
  perror("Reason:");
  exit(1);
}

/*****************************************************************************
 *                                                                           *
 * free_var_info                                                             *
 *                                                                           *
 * frees a variable info structure.                                          *
 *                                                                           *
 *****************************************************************************/

void
free_var_info(struct var_info *v)
{
  f2jfree(v->name, strlen(v->name)+1);
  f2jfree(v->desc, strlen(v->desc)+1);
  f2jfree(v->class, strlen(v->class)+1);
  f2jfree(v, sizeof(struct var_info));
}

/*****************************************************************************
 *                                                                           *
 * freeFieldref                                                              *
 *                                                                           *
 * this function frees memory previously allocated for a fieldref.           *
 *                                                                           *
 *****************************************************************************/

void
free_fieldref(METHODREF *fieldref)
{
  f2jfree(fieldref->classname, strlen(fieldref->classname) + 1);
  f2jfree(fieldref->methodname, strlen(fieldref->methodname) + 1);
  f2jfree(fieldref->descriptor, strlen(fieldref->descriptor) + 1);
  f2jfree(fieldref, sizeof(METHODREF));
}

/*****************************************************************************
 * free_constant_pool                                                        *
 *                                                                           *
 *****************************************************************************/

void
free_class(struct ClassFile *class)
{
  free_interfaces(class);
  free_fields(class);
  free_methods(class);
  free_attributes(class->attributes,class->constant_pool);

  /* NOTE: must free constant pool last. */
  free_constant_pool(class);

  f2jfree(class, sizeof(struct ClassFile));
}

/*****************************************************************************
 * free_constant_pool                                                        *
 *                                                                           *
 *****************************************************************************/

void
free_constant_pool(struct ClassFile *class)
{
  CPNODE * tmpconst;
  Dlist tmpPtr;

  dl_traverse(tmpPtr,class->constant_pool) {
    tmpconst = (CPNODE *) tmpPtr->val;

    if(tmpconst->val->tag == CONSTANT_Utf8)
      f2jfree(tmpconst->val->cpnode.Utf8.bytes,
              tmpconst->val->cpnode.Utf8.length);
    f2jfree(tmpconst->val, sizeof(struct cp_info));
    f2jfree(tmpconst, sizeof(CPNODE));
  }

  dl_delete_list(class->constant_pool);
  f2jfree(class->constant_pool, sizeof(Dlist));
}

/*****************************************************************************
 * free_interfaces                                                           *
 *                                                                           *
 *                                                                           *
 *****************************************************************************/

void
free_interfaces(struct ClassFile *class)
{

  /* intentionally empty */

}

/*****************************************************************************
 * free_fields                                                               *
 *                                                                           *
 *****************************************************************************/

void
free_fields(struct ClassFile *class)
{
  struct field_info *tmpfield;
  Dlist tmpPtr;

  dl_traverse(tmpPtr,class->fields) {
    tmpfield = (struct field_info *) tmpPtr->val;
    if(tmpfield->attributes_count > 0)
      fprintf(stderr,"free_fields(): not expecting field attributes!\n");
    f2jfree(tmpfield, sizeof(struct field_info));
  }

  dl_delete_list(class->fields);
  f2jfree(class->fields, sizeof(Dlist));
}

/*****************************************************************************
 * free_methods                                                              *
 *                                                                           *
 *                                                                           *
 *****************************************************************************/

void
free_methods(struct ClassFile *class)
{
  struct method_info *tmpmeth;
  Dlist tmpPtr;

  dl_traverse(tmpPtr,class->methods) {
    tmpmeth = (struct method_info *) tmpPtr->val;

    free_attributes(tmpmeth->attributes,class->constant_pool);
    f2jfree(tmpmeth, sizeof(struct method_info));
  }

  dl_delete_list(class->methods);
  f2jfree(class->methods, sizeof(Dlist));
}

/*****************************************************************************
 * free_attributes                                                           *
 *                                                                           *
 *                                                                           *
 *****************************************************************************/

void
free_attributes(Dlist attr_list, Dlist const_pool)
{
  struct attribute_info *tmpattr;
  char *attr_name;
  Dlist tmpPtr, tmpPtr2;
  CPNODE *c;

  if((attr_list == NULL) || (const_pool == NULL))
    return;

  dl_traverse(tmpPtr,attr_list) {
    tmpattr = (struct attribute_info *) tmpPtr->val;

    c = cp_entry_by_index(const_pool, tmpattr->attribute_name_index);
 
    if(c==NULL) {
      fprintf(stderr,"WARNING: free_attributes() can't find attribute name\n");
      continue;
    } 
      
    attr_name = null_term(c->val->cpnode.Utf8.bytes,c->val->cpnode.Utf8.length);

    if(!strcmp(attr_name,"SourceFile")) {
      f2jfree(tmpattr->attr.SourceFile, sizeof(struct SourceFile_attribute));
    }
    else if(!strcmp(attr_name,"Code")) {
      free_code(tmpattr->attr.Code->code);

      if(tmpattr->attr.Code->exception_table_length > 0)
        f2jfree(tmpattr->attr.Code->exception_table, 
           sizeof(struct ExceptionTable) *
           tmpattr->attr.Code->exception_table_length); 

      if(tmpattr->attr.Code->attributes_count > 0)
        free_attributes(tmpattr->attr.Code->attributes, const_pool);

      f2jfree(tmpattr->attr.Code, sizeof(struct Code_attribute));
    }
    else if(!strcmp(attr_name,"Exceptions")) {
      dl_traverse(tmpPtr2, tmpattr->attr.Exceptions->exception_index_table)
        f2jfree(tmpPtr2->val, sizeof(int));

      dl_delete_list(tmpattr->attr.Exceptions->exception_index_table);
      f2jfree(tmpattr->attr.Exceptions->exception_index_table, sizeof(Dlist));
      f2jfree(tmpattr->attr.Exceptions, sizeof(struct Exceptions_attribute));
    }

    f2jfree(tmpattr, sizeof(struct attribute_info));
    f2jfree(attr_name, strlen(attr_name)+1);
  }

  dl_delete_list(attr_list);
  f2jfree(attr_list, sizeof(Dlist));
}

/*****************************************************************************
 * free_code                                                                 *
 *                                                                           *
 *                                                                           *
 *****************************************************************************/

void
free_code(Dlist g)
{
  Dlist tmp;

  dl_traverse(tmp, g)
    f2jfree(tmp->val, sizeof(CodeGraphNode));

  dl_delete_list(g);
  f2jfree(g, sizeof(Dlist));
}
