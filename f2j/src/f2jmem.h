#ifndef F2JMEM_H
#define F2JMEM_H

#include"f2j.h"
#include"constant_pool.h"

void
  alloc_error(size_t),
  f2jfree(void *, size_t),
  free_fieldref(METHODREF *),
  free_var_info(struct var_info *),
  * f2jalloc(size_t),
  * f2jcalloc(size_t, size_t),
  * f2jrealloc(void *, size_t),
  free_class(struct ClassFile *),
  free_constant_pool(struct ClassFile *),
  free_interfaces(struct ClassFile *),
  free_fields(struct ClassFile *),
  free_methods(struct ClassFile *),
  free_attributes(Dlist, Dlist),
  free_ast_node(AST *),
  free_code(Dlist);

#endif
