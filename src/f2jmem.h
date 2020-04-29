/*
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */


#ifndef F2JMEM_H
#define F2JMEM_H

#include"f2j.h"

void
  alloc_error(size_t),
  f2jfree(void *, size_t),
  free_var_info(struct var_info *),
  * f2jalloc(size_t),
  * f2jcalloc(size_t, size_t),
  * f2jrealloc(void *, size_t),
  free_ast_node(AST *);

#endif
