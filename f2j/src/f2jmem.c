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
 * free_ast_node                                                             *
 *                                                                           *
 *                                                                           *
 *****************************************************************************/

void
free_ast_node(AST *n)
{
  if( n == NULL )
    return;

  switch(n->nodetype) {
    case Identifier:
    case Constant:
    case Typedec:
    case Assignment:
      break;
    case IoExplist:
      /* currently we should ignore this */
      break;
    case Expression:
      free_ast_node(n->astnode.expression.rhs);
      break;
    case Binaryop:
    case Power:
      free_ast_node(n->astnode.expression.lhs);
      free_ast_node(n->astnode.expression.rhs);
      break;
    default:
      fprintf(stderr,"free_ast_node() warning: unsupported node %s.\n", 
         print_nodetype(n));
      break; /*ansi*/
  }

  f2jfree(n, sizeof(AST));
}
