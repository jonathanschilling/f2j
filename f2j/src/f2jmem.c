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

#include"f2j.h"
#include"f2jparse.tab.h"
#include"dlist.h"
#include"constant_pool.h"

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
