
/*****************************************************************************
 * constant_pool.h                                                           *
 *                                                                           *
 * Definitions and prototypes for the constant pool routines.                *
 *                                                                           *
 *****************************************************************************/

#ifndef _CONSTANT_POOL_H
#define _CONSTANT_POOL_H

#include"class.h"
#include"f2j.h"

/*
 * We build a linked list containing all the constant pool entries.
 * Each entry in the list has the following structure:
 */
typedef struct _constListNode {
  int index;
  int next_idx;
  struct cp_info * val;
} CPNODE;

CPNODE * cp_lookup(Dlist, enum _constant_tags, void *);
int      cp_insert(Dlist, struct cp_info *, char *, char);
void     cp_dump(Dlist);
void     cp_initialize(AST *, Dlist);

#endif
