
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

typedef struct _methodref {
  char *classname,
       *methodname,
       *descriptor;
} METHODREF;

CPNODE * cp_lookup(Dlist, enum _constant_tags, void *);
CPNODE * cp_find_or_insert(Dlist, enum _constant_tags, void *);
CPNODE * cp_entry_by_index(Dlist, int);
CPNODE * cp_insert(Dlist, struct cp_info *, char);
CPNODE * insert_constant(int, char *);
CPNODE * newMethodref(Dlist, char *, char *, char *);
char   * null_term(u1 *, int);
void     cp_dump(Dlist);
void     cp_quickdump(Dlist);
void     cp_initialize(AST *, Dlist);

#endif
