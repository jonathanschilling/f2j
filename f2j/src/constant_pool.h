
/*****************************************************************************
 * constant_pool.h                                                           *
 *                                                                           *
 * Definitions and prototypes for the constant pool routines.                *
 *                                                                           *
 *****************************************************************************/

#include"class.h"
#include"f2j.h"

/*
 * We build a linked list containing all the constant pool entries.
 * Each entry in the list has the following structure:
 */
typedef struct _constListNode {
  char * id;
  int index;
  int next_idx;
  struct cp_info * val;
} CPNODE;

CPNODE * cp_lookup(Dlist, char *);
int      cp_insert(Dlist, struct cp_info *, char *, char);
void     cp_dump(Dlist);
void     cp_initialize(AST *, Dlist);
