/*
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */

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

/*****************************************************************************
 *                                                                           *
 * Following are some constants that help determine which integer load       *
 * instruction to use.                                                       *
 *                                                                           *
 * if intval < JVM_SHORT_MIN or intval > JVM_SHORT_MAX, use ldc              *
 * else if intval < JVM_BYTE_MIN or intval > JVM_BYTE_MAX, use sipush        *
 * else if intval < JVM_ICONST_MIN or intval > JVM_ICONST_MAX, use bipush    *
 * else use iconst_<intval>                                                  *
 *                                                                           *
 *****************************************************************************/

#define JVM_SHORT_MIN (-32768)
#define JVM_SHORT_MAX 32767
#define JVM_BYTE_MIN  (-128)
#define JVM_BYTE_MAX  127
#define JVM_ICONST_MIN -1
#define JVM_ICONST_MAX 5

/*
 * We build a linked list containing all the constant pool entries.
 * Each entry in the list has the following structure:
 */
typedef struct _constListNode {
  unsigned int index;
  unsigned int next_idx;
  struct cp_info * val;
} CPNODE;

typedef struct _methodref {
  char *classname,
       *methodname,
       *descriptor;
} METHODREF;

METHODREF * newMethodNode(char *, char *, char *);
METHODREF * find_method(char *, Dlist);
CPNODE    * cp_lookup(Dlist, enum _constant_tags, void *);
CPNODE    * cp_find_or_insert(Dlist, enum _constant_tags, void *);
CPNODE    * cp_find_function_body(Dlist, enum _constant_tags, void *);
CPNODE    * cp_entry_by_index(Dlist, unsigned int);
CPNODE    * cp_insert(Dlist, struct cp_info *, unsigned int);
CPNODE    * insert_constant(Dlist, int, void *);
CPNODE    * newMethodref(Dlist, char *, char *, char *);
CPNODE    * newFieldref(Dlist, char *, char *, char *);
char      * null_term(u1 *, unsigned int);
void        cp_dump(Dlist);
void        cp_quickdump(Dlist);
void        fields_dump(Dlist, Dlist);

#endif
