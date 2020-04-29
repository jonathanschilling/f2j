/*
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */


#ifndef _SYMTAB_H
#define _SYMTAB_H

/*****************************************************************************
 * symtab.h                                                                  *
 *                                                                           *
 * Header file for the symbol table routines.                                *
 *                                                                           *
 *****************************************************************************/

#include<string.h>
#include "dlist.h"

/* Enumeration of the different return types */

enum returntype
{
  String, Character, Complex, Double, Float, Integer, Logical, Object
};

/*****************************************************************************
 * Structure of a hash table node.                                           *
 *****************************************************************************/

typedef struct hash_node
{
  struct ast_node *variable;     /* The variable corresponding to this entry */
  char *ident;                   /* String tag                               */
  enum returntype type;          /* The variable's data type                 */
  struct hash_node *next;        /* Next node                                */
}
HASHNODE;

/*****************************************************************************
 * Function prototypes to keep the compiler from complaining.                *
 *****************************************************************************/

typedef struct sym_table
{
  int num_entries,               /* Number of entries in this hash table     */
      num_items;                 /* Number of items stored in hash table     */

  HASHNODE **entry;              /* Pointer to the entries                   */
}
SYMTABLE;

/*****************************************************************************
 * Function prototypes to keep the compiler from complaining.                *
 *****************************************************************************/

HASHNODE 
  * search_hashlist(HASHNODE *, char *),
  * type_lookup(SYMTABLE *, char *),
  * hash_delete(SYMTABLE *, char *);

Dlist enumerate_symtable(SYMTABLE *);


#endif
