/*
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */

/*****************************************************************************
 * symtab.h                                                                  *
 *                                                                           *
 * Header file for the symbol table routines.                                *
 *                                                                           *
 *****************************************************************************/


/*****************************************************************************
 * Structure of a hash table node.                                           *
 *****************************************************************************/

typedef struct hash_node
{
  struct ast_node *variable;     /* The variable corresponding to this entry */
  char *ident;                   /* String tag                               */
  int type;                      /* The variable's data type                 */
  int localvarnum;               /* Local variable number                    */
  struct hash_node *next;        /* Next node                                */
}
HASHNODE;

/*****************************************************************************
 * Function prototypes to keep the compiler from complaining.                *
 *****************************************************************************/

typedef struct sym_table
{
  int num_entries;               /* Number of entries in this hash table     */
  HASHNODE **entry;              /* Pointer to the entries                   */
}
SYMTABLE;

/*****************************************************************************
 * Function prototypes to keep the compiler from complaining.                *
 *****************************************************************************/

HASHNODE 
  * search_hashlist(HASHNODE *, char *),
  * type_lookup(SYMTABLE *, char *);
