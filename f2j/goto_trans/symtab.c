#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include "symtab.h"

#define symdebug 0

char *strdup(const char *);
void *malloc(size_t);
void *calloc(size_t, size_t);

SYMTABLE *
new_symtable (int numentries)
{
    SYMTABLE *newtable;
    newtable = (SYMTABLE *) malloc (sizeof (SYMTABLE));

    /* Handle out-of-mem. */
    if (newtable == NULL)
      {
	  perror ("malloc error creating new symboltable");
	  exit (-1);
      }

    newtable->num_entries = numentries;
    newtable->entry = (HASHNODE **) calloc (numentries, sizeof (HASHNODE *));

    /* Handle out-of-mem. */
    if (newtable->entry == NULL)
      {
	  perror ("calloc error creating new symbol table");
	  exit (-1);
      }

    return (newtable);
}				/*  Close new_symtable().  */

void
type_insert (HASHNODE ** list, int node_val, char *tag)
{

    HASHNODE *newnode;

    newnode = (HASHNODE *) malloc (sizeof (HASHNODE));
    newnode->ident = tag;
    newnode->val = node_val;

    /*  Note carefully the dereferencing operators. */
    newnode->next = *list;
    *list = newnode;
}


/*  This is a specific lookup routine to match an id with 
   its associated type.  I will need others for matching 
   externals, intrinsics, etc.  */
HASHNODE *
type_lookup (SYMTABLE * table, char *id)
{
  int index;
  HASHNODE *hash_entry;
  int hash(char *);

  if((table == NULL) || (id == NULL)) {
    return NULL;
  }

  index = hash (id) % table->num_entries;

  hash_entry = search_hashlist (table->entry[index], id);
  if (hash_entry == NULL)
  {
    if(symdebug)printf ("Not in table.\n"); 
      return NULL;
  }
  else   /*  Attempt to return the value pointed to by "type". */
  {
    if(symdebug)printf("In table.\n");
      return (hash_entry);
  }
}

HASHNODE * format_lookup(SYMTABLE *table, char *label)
{
  return type_lookup(table,label);
}

HASHNODE *
search_hashlist (HASHNODE * list, char *id)
{

  if(id == NULL)
    return NULL;

  for (; list != NULL ; list = list->next)
  {
    if(list->ident == NULL)
      continue;
    if (!strcmp (list->ident, id))
      return (list);
  }

  return NULL;		/*  Not in list. */
}


/*  Simple hash function: just add the ascii integer
    values of each character in the string. 

    Added error check for null string and made some
    other minor changes.  12/5/97  --Keith
 */ 

int
hash (char *str)
{
    int sum = 0;
    int i=0, len;

    if(str == NULL)
      return 0;

    len = strlen(str);

    while (i < len)
    {
      sum += (int) str[i];
      i++;
    }
    return sum;
}
