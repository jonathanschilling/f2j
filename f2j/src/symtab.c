#include<stdio.h>
#include "f2j.h"

#define symdebug 0

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

/*  hash_insert is a general wrapper for inserting stuff
   into the appropriate hash table.  It should probably
   have a switch/case structure to decide the appropriate
   hash entry insertion procedure to use.  */
int
hash_insert (SYMTABLE * table, AST * node)
{

    int index;
    char *hashid = node->astnode.ident.name;

    index = hash (hashid) % table->num_entries;


    /* Search the list associated with the hash index to
       see whether that variable is already in the 
       symbol table.  */
    if (search_hashlist (table->entry[index], hashid) != NULL)
      {
	/* 	printf("Duplicate entry.\n"); */
	/*	return (-1);  */
      }

    /* Else, decide how to insert the information based on the
       type of node of the ast that is passed in.  */
    switch (node->nodetype)
      {
      case Typedec:
	  {
	      AST *temp;
	      int returntype = node->astnode.typeunit.returns;
	      temp = node->astnode.typeunit.declist;

	      while (temp->nextstmt != NULL)
		{
		    /* Call appropriate insertion routine
		       to insert the ast and returntype.  */
		    type_insert (&(table->entry[index]), temp, returntype);
		    temp = temp->nextstmt;
		}
	      /* Then insert the last stmt.  */
	      type_insert (&(table->entry[index]), temp, returntype);
	  }
	  break;
      }				/* Close switch().  */
    return (1);
}

int
type_insert (HASHNODE ** list, AST * node_val, int returntype)
{

    HASHNODE *newnode;

    newnode = (HASHNODE *) malloc (sizeof (HASHNODE));
    newnode->ident = node_val->astnode.ident.name;
    newnode->type = returntype;
    newnode->variable = node_val;
    /*  Initialize to zero; assign number after all types
	are declared.  */
    newnode->localvarnum = -1;

    /*  Note carefully the dereferencing operators. */
    newnode->next = *list;
    *list = newnode;
}


#ifdef GETRIDOFTHISLATER
int
local_insert (HASHNODE ** list, AST * node_val, int returntype)
{

static int localvarnum = 0;

    HASHNODE *newnode;

    newnode = (HASHNODE *) malloc (sizeof (HASHNODE));
    newnode->ident = node_val->astnode.ident.name;
    newnode->type = returntype;
    newnode->variable = node_val;
    newnode->localvarnum = localvarnum;

    newnode->next = *list;
    *list = newnode;
localvarnum++;
}
#endif


/*  This is a specific lookup routine to match an id with 
   its associated type.  I will need others for matching 
   externals, intrinsics, etc.  */
HASHNODE *
type_lookup (SYMTABLE * table, char *id)
{
    int index;
    HASHNODE *hash_entry;
    index = hash (id) % table->num_entries;

    hash_entry = search_hashlist (table->entry[index], id);
    if (hash_entry == NULL)
      {
	  if(symdebug)printf ("Not in table.\n"); 
	  return NULL;
      }
    /*  Attempt to return the value pointed top by 
       "type". */
    else
      {
      	if(symdebug)printf("In table.\n");
	return (hash_entry);
      }
}


HASHNODE *
search_hashlist (HASHNODE * list, char *id)
{
  /*HASHNODE *hash_entry;*/

    /*for (hash_entry = list; hash_entry != NULL; hash_entry = hash_entry->next)*/
    for (list; list; list = list->next)

      {
	  if (!strcmp (list->ident, id))
	    {
		return (list);
	    }
      }
    return NULL;		/*  Not in list. */
}


/*  Simple hash function: just add the ascii integer
    values of each character in the string. */
int
hash (char *str)
{
    int sum = 0;
    while (*str)
      {
	  sum += (int) *str;
	  str++;
      }
    return sum;
}