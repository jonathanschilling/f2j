
typedef struct hash_node
  {
      int val;
      char *ident;
      struct hash_node *next;
  }
HASHNODE;


typedef struct sym_table
  {
      int num_entries;
      HASHNODE **entry;
  }
SYMTABLE;

/*  Prototypes. */

HASHNODE * search_hashlist(HASHNODE *, char *);
HASHNODE * type_lookup(SYMTABLE *, char *);
SYMTABLE * new_symtable(int);
