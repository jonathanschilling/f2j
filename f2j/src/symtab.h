
typedef struct hash_node
  {
      char * leaddim;
      struct ast_node *variable;
      char *ident;
      int type;
      int localvarnum;
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

