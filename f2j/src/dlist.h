
#ifndef _DLIST_H
#define _DLIST_H

typedef struct dlist {
  struct dlist *flink;
  struct dlist *blink;
  void *val;
} *Dlist;

/* Nil, first, next, and prev are macro expansions for list traversal 
 * primitives. */

#define dl_nil(l) (l)

#define dl_first(l) (l->flink)

#define dl_last(l) (l->blink)

#define dl_next(n) (n->flink)

#define dl_prev(n) (n->blink)

/* These are the routines for manipluating lists */

extern Dlist make_dl();
extern void dl_insert_b(/* node, val */); /* Makes a new node, and inserts it before
                                        the given node -- if that node is the 
                                        head of the list, the new node is 
                                        inserted at the end of the list */
#define dl_insert_a(n, val) dl_insert_b(n->flink, val)

extern void dl_delete_node(/* node */);    /* Deletes and free's a node */

extern void dl_delete_list(/* head_node */);  /* Deletes the entire list from
                                            existance */
extern void *dl_val(/* node */);   /* Returns node->val (used to shut lint
				      up) */
extern void *dl_pop(/* head_node */);  /* returns the first node and removes
                                          it from the list */

#define dl_traverse(ptr, list) \
  for (ptr = dl_first(list); ptr != dl_nil(list); ptr = dl_next(ptr))
#define dl_empty(list) (list->flink == list)

#endif
