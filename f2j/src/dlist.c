/* 
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */

#include <stdio.h>    /* Basic includes and definitions */
#include "dlist.h"

#define boolean int
#define TRUE 1
#define FALSE 0


/*---------------------------------------------------------------------*
 * PROCEDURES FOR MANIPULATING DOUBLY LINKED LISTS 
 * Each list contains a sentinal node, so that     
 * the first item in list l is l->flink.  If l is  
 * empty, then l->flink = l->blink = l.            
 *---------------------------------------------------------------------*/

Dlist make_dl()
{
  Dlist d;

  d = (Dlist) malloc (sizeof(struct dlist));
  d->flink = d;
  d->blink = d;
  d->val = (void *) 0;
  return d;
}
 
dl_insert_b(node, val)	/* Inserts to the end of a list */
Dlist node;
void *val;
{
  Dlist last_node, new;

  new = (Dlist) malloc (sizeof(struct dlist));
  new->val = val;

  last_node = node->blink;

  node->blink = new;
  last_node->flink = new;
  new->blink = last_node;
  new->flink = node;
}

dl_insert_list_b(node, list_to_insert)
Dlist node;
Dlist list_to_insert;
{
  Dlist last_node, f, l;

  if (dl_empty(list_to_insert)) {
    free(list_to_insert);
    return;
  }
  f = list_to_insert->flink;
  l = list_to_insert->blink;
  last_node = node->blink;

  node->blink = l;
  last_node->flink = f;
  f->blink = last_node;
  l->flink = node;
  free(list_to_insert);
}

dl_delete_node(item)		/* Deletes an arbitrary iterm */
Dlist item;
{
  item->flink->blink = item->blink;
  item->blink->flink = item->flink;
  free(item);
}

dl_delete_list(l)
Dlist l;
{
  Dlist d, next_node;

  d = l->flink;
  while(d != l) {
    next_node = d->flink;
    free(d);
    d = next_node;
  }
  free(d);
}

void *dl_val(l)
Dlist l;
{
  return l->val;
}

void*
dl_pop(li)
Dlist li;
{
  Dlist item = dl_last(li);

  if(item == NULL)
    return NULL;

  item->flink->blink = item->blink;
  item->blink->flink = item->flink;

  return(dl_val(item));
}
