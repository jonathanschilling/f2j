/* 
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */

/* Jim Plank's dlist routines.  Contact plank@cs.utk.edu  */

#include <stdio.h>    /* Basic includes and definitions */
#include <stdlib.h>
#include "dlist.h"
#include "f2j.h"
#include "f2jmem.h"
#include "f2j_externs.h"

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

  d = (Dlist) f2jalloc (sizeof(struct dlist));
  d->flink = d;
  d->blink = d;
  d->val = (void *) 0;
  return d;
}
 
void
dl_insert_b(node, val)	/* Inserts to the end of a list */
Dlist node;
void *val;
{
  Dlist last_node, new;

  new = (Dlist) f2jalloc (sizeof(struct dlist));
  new->val = val;

  last_node = node->blink;

  node->blink = new;
  last_node->flink = new;
  new->blink = last_node;
  new->flink = node;
}

void
dl_insert_list_b(Dlist node, Dlist list_to_insert)
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

void
dl_delete_node(item)		/* Deletes an arbitrary iterm */
Dlist item;
{
  item->flink->blink = item->blink;
  item->blink->flink = item->flink;
  free(item);
}

void
dl_delete_list(l)
Dlist l;
{
  Dlist d, next_node;
 
  if(l == NULL)
    return;

  d = l->flink;
  while(d != l) {
    next_node = d->flink;
    free(d);
    d = next_node;
  }
  free(d);
}

void *
dl_val(l)
Dlist l;
{
  return l->val;
}

void*
dl_pop(li)
Dlist li;
{
  Dlist item = dl_last(li);
  void *tmp;

  if(item == NULL)
    return NULL;

  item->flink->blink = item->blink;
  item->blink->flink = item->flink;

  tmp = dl_val(item);
  f2jfree(item, sizeof(Dlist));

  return tmp;
}

/*****************************************************************************
 *                                                                           *
 * in_dlist                                                                  *
 *                                                                           *
 * returns 1 if it is in the list 0 otherwise                                *
 *                                                                           *
 *****************************************************************************/
int
in_dlist(Dlist list, char *name){
   Dlist ptr;
   char *list_name;

   dl_traverse(ptr, list){
      list_name = (char *)dl_val(ptr);
      if(!strcmp(list_name, name))
         return 1;
   }

   return 0;   
}
