#include "list.h"
#define TRUE 1
#define FALSE 0

list_push(EntryList **list, int value)
{
   EntryList *new_node;

   if((new_node = (EntryList *) malloc(sizeof(EntryList))) == NULL)
   {
     perror("malloc error in list_insert");
     exit(EX_OSERR);
   }
   new_node->value = value;
   new_node->next = *list;
   *list = new_node;
}

int
list_pop(EntryList **list)
{
  EntryList *temp;
  int intval=0;

  temp = *list;
  intval = temp->value;
  *list = temp->next;
  free(temp); 
  return(intval);
}

/* the list here doesn't need to be a double-pointer
   but I'm leaving it that way for consistency with
   the other functions */
int
list_examine(EntryList **list)
{
  return ( (*list)->value );
}

/* this one doesn't need to be a double pointer either */

int
list_search(EntryList **list, int val)
{
  EntryList *temp;

  for(temp=*list;temp!=NULL;temp=temp->next)
    if(val == temp->value)
      return (TRUE);  

  return (FALSE);
}

list_display(EntryList *list)
{
  EntryList *asdf;
  int val;

  asdf = list;

  for(asdf=list;asdf!=NULL;asdf=asdf->next) {
     val = asdf->value;
     printf("%d,", val);
  }
  printf("\n");
}
