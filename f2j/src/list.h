#include<stdio.h>
#include<sysexits.h>

typedef struct entry_node {
   int value;
   struct entry_node *next;
} EntryList;

