
/* *************
   *** JAVAB ***
   ****************************************************
   *** Copyright (c) 1997                           ***
   *** Aart J.C. Bik             Indiana University *** 
   *** All Rights Reserved                          ***
   ****************************************************
   *** Please refer to the LICENSE file distributed ***
   *** with this software for further details on    ***
   *** the licensing terms and conditions.          ***
   ***                                              ***
   *** Please, report all bugs, comments, etc.      ***
   *** to: ajcbik@extreme.indiana.edu               ***
   ****************************************************
   *** main.c : control program
   ***
   ***
   *** Your courtesy in mentioning the use of this bytecode tool
   *** in any scientific work that presents results obtained
   *** by using (extensions or modifications of) the tool
   *** is highly appreciated.
   ***
   *** */

/* ********************************************************
   *** INCLUDE FILES and DEFINITIONS                    ***
   ********************************************************  */

#include <stdarg.h>
#include "class.h"

#undef AUTO_QUERY

/* ********************************************************
   *** EXTERNAL VARIABLES                               ***
   ******************************************************** */

/* PUBLIC  
   ****** */

u1_int  my_not = 4u;
u1_int  error  = 0u;
u4_int  n_par  = 0u, n_nest  = 0u, n_loop = 0u, n_triv  = 0u;
char   *filename = NULL;

/* PRIVATE
   ******* */

static u1_int tot_err   = 0u; 
static u4_int files     = 0u;
static FILE   *file     = NULL;

/* ********************************************************
   *** PRIVATE FUNCTIONS                                ***
   ******************************************************** */

/* Move original  'file.ext' to 'file.old'
   and open a new 'file.ext' for output
   ************************************************* */

static FILE *new_file(char *oldname) {
  int    i,  last = -1;
  char   *newname = NULL, c = '\0';
  FILE   *newfile = NULL;

  if (! oldname)   /* Safety */
    javab_out(-1, "incorrect invocation of new_file()");

  /* Construct new name file.old by stripping 
     last extension from original file.ext      */

  for (i = 0; oldname[i]; i++) 
    if (oldname[i] == '.') 
      last = i;

  if (last >= 0) {
    c = oldname[last];
    oldname[last] = '\0';
  }

  /* DYNAMIC MEMORY ALLOCATION -> no restriction on length */

  newname = (char *) make_mem(sizeof(char) * (strlen(oldname) + 6));
  sprintf(newname, "%s.old", oldname);

  if (last >= 0) 
    oldname[last] = c;

  /* Prevent Overwriting of existing file.old */

  if((newfile = fopen(newname, "r"))) {
    javab_out(0, "cannot apply javab to %s"
		 " (%s exists)", oldname, newname);
    fclose(newfile);
    newfile = NULL;
  }
  else {
  
    /* DYNAMIC MEMORY ALLOCATION -> no restriction on length */

    char *command = (char *) make_mem(sizeof(char) * 
                        (strlen(oldname) + strlen(newname) + 5));

    sprintf(command, "mv %s %s", oldname, newname);

    /* Re-name original file.ext to file.old */

    javab_out(2, " -- executing '%s'", command);

    if (system(command))
      javab_out(0, "command %s failed", command);
    else {

      /* Re-open original file.ext for new output */

      if (! (newfile = fopen(oldname, "w")))
        javab_out(0, "cannot open file %s", oldname); 
      else
       javab_out(2, " -- output to file %s", oldname);
    }
    free(command);
  } 
  free(newname);

  return newfile;
}

/* Process a Class File
   ******************** */

static void process(void) {
  FILE *outfile;

  files++;
  error = 0;

  process_classfile(file, 0u); /* read   */
 /*  process_classfile(NULL, 1u);  */   /* verify */

  byte_proc();

  outfile = new_file(filename);
  	
  if (outfile) {
    dump_classfile(outfile);
    fclose(outfile);
  }
}

/* ********************************************************
   *** PUBLIC FUNCTIONS                                 ***
   ******************************************************** */

/* ****************************
   *** User input on Query  ***
   ****************************
   *** y/Y : yes            ***
   *** n/N : no             ***
   *** q/Q : no, quit query ***
   **************************** */

u1_int query(void) {
  char   str[80];
  u1_int res = 2u;

#ifdef AUTO_QUERY
  fprintf(stderr, "(y/n/q) => Y\n");
  return 1u;
#endif

  do { 
  
    fprintf(stderr, "(y/n/q) => ");
    fflush(stderr);

    fgets(str, 80, stdin);

    if (strlen(str) != 0)
     switch (str[0]) {

      case 'y':
      case 'Y':
	res = 1u;
	break;

      case 'n':
      case 'N':
	res = 0u;
	break;

      case 'q':
      case 'Q':
	res     = 0u;
	break;
    }
  }
  while (res == 2u);

  return res;
}

/* **********************************
   *** Error and Warning Messages ***
   ***************************************************************
   *** level == -1 : FATAL ERROR,     EXIT PROGRAM             ***
   *** level ==  0 : ERRROR,          SET ERROR FLAG           ***
   *** level ==  1 : STRONG MESSAGE                            ***
   *** level ==  2 : MESSAGE,         PRINT ONLY FOR '-v'      ***
   *************************************************************** */

void javab_out(s1_int level, char *fmt, ...)  {
  va_list argv;

  if (level == 0)
     tot_err = error = 1; 
  /* else if (level > 1 && ! s_verbose) */
  else if (level > 1)
    return;
    
  va_start(argv, fmt);

  if (file)
    fprintf(stderr, "%s:: ", (filename) ? filename : "stdin");

  vfprintf(stderr, fmt, argv);
  putc('\n', stderr);

  va_end(argv);

  if (level < 0)
    exit(1);
}

/* Memory Allocation Functions
   [for size <= 0, size == 1 is used
    because some systems return NULL for malloc(0);]
   ************************************************* */

void *make_mem(int size) { 
  void *p = calloc(((size > 0) ? size : 4), sizeof(u1_int)); 
  if (! p) 
    javab_out(-1, "Out of Memory");
  return p; 
}

void *more_mem(void *p, int size) { 
  if (p) {
    p = realloc(p, ((size > 0) ? size : 1));
    if (! p) 
      javab_out(-1, "Out of Memory (re-allocation)");
    return p;
  }
  else
    return make_mem(size);
}

/* ********************
   *** Main Program ***
   ******************** */

int main(int argc, char *argv[]) { 
  int    i;
  u1_int unproc = 1;

  /* Process Environment Variable */

  char *env = getenv("JAVAB_THREADS");

  if (env) {
    my_not = (u1_int) atoi(env);

    if (my_not < 2u)
      my_not = 4u;
    else if (my_not > 16u)
      my_not = 16u;
  }

  for (i = 1; i < argc; i++) {

    if (argv[i]) { 
      file   = fopen(filename = argv[i], "r");
      unproc = 0;
	
      if (file) {
        process();
        fclose(file);
      }
      else
        javab_out(0, "cannot open file %s", filename);
    }
  }

  /* Process Standard Input by Default */

  if (unproc) {
    file = stdin;
    process();
  }

  return tot_err;
}

