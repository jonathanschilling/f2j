/*
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */

/*****************************************************************************
 * f2jmain.c                                                                 *
 *                                                                           *
 * This file contains the  main routine for the Fortran-to-Java translator.  *
 *                                                                           *
 *****************************************************************************/

#include<stdlib.h>
#include<sys/types.h>
#include<dirent.h>
#include<stdio.h>
#include<stdarg.h>
#include<ctype.h>
#include<string.h>
#include<time.h>
#include<signal.h>
#include"f2j.h"
#include"f2jparse.tab.h"
#include"dlist.h"
#include"constant_pool.h"

extern char *java_reserved_words[];
extern char *jasmin_reserved_words[];
extern char *blas_routines[];
extern char *generic_intrinsics[];
extern char *unit_name;
extern char *optarg;

FILE *devnull;             /* pointer to the file /dev/null                  */

BOOLEAN isBigEndian(void);
AST *addnode(void);
char *strdup(const char *),
       * get_full_classname(char *);
SYMTABLE * new_symtable (int);
int yyparse (void);
extern int getopt(int, char *const *, const char *);
void type_insert (SYMTABLE *, AST *, enum returntype, char *);
void handle_segfault(int);
void insert_entries(char *, Dlist);

/*****************************************************************************
 * main                                                                      *
 *                                                                           *
 * This is the main f2java routine.  Parse the command-line options and      *
 * open the input file.                                                      *
 *                                                                           *
 *****************************************************************************/

int
main (int argc, char **argv)
{
  char classname[130];
  char *truncfilename;
  char sourcename[130];
  char jasminname[130];
  char vcgname[130];
  char *indexname;
  char *f2jpath;
  char *search_path;

  AST *temp;
  int errflg = 0;
  int c;
  int i;

  char f2java_help[] = "The program is used as follows:\n\n\
To compile a program into Java source code:\n\
    f2java filename\n\n\
The -c option may also be used to specify the search\n\
path for \".f2j\" files.  For example:\n\n\
    f2java -c .:../objects filename\n\n\
The -p option may also be used to specify the name\n\
of the package.  For example:\n\n\
    f2java -p org.netlib.blas filename\n\n\
The -o option specifies the destination directory\n\
to which the code should be written.\n\n\
The -w option forces all scalars to be generated as\n\
wrapped objects.  The default behavior is to only\n\
wrap those scalars that must be passed by reference.\n\n\
The -i option causes f2j to generate a high-level\n\
interface to each subroutine and function.\n\n\
The -h option displays this helpful information.\n\n\
The -s option causes f2j to simplify the interfaces\n\
by removing the offset parameter and using a zero offset.\n\
It isn't necessary to specify the -i flag in addition\n\
to the -s.\n\n\
The -d options causes f2j to generate comments in\n\
a format suitable for javadoc.  It is a bit of a LAPACK-\n\
specfic hack...the longest comment in the program unit\n\
is placed in the javadoc comment.  It works fine for\n\
BLAS/LAPACK code (or any other code where the longest\n\
comment is the one that describes the function), but\n\
will most likely not work for other code.\n";

  signal(SIGSEGV,handle_segfault);

  omitWrappers  = TRUE;
  genInterfaces = FALSE;
  genJavadoc    = FALSE;
  noOffset      = FALSE;
  package_name  = NULL;
  bigEndian     = isBigEndian();
  output_dir    = NULL; 
  search_path   = NULL; 

  ignored_formatting = 0;
  bad_format_count = 0;

  while((c = getopt(argc,argv,"c:p:wisdho:")) != EOF)
    switch(c) {
      case 'c':
        search_path = optarg;
        break;
      case 'p':
        package_name = optarg;
        break;
      case 'w':
        omitWrappers = FALSE;
        break;
      case 'h':
        printf("%s",f2java_help);
        exit(1);
        break;
      case 'i':
        genInterfaces = TRUE;
        break;
      case 'd':
        genJavadoc = TRUE;
        break;
      case 's':
        noOffset = TRUE;
        break;
      case 'o':
        output_dir = optarg;
        break;
      case '?':
        errflg++;
        break;
      default:
        printf("hit default case\n");
        break;
    }

  if(errflg || (argc < 2))
  {
    fprintf(stderr, "Usage: f2java [-c search path] [-p package name]");
    fprintf(stderr, " [-o output dir]");
    fprintf(stderr, " [-w] [-i] [-s] [-d] <filename>\n");
    fprintf(stderr,
     "For help: f2java -h\n");
    exit(2);
  }

  if(noOffset)
    genInterfaces = TRUE;

  inputfilename = argv[argc - 1];

  if((ifp = fopen (inputfilename, "r"))==NULL) {
    fprintf(stderr,"Input file not found: '%s'\n",inputfilename);
    exit(1);
  }

  truncfilename = strdup(inputfilename);
  truncfilename = strtok (truncfilename, ".");
  *truncfilename = toupper (*truncfilename);

  /* Loathsome hacks... */
  strcpy (classname, truncfilename);
  strcpy (sourcename, truncfilename);
  strcpy (jasminname, truncfilename);
  strcpy (vcgname, truncfilename);

  strcat (sourcename, ".java");
  strcat (jasminname, ".j");
  strcat (vcgname, ".vcg");

  initialize ();

#if VCG
  if((vcgfp = fopen(vcgname, "w"))==NULL) {
    fprintf(stderr,"Cannot open output file '%s'.\n",sourcename);
    perror("Reason");
    exit(1);
  }
#endif

  indexname = (char *)f2jalloc(strlen(truncfilename) + 5);

  strcpy(indexname, truncfilename);
  strcat(indexname, ".f2j");

  if((indexfp = fopen_fullpath(indexname,"w")) == NULL) {
    fprintf(stderr,"Error opening index file: '%s'\n", indexname);
    exit(-1);
  }

  /* the Java keywords are stored in a list of strings.  Store them 
   * all in a hash table for quick lookup. */

  java_keyword_table  = (SYMTABLE *) new_symtable (211);
  temp = addnode();
 
  for(i=0;java_reserved_words[i] != NULL; i++)
    type_insert(java_keyword_table,temp,0,java_reserved_words[i]);

  jasmin_keyword_table = (SYMTABLE *) new_symtable(211);
  temp = addnode();

  for(i=0;jasmin_reserved_words[i] != NULL; i++)
    type_insert(jasmin_keyword_table,temp,0,jasmin_reserved_words[i]);

  blas_routine_table = (SYMTABLE *) new_symtable(211);
  temp = addnode();

  for(i=0;blas_routines[i] != NULL; i++)
    type_insert(blas_routine_table,temp,0,blas_routines[i]);

  generic_table = (SYMTABLE *) new_symtable(211);
  temp = addnode();

  for(i=0;generic_intrinsics[i] != NULL; i++)
    type_insert(generic_table,temp,0,generic_intrinsics[i]);

  /* if search path was not specified on command line, then
   * check for environment variable.
   */
  if(search_path == NULL) {
    f2jpath = getenv(F2J_PATH_VAR);

    if(f2jpath == NULL) {
      /* can't use strtok on constant strings, so create a new one here */
      f2jpath = strdup(".");
    }
  }
  else
    f2jpath = search_path;

  descriptor_table = build_method_table(f2jpath);

  devnull = fopen("/dev/null","w");

  if(devnull == NULL) {
    fprintf(stderr,"Cannot open /dev/null for writing\n");
    exit(-1);
  }

  fprintf(stderr,"%s:\n",inputfilename);
  yyparse ();

#if VCG
  fclose (vcgfp);
#endif

  if(bad_format_count > 0)
    fprintf(stderr,"Bad formatting (%d statements)\n", bad_format_count);

  if(ignored_formatting > 0)
    fprintf(stderr,"Ignored %d format statement(s) with implied loops\n", 
       ignored_formatting);

  exit (0);
}

/*****************************************************************************
 *                                                                           *
 * jasminheader                                                              *
 *                                                                           *
 * This stuff goes at the top of every jasmin file.                          *
 *                                                                           *
 *****************************************************************************/

void
jasminheader (FILE * fp, char *classname)
{
  fprintf(fp,";  Produced by f2jas.  f2jas is part of the Fortran-\n");
  fprintf(fp,";  -to-Java project at the University of Tennessee Netlib\n");
  fprintf(fp,";  numerical software repository.\n");
  fprintf(fp,";  David M. Doolin, doolin@cs.utk.edu\n\n");
  fprintf(fp,"\n; Conventions:\n");
  fprintf(fp,";\t1. S_label<n> refers to a label n from the Fortran source.\n");
  fprintf(fp,";\t2. Variable names, constants and operators from the\n");
  fprintf(fp,";\t   Fortran source are listed when possible as comments ");
  fprintf(fp,"to each instruction.\n");
  fprintf(fp,";\t3. Jasmin opcodes are indent 3 spaces.\n");
  fprintf(fp,";\t4. Jasmin directives start with a `.' and aren't indented.\n");
  fprintf(fp,"\n");
  fprintf(fp,".class public %s\n", classname);
  fprintf(fp,".super java/lang/Object\n\n");
  fprintf(fp,"; The instance initialization method.\n");
  fprintf(fp,".method public <init>()V\n");
  fprintf(fp,"   ;  Just call the initializer for Object.\n");
  fprintf(fp,"   aload_0\n");
  fprintf(fp,"   invokespecial java/lang/Object/<init>()V\n");
  fprintf(fp,"   return\n");
  fprintf(fp,".end method\n");
}				/* Close jasminheader(). */

/*****************************************************************************
 *                                                                           *
 * javaheader                                                                *
 *                                                                           *
 *  The header for the Java source will depend on whether the                *
 * BLAS or LAPACK routines are being compiled.  The way this                 *
 * works is to have the CLASSPATH point at the directory that                *
 * contains directories that contain the actual classes.                     * 
 * The preprocessor junk is a necessary evil, at least temporarily.          *
 *                                                                           *
 *****************************************************************************/

void
javaheader (FILE * fp, char *reflect)
{
  fprintf(fp,"/*\n");
  fprintf(fp," *  Produced by f2java.  f2java is part of the Fortran-\n");
  fprintf(fp," *  -to-Java project at the University of Tennessee Netlib\n");
  fprintf(fp," *  numerical software repository.\n *\n");
  fprintf(fp," *  Original authorship for the BLAS and LAPACK numerical\n");
  fprintf(fp," *  routines may be found in the Fortran source, available at\n");
  fprintf(fp," *  www.netlib.org.\n *\n");
  fprintf(fp," *  Fortran input file: %s\n *\n", inputfilename);
  fprintf(fp," *  The f2j compiler code was written by\n");
  fprintf(fp," *  David M. Doolin (doolin@cs.utk.edu) and\n");
  fprintf(fp," *  Keith  Seymour (seymour@cs.utk.edu)\n");
  fprintf(fp," */\n\n");

  if(package_name != NULL)
    fprintf(fp,"package %s;\n",package_name);

  fprintf(fp,"import java.lang.*;\n");
  fprintf(fp,"import org.netlib.util.*;\n\n");
  fprintf(fp,"%s", reflect);   /* the import stmt for reflection capability */

  fprintf(fp,"\n\n");
}

/*****************************************************************************
 * initialize                                                                *
 *                                                                           *
 * Take care of some other crap that cannot be handled in the                *
 * parser.  Basically, I should initialize ALL of the symbol                 *
 * tables in here, then access all of them as externs.  As a                 *
 * matter of fact, I should put all initializations into their               *
 * own file.                                                                 *
 *                                                                           *
 *****************************************************************************/

void
initialize ()
{
  int tablesize = 211;

  lineno = 0;
  statementno = 0;
  func_stmt_num = 0;

  array_table          = (SYMTABLE *) new_symtable (tablesize);
  format_table         = (SYMTABLE *) new_symtable (tablesize);
  data_table           = (SYMTABLE *) new_symtable (tablesize);
  save_table           = (SYMTABLE *) new_symtable (tablesize);
  common_table         = (SYMTABLE *) new_symtable (tablesize);
  common_block_table   = (SYMTABLE *) new_symtable (tablesize);
  function_table       = (SYMTABLE *) new_symtable (tablesize);
  parameter_table      = (SYMTABLE *) new_symtable (tablesize);
  global_func_table    = (SYMTABLE *) new_symtable (tablesize);
  global_common_table  = (SYMTABLE *) new_symtable (tablesize);
}


/*****************************************************************************
 * uppercase                                                                 *
 *                                                                           *
 * This should be located in some other file                                 *
 * than main().  Procedure simply uppercases                                 *
 * every character in a string.                                              *
 *                                                                           *
 *****************************************************************************/

void 
uppercase(char * name)
{
  while (*name)
  {
    *name = toupper(*name);
    name++;
  }
} 

/*****************************************************************************
 *                                                                           *
 * handle_segfault                                                           *
 *                                                                           *
 * This function is called whenever the program seg faults.  We flush        *
 * stdout so that we can get a better idea of where the program was when it  *
 * crashed.                                                                  *
 *                                                                           *
 *****************************************************************************/

void handle_segfault(int x)
{
  fflush(stdout);
  fprintf(stderr,"Segmentation Fault, stdout flushed.\n");
  if(unit_name != NULL)
    fprintf(stderr,"unit name is %s\n",unit_name);
  fflush(stderr);
  exit(1);
}

/*****************************************************************************
 *                                                                           *
 * isBigEndian                                                               *
 *                                                                           *
 * This function determines the endianness of the machine we're running on.  *
 * Such information is used during bytecode generation since the numerical   *
 * constants are always stored in big endian format.                         *
 *                                                                           *
 * returns TRUE if this machine is big endian, FALSE otherwise.              *
 *                                                                           *
 *****************************************************************************/

BOOLEAN
isBigEndian()
{
  int x = 1;

  if (*((char *)&x)== 1)
    return FALSE;
  else
    return TRUE;
}

/*****************************************************************************
 *                                                                           *
 * f2jfree                                                                   *
 *                                                                           *
 * Wrapper around free which may overwrite the memory such that we can find  *
 * problems early (only if DEBUG_MEM is defined).                            *
 *                                                                           *
 *****************************************************************************/

void
f2jfree(void *p)
{
#ifdef DEBUG_MEM
  /* do something here eventually */
#endif

  free(p);
}

/*****************************************************************************
 *                                                                           *
 * f2jalloc                                                                  *
 *                                                                           *
 * Error-checking memory allocation routine for f2java.  we can't recover    *
 * from an out of memory condition, so we'll just call exit() which will     *
 * close all open streams for us.                                            *
 *                                                                           *
 *****************************************************************************/

void *
f2jalloc(size_t numbytes)
{
  void * mem = malloc(numbytes);

  if(mem == NULL)
    alloc_error(numbytes);

  return mem;
}

/*****************************************************************************
 *                                                                           *
 * f2jcalloc                                                                 *
 *                                                                           *
 * Error-checking memory allocation routine for f2java.  we can't recover    *
 * from an out of memory condition, so we'll just call exit() which will     *
 * close all open streams for us.                                            *
 *                                                                           *
 *****************************************************************************/

void *
f2jcalloc(size_t numitems, size_t numbytes)
{
  void * mem = calloc(numitems, numbytes);

  if(mem == NULL)
    alloc_error(numbytes);

  return mem;
}

/*****************************************************************************
 *                                                                           *
 * f2jrealloc                                                                *
 *                                                                           *
 * Error-checking memory allocation routine for f2java.  we can't recover    *
 * from an out of memory condition, so we'll just call exit() which will     *
 * close all open streams for us.                                            *
 *                                                                           *
 *****************************************************************************/

void *
f2jrealloc(void *ptr, size_t size)
{
  void *mem = realloc(ptr, size);

  if(mem == NULL)
    alloc_error(size);

  return mem;
}

/*****************************************************************************
 *                                                                           *
 * alloc_error                                                               *
 *                                                                           *
 * called when there is an error allocating memory.  this function prints    *
 * an error message and exits.                                               *
 *                                                                           *
 *****************************************************************************/

void
alloc_error(size_t size)
{
  fprintf(stderr,"f2java: Error allocating %d bytes of memory.  Stopping.\n",
     (int)size);
  perror("Reason:");
  exit(1);
}

/*****************************************************************************
 *                                                                           *
 * build_method_table                                                        *
 *                                                                           *
 * this function searches through all the .f2j files found in directories    *
 * specified in the user's F2J_SEARCH_PATH environment variable and builds   *
 * a list of the method descriptors.                                         *
 *                                                                           *
 *****************************************************************************/

Dlist
build_method_table(char *path)
{
  char *token;
  struct dirent *dir_entry;
  DIR *cur_dir;
  int len;
  int size = 5;
  char * full_path;
  Dlist paths, tmp, new_table;

  new_table = make_dl();

  full_path = (char *)malloc(size);

  token = strtok(path, ":");

  if(token == NULL)
    return NULL;

  paths = make_dl();

  /* gotta build a list of tokens in the F2J_SEARCH_PATH
   * because nested calls to strtok() don't work.
   */
  do {
    dl_insert_b(paths, token);
  } while( (token = strtok(NULL, ":")) != NULL);

  dl_traverse(tmp, paths) {
    token = (char *) tmp->val;

    if((cur_dir = opendir(token)) == NULL)
      continue;

    while((dir_entry = readdir(cur_dir))) {
      len = strlen(dir_entry->d_name);
      if((len > 4) && !strncmp(dir_entry->d_name+(len-4), ".f2j", 4)) {

        if((len + strlen(token) +2) > size) {
          size = (len + strlen(token)) * 2;  /* double for good measure */
          full_path = realloc(full_path, size);
        }

        strcpy(full_path, token);
        if(full_path[strlen(full_path)-1] != '/')
          strcat(full_path, "/");
        strcat(full_path, dir_entry->d_name);
        insert_entries(full_path, new_table);
      }
    }
  }

  return new_table;
}

/*****************************************************************************
 *                                                                           *
 * find_method                                                               *
 *                                                                           *
 * this function searches the given Dlist for a method reference matching    *
 * the given method name.  the first matching entry is returned.             *
 *                                                                           *
 *****************************************************************************/

METHODREF *
find_method(char *meth, Dlist methtab)
{
  Dlist tmp;
  METHODREF * entry;

  dl_traverse(tmp, methtab) {
    entry = (METHODREF *) tmp->val;

    if( !strcmp(entry->methodname, meth) )
      return entry;
  }

  return NULL;
}

/*****************************************************************************
 *                                                                           *
 * insert_entries                                                            *
 *                                                                           *
 * given the filename, insert all method/descriptor entries from that file   *
 * into the specified Dlist.                                                 *
 *                                                                           *
 *****************************************************************************/
#define BUFSZ 400

void
insert_entries(char *path, Dlist methtab)
{
  char * class, * method, * desc;
  char buf[BUFSZ];
  FILE *in;
  
  if((in = fopen(path, "r")) == NULL)
    return;

  while(fgets(buf, BUFSZ, in) != NULL) {
    buf[strlen(buf)-1] = '\0';
    class  = strtok(buf,":");
    method = strtok(NULL,":");
    desc   = strtok(NULL,":");

    if(!class || !method || !desc)
      continue;

    dl_insert_b(methtab, newMethodNode(class,method,desc));
  }

  return;
}

/*****************************************************************************
 *                                                                           *
 * strAppend                                                                 *
 *                                                                           *
 * Append the given string value (new) to the expandable string (str),       *
 * allocating more memory if necessary.                                      *
 *                                                                           *
 *****************************************************************************/

struct _str *
strAppend(struct _str *str, char *new)
{
  if(str == NULL) {
    str = (struct _str *)f2jalloc(sizeof (struct _str));
    str->size = STR_INIT;
    str->val = (char *)f2jalloc(STR_INIT);
    str->val[0] = '\0';
  }

  if(strlen(new) + strlen(str->val) >= str->size) {
    if(strlen(new) > STR_CHUNK) {
      str->val = (char *)f2jrealloc(str->val, str->size + strlen(new));
      str->size += strlen(new);
    }
    else {
      str->val = (char *)f2jrealloc(str->val, str->size + STR_CHUNK);
      str->size += STR_CHUNK;
    }
  }

  strcat(str->val, new);

  return str;
}
