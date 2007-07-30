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
#include"y.tab.h"
#include"dlist.h"
#include"f2jmem.h"
#include"f2j_externs.h"

extern char *java_reserved_words[];
extern char *blas_routines[];
extern char *generic_intrinsics[];
extern char *unit_name;
extern char *optarg;
extern Dlist file_stack;
extern Dlist include_paths;

#ifdef _WIN32
  char null_file[] = "f2j.tmp";
#else
  char null_file[] = "/dev/null";
#endif

FILE *devnull;             /* pointer to the file /dev/null                  */

AST
  *addnode(void);

char
  *f2j_fgets(char *, int, FILE *);

SYMTABLE
  *new_symtable (int);

int
  yyparse (void);

void
  type_insert (SYMTABLE *, AST *, enum returntype, char *),
  handle_segfault(int),
  insert_entries(char *, Dlist);

extern int
  getopt(int, char *const *, const char *);

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
  char vcgname[130];
  char *indexname;
  char *f2jpath;
  char *search_path;

  AST *temp;
  int errflg = 0;
  int c;
  int i;

  /* split the help string into multiple sections to comply
   * with some iso standard on string lengths...
   */
  char f2java_help[] = "The program is used as follows:\n\n\
To compile a program into Java source code:\n\
    f2java filename\n\n";

  char f2java_help_I_option[] = "The -I option specifies\
 a path to be searched for\nincluded files (may be used\
 multiple times).\n\n";

#ifdef _WIN32
  char f2java_help_c_option[] = "The -c option may also be\
 used to specify the search\n\
path for \".f2j\" files.  It is a semicolon-separated\n\
list of paths, like a Java CLASSPATH).  For example:\n\n\
    f2java -c .;..\\objects filename\n\n";
#else
  char f2java_help_c_option[] = "The -c option may also be\
 used to specify the search\n\
path for \".f2j\" files.  It is a colon-separated\n\
list of paths, like a Java CLASSPATH).  For example:\n\n\
    f2java -c .:../objects filename\n\n";
#endif

  char f2java_help_p_option[] = "The -p option may also be\
 used to specify the name\n\
of the package.  For example:\n\n\
    f2java -p org.netlib.blas filename\n\n";

  char f2java_help_o_option[] = "The -o option specifies\
 the destination directory\n\
to which the code should be written.\n\n";

  char f2java_help_w_option[] = "The -w option forces all\
 scalars to be generated as\n\
wrapped objects.  The default behavior is to only\n\
wrap those scalars that must be passed by reference.\n\n";

  char f2java_help_i_option[] = "The -i option causes f2j\
 to generate a high-level\n\
interface to each subroutine and function.\n\n";

  char f2java_help_h_option[] = "The -h option displays\
 this helpful information.\n\n";

  char f2java_help_s_option[] = "The -s option causes f2j\
 to simplify the interfaces\n\
by removing the offset parameter and using a zero offset.\n\
It isn't necessary to specify the -i flag in addition\n\
to the -s.\n\n";

  char f2java_help_d_option[] = "The -d options causes f2j\
 to generate comments in\n\
a format suitable for javadoc.  It is a bit of a LAPACK-\n\
specfic hack...the longest comment in the program unit\n\
is placed in the javadoc comment.  It works fine for\n\
BLAS/LAPACK code (or any other code where the longest\n\
comment is the one that describes the function), but\n\
will most likely not work for other code.\n\n";

  char f2java_help_fm_option[] = "The -fm option causes f2j\
 to generate code that calls\njava.lang.StrictMath\
 instead of java.lang.Math.  By\ndefault, java.lang.Math is used.\n\n";

  char f2java_help_fs_option[] = "The -fs option causes f2j\
 to declare the generated\ncode as strictfp (strict floating point).\
  By default,\nthe generated code is not strict.\n\n";

  char f2java_help_fb_option[] = "The -fb option enables\
 both the -fm and -fs options.\n\n";

  char f2java_help_vs_option[] = "The -vs option causes f2j\
 to generate all variables\nas static class variables.\
  By default f2j generates\nvariables as locals.\n\n";

  char f2java_help_va_option[] = "The -va option causes f2j\
 to generate arrays\nas static class variables,\
  but other\nvariables are generated as locals.\n\n";

  signal(SIGSEGV,handle_segfault);

  omitWrappers      = TRUE;
  strictMath        = FALSE;
  strictFp          = FALSE;
  genInterfaces     = FALSE;
  genJavadoc        = FALSE;
  noOffset          = FALSE;
  package_name      = NULL;
  output_dir        = NULL; 
  search_path       = NULL; 
  save_all_override = FALSE;
  f2j_arrays_static = FALSE;

  file_stack = make_dl();
  include_paths = make_dl();
  dl_insert_b(include_paths, ".");

  ignored_formatting = 0;
  bad_format_count = 0;

  while((c = getopt(argc,argv,"I:c:p:wif:sdho:v:")) != EOF)
    switch(c) {
      case 'I':
        dl_insert_b(include_paths, optarg);
        break;
      case 'c':
        search_path = optarg;
        break;
      case 'p':
        package_name = optarg;
        break;
      case 'f':
        if(!strcmp("b", optarg))
          strictMath = strictFp = TRUE;
        else if(!strcmp("m", optarg))
          strictMath = TRUE;
        else if(!strcmp("s", optarg))
          strictFp = TRUE;
        break;
      case 'w':
        omitWrappers = FALSE;
        break;
      case 'h':
        printf("%s",f2java_help);
        printf("%s",f2java_help_I_option);
        printf("%s",f2java_help_c_option);
        printf("%s",f2java_help_p_option);
        printf("%s",f2java_help_o_option);
        printf("%s",f2java_help_w_option);
        printf("%s",f2java_help_i_option);
        printf("%s",f2java_help_h_option);
        printf("%s",f2java_help_s_option);
        printf("%s",f2java_help_d_option);
        printf("%s",f2java_help_fm_option);
        printf("%s",f2java_help_fs_option);
        printf("%s",f2java_help_fb_option);
        printf("%s",f2java_help_vs_option);
        printf("%s",f2java_help_va_option);
        exit(EXIT_SUCCESS);
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
      case 'v':
        if(!strcmp("s", optarg))
          save_all_override = TRUE;
        else if(!strcmp("a", optarg))
          f2j_arrays_static = TRUE;
        else {
          fprintf(stderr,"-v%s: bad argument\n",optarg);
          errflg++;
        }

        break;
      case 'o':
        output_dir = optarg;
        break;
      case '?':
        errflg++;
        break;
      default:
        printf("Bad arg.\n");
        break;
    }

  if(errflg || (argc < 2))
  {
    fprintf(stderr, "Usage: f2java [-I include path] [-c search path]");
    fprintf(stderr, "  [-p package name] [-o output dir]");
    fprintf(stderr, " [-w] [-i] [-s] [-d] [-vs] [-va] [-fs] [-fm] [-fb] <filename>\n");
    fprintf(stderr, "For help: f2java -h\n");
    exit(EXIT_FAILURE);
  }

  if(noOffset)
    genInterfaces = TRUE;

  inputfilename = argv[argc - 1];

  if((ifp = fopen (inputfilename, "rb"))==NULL) {
    fprintf(stderr,"Input file not found: '%s'\n",inputfilename);
    exit(EXIT_FAILURE);
  }

  current_file_info = (INCLUDED_FILE *)f2jalloc(sizeof(INCLUDED_FILE));
  current_file_info->name = strdup(inputfilename);
  current_file_info->line_num = 0;
  current_file_info->fp = ifp;

  truncfilename = strdup(inputfilename);
  truncfilename = strtok (truncfilename, ".");
  *truncfilename = toupper (*truncfilename);

  /* Loathsome hacks... */
  strcpy (classname, truncfilename);
  strcpy (sourcename, truncfilename);
  strcpy (vcgname, truncfilename);

  strcat (sourcename, ".java");
  strcat (vcgname, ".vcg");

  initialize ();

#if VCG
  if((vcgfp = fopen(vcgname, "w"))==NULL) {
    fprintf(stderr,"Cannot open output file '%s'.\n",sourcename);
    perror("Reason");
    exit(EXIT_FAILURE);
  }
#endif

  indexname = (char *)f2jalloc(strlen(truncfilename) + 5);

  strcpy(indexname, truncfilename);
  strcat(indexname, ".f2j");

  if((indexfp = bc_fopen_fullpath(indexname,"w", output_dir)) == NULL) {
    fprintf(stderr,"Error opening index file: '%s'\n", indexname);
    exit(EXIT_FAILURE);
  }

  /* the Java keywords are stored in a list of strings.  Store them 
   * all in a hash table for quick lookup. */

  java_keyword_table  = (SYMTABLE *) new_symtable (211);
  temp = addnode();
 
  for(i=0;java_reserved_words[i] != NULL; i++)
    type_insert(java_keyword_table,temp,0,java_reserved_words[i]);

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

  devnull = fopen(null_file,"w");

  if(devnull == NULL) {
    fprintf(stderr,"Cannot open %s for writing\n", null_file);
    exit(EXIT_FAILURE);
  }

  fprintf(stderr,"%s:\n",inputfilename);

  if(yyparse() != 0) {
    fprintf(stderr, "Parsing failed.\n");
    exit(EXIT_FAILURE);
  }

  fclose(ifp);

#if VCG
  fclose(vcgfp);
#endif

  if(bad_format_count > 0)
    fprintf(stderr,"Unsupported formatting (%d statements)\n", bad_format_count);

  if(ignored_formatting > 0)
    fprintf(stderr,"Ignored %d format statement(s) with implied loops\n", 
       ignored_formatting);

  if(fclose(indexfp) < 0) {
    fprintf(stderr,"error closing indexfp...\n");
    perror("reason");
  }

  if(fclose(devnull) < 0) {
    fprintf(stderr,"error closing devnull...\n");
    perror("reason");
  }

#ifdef _WIN32
  /* for windows, we should delete the temp file created earlier. */
  if(remove(null_file) < 0) {
    fprintf(stderr,"couldn't remove temp file...\n");
    perror("reason");
  }
#endif

  exit(EXIT_SUCCESS);
}

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
  fprintf(fp," *  http://www.netlib.org.\n *\n");
  fprintf(fp," *  Fortran input file: %s\n *\n", inputfilename);
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

/*
 *  array_table          = (SYMTABLE *) new_symtable (tablesize);
 *  format_table         = (SYMTABLE *) new_symtable (tablesize);
 *  data_table           = (SYMTABLE *) new_symtable (tablesize);
 *  save_table           = (SYMTABLE *) new_symtable (tablesize);
 *  common_table         = (SYMTABLE *) new_symtable (tablesize);
 *  parameter_table      = (SYMTABLE *) new_symtable (tablesize);
 */
  common_block_table   = (SYMTABLE *) new_symtable (tablesize);
  function_table       = (SYMTABLE *) new_symtable (tablesize);
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
  fprintf(stderr,"Segmentation Fault, stdout flushed. [%d]\n", x);
  if(unit_name != NULL)
    fprintf(stderr,"unit name is %s\n",unit_name);
  fflush(stderr);
  exit(EXIT_FAILURE);
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

  full_path = (char *)f2jalloc(size);

  token = strtok(path, PATH_DELIM);

  if(token == NULL)
    return NULL;

  paths = make_dl();

  /* gotta build a list of tokens in the F2J_SEARCH_PATH
   * because nested calls to strtok() don't work.
   */
  do {
    dl_insert_b(paths, token);
  } while( (token = strtok(NULL, PATH_DELIM)) != NULL);

  dl_traverse(tmp, paths) {
    token = (char *) tmp->val;

    if((cur_dir = opendir(token)) == NULL)
      continue;

    while((dir_entry = readdir(cur_dir)) != NULL) {
      len = strlen(dir_entry->d_name);
      if((len > 4) && !strncmp(dir_entry->d_name+(len-4), ".f2j", 4)) {

        if((len + strlen(token) +2) > (unsigned int)size) {
          size = (len + strlen(token)) * 2;  /* double for good measure */
          full_path = f2jrealloc(full_path, size);
        }

        strcpy(full_path, token);
        if(full_path[strlen(full_path)-1] != FILE_DELIM[0])
          strcat(full_path, FILE_DELIM);
        strcat(full_path, dir_entry->d_name);
        insert_entries(full_path, new_table);
      }
    }

    closedir(cur_dir);
  }

  f2jfree(full_path, size);
  dl_delete_list(paths);

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

JVM_METHODREF *
find_method(char *meth, Dlist methtab)
{
  Dlist tmp;
  JVM_METHODREF * entry;

  dl_traverse(tmp, methtab) {
    entry = (JVM_METHODREF *) tmp->val;

    if( !strcmp(entry->methodname, meth) )
      return entry;
  }

  return NULL;
}

/*****************************************************************************
 *                                                                           *
 * get_line                                                                  *
 *                                                                           *
 * Keeps reading chunks from the specified file until a newline is found.    *
 * Appends all the chunks to one string and returns that.                    *
 *                                                                           *
 *****************************************************************************/

char *
get_line(FILE *in)
{
#define BUFSZ 400
  char buf[BUFSZ];
  char *rv, *line, *ltmp;
  int idx = 0, cur_size = BUFSZ;

  if(!in) return NULL;

  line = (char *)malloc(BUFSZ);
  *line = '\0';

  if(!line) return NULL;

  do {
    rv = fgets(buf, BUFSZ, in);

    if(!rv)
      return NULL;

    memcpy(line+idx, buf, BUFSZ);
    idx += strlen(buf);

    cur_size += BUFSZ;
    ltmp = realloc(line, cur_size);

    if(!ltmp) return NULL;
    line = ltmp;
  } while(buf[strlen(buf)-1] != '\n');

  return line;
}

/*****************************************************************************
 *                                                                           *
 * insert_entries                                                            *
 *                                                                           *
 * given the filename, insert all method/descriptor entries from that file   *
 * into the specified Dlist.                                                 *
 *                                                                           *
 *****************************************************************************/

void
insert_entries(char *path, Dlist methtab)
{
  char * class, * method, * desc, * buf;
  FILE *in;
  
  if((in = fopen(path, "rb")) == NULL)
    return;

  while((buf=get_line(in)) != NULL) {
    buf[strlen(buf)-1] = '\0';
    class  = strtok(buf,":");
    method = strtok(NULL,":");
    desc   = strtok(NULL,":");

    if(!class || !method || !desc)
      continue;

    dl_insert_b(methtab, bc_new_method_node(class,method,desc));
  }

  fclose(in);

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
