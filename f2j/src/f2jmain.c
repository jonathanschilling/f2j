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

#include<stdio.h>
#include<stdarg.h>
#include<ctype.h>
#include<string.h>
#include<time.h>
#include<signal.h>
#include"f2j.h"
#include"f2jparse.tab.h"

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
  extern char *inputfilename;
  extern char *package_name;
  extern char *java_reserved_words[];
  extern char *jasmin_reserved_words[];
  extern char *blas_routines[];
  extern char *optarg;

  extern FILE *ifp;
  extern FILE *jasminfp;

/* 3/23/00 kgs -- removed the following unreferenced variables.
 * extern FILE *vcgfp;
 * extern int lineno;
 * extern int statementno;
 */

  char classname[130];
  char *truncfilename;
  char sourcename[130];
  char jasminname[130];
  char vcgname[130];

  AST *temp;
  int errflg = 0;
  int c;
  int i;

  AST *addnode();
  char *strdup(const char *);
  SYMTABLE * new_symtable (int);
  int yyparse (void);
  extern int getopt(int, char *const *, const char *);
  void type_insert (SYMTABLE *, AST *, int, char *);
  void handle_segfault();

  char f2java_help[] = "The program is used as follows:\n\n\
To compile a program into Java source code:\n\
    f2java -java filename\n\n\
To compile a program into Jasmin assembly code:\n\
    f2java -jas filename\n\n\
If no language is specified (e.g. \"f2java filename\"),\n\
the default behavior is to generate Java source code.\n\n\
The -p option may also be used to specify the name\n\
of the package.  For example:\n\n\
    f2java -java -p org.netlib.blas filename\n\n\
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
  JAS = FALSE;   /* default to Java output */

  ignored_formatting = 0;
  bad_format_count = 0;

  while((c = getopt(argc,argv,"j:p:wisdh")) != EOF)
    switch(c) {
      case 'j':
        if(!strcmp(optarg,"ava"))
          JAS = FALSE;
        else if(!strcmp(optarg,"as"))
          JAS = TRUE;
        else
        {
          fprintf(stderr,"Error: use either \"-java\" or \"-jas\"\n");
          errflg++;
        }
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
      case '?':
        errflg++;
        break;
      default:
        printf("hit default case\n");
        break;
    }

  if(errflg || (argc < 2))
  {
    fprintf(stderr,
     "Usage: f2java [-java/-jas] [-p package name] [-w] [-i] [-s] [-d] <filename>\n");
    fprintf(stderr,
     "For help: f2java -h\n");
    exit(2);
  }

  if(noOffset)
    genInterfaces = TRUE;

  inputfilename = argv[argc - 1];

  printf("Ok... compiling '%s' to %s\n", inputfilename, 
     JAS ? "JAS" : "JAVA");

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

  if(JAS) {
    if((jasminfp = fopen (jasminname, "w"))==NULL) {
      fprintf(stderr,"Cannot open output file '%s'.\n",jasminname);
      perror("Reason");
      exit(1);
    }
    jasminheader (jasminfp, classname);
  }

#if VCG
  if((vcgfp = fopen(vcgname, "w"))==NULL) {
    fprintf(stderr,"Cannot open output file '%s'.\n",sourcename);
    perror("Reason");
    exit(1);
  }
#endif

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

  fprintf(stderr,"%s:\n",inputfilename);
  yyparse ();

  if(JAS)
    fclose (jasminfp);

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
  extern int lineno;
  int tablesize = 211;
  extern SYMTABLE *array_table; /* Variables of type array. */
  SYMTABLE * new_symtable (int);

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

void handle_segfault()
{
  extern char * unit_name;

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

char isBigEndian()
{
  int x = 1;

  if (*((char *)&x)== 1)
    return FALSE;
  else
    return TRUE;
}
