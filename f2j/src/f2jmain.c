#include<stdio.h>
#include<stdarg.h>
#include<ctype.h>
#include<string.h>
#include<time.h>
#include<signal.h>
#include"f2j.h"
#include"f2jparse.tab.h"

/* extern yydebug; */

void
main (int argc, char **argv)
{
    /* I loath all these character arrays.  There has to be
       a better way to do this using char *.  */
    char classname[130];
    extern char *inputfilename;
    extern char *package_name;
    extern char *java_reserved_words[];
    extern char *jasmin_reserved_words[];
    extern char *blas_routines[];
    extern FILE *ifp;
    extern FILE *jasminfp;
    /*    extern FILE *javafp;  9-11-97, Keith*/

    AST *addnode();
    extern FILE *vcgfp;
    extern int lineno;
    extern int statementno;
    int i, index;
    AST *temp;

    char *truncfilename;
    char sourcename[130];
    char jasminname[130];
    char vcgname[130];

    char *strdup(const char *);
    SYMTABLE * new_symtable (int);
    int hash (char *);
    void type_insert (HASHNODE **, AST *, int, char *);
    int yyparse (void);
    extern int getopt(int, char *const *, const char *);

    extern char *optarg;
    /* extern int optind; */
    int errflg = 0;
    int c;

    void handle_segfault(int);

    signal(SIGSEGV,handle_segfault);

    /* yydebug = DEBUGGEM; */

    /* 
     * The program is used as follows:
     *
     * To compile a program into Java source code:
     *     f2java -java filename
     *
     * To compile a program into Jasmin assembly code:
     *     f2java -jas filename
     *
     * If no language is specified (e.g. "f2java filename"),
     * the default behavior is to generate Java source code.
     *
     * The -p option may also be used to specify the name
     * of the package.  For example:
     *     f2java -java -p org.netlib.blas filename
     *
     * The -w option forces all scalars to be generated as
     * wrapped objects.  The default behavior is to only
     * wrap those scalars that must be passed by reference.
     *
     * The -i option causes f2j to generate a high-level
     * interface to each subroutine and function.
     *
     * The -s option causes f2j to simplify the interfaces
     * by removing the offset parameter and using a zero offset.
     * It isn't necessary to specify the -i flag in addition
     * to the -s.
     */

    omitWrappers  = TRUE;
    genInterfaces = FALSE;
    noOffset      = FALSE;
    package_name  = NULL;
    JAS = 0;   /* default to Java output */

    ignored_formatting = 0;
    bad_format_count = 0;

    while((c = getopt(argc,argv,"j:p:wis")) != EOF)
      switch(c) {
        case 'j':
          if(!strcmp(optarg,"ava"))
            JAS = 0;
          else if(!strcmp(optarg,"as"))
            JAS = 1;
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
        case 'i':
          genInterfaces = TRUE;
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
        "Usage: f2java [-java/-jas] [-p package name] [-w] [-i] [-s] <filename>\n");
      exit(2);
    }

    if(noOffset)
      genInterfaces = TRUE;

    inputfilename = argv[argc - 1];

    printf("Ok... compiling '%s' to %s\n", inputfilename, 
       JAS == 1 ? "JAS" : "JAVA");

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
    } else {
/*  commented out 9-11-97, keith
*
*     if((javafp = fopen (sourcename, "w"))==NULL) {
*       fprintf(stderr,"Cannot open output file '%s'.\n",sourcename);
*       perror("Reason");
*       exit(1);
*     }
*     javaheader (javafp, classname);
*/
      /* Write to standard out to debug code
         generation.  */
      /*  javafp = stdout;     */
    }

#if VCG
    if((vcgfp = fopen(vcgname, "w"))==NULL) {
      fprintf(stderr,"Cannot open output file '%s'.\n",sourcename);
      perror("Reason");
      exit(1);
    }
#endif

    /* the Java keywords are stored in a list of strings.  Store them 
       all in a hash table for quick lookup. */

    java_keyword_table  = (SYMTABLE *) new_symtable (211);
    temp = addnode();
 
    for(i=0;java_reserved_words[i] != NULL; i++) {
      index = hash(java_reserved_words[i]) % java_keyword_table->num_entries;
      type_insert(&(java_keyword_table->entry[index]),temp,0,
        java_reserved_words[i]);
    }

    jasmin_keyword_table = (SYMTABLE *) new_symtable(211);
    temp = addnode();

    for(i=0;jasmin_reserved_words[i] != NULL; i++) {
      index = hash(jasmin_reserved_words[i]) % jasmin_keyword_table->num_entries;
      type_insert(&(jasmin_keyword_table->entry[index]),temp,0,
        jasmin_reserved_words[i]);
    }

    blas_routine_table = (SYMTABLE *) new_symtable(211);
    temp = addnode();

    for(i=0;blas_routines[i] != NULL; i++) {
      index = hash(blas_routines[i]) % blas_routine_table->num_entries;
      type_insert(&(blas_routine_table->entry[index]),temp,0,
        blas_routines[i]);
    }

    fprintf(stderr,"%s:\n",inputfilename);
    yyparse ();

    if(JAS)
      fclose (jasminfp);
    else {

/*  commented out 9-11-97, keith
*     fprintf(javafp,"} // End class.\n");
*     fclose (javafp);
*/

    }

#if VCG
    fclose (vcgfp);
#endif

/*
    printf("Line number: %d\n", lineno);
    printf("Statement number: %d\n", statementno);
*/

    if(bad_format_count > 0)
      fprintf(stderr,"Bad formatting (%d statements)\n", bad_format_count);

    if(ignored_formatting > 0)
      fprintf(stderr,"Ignored %d format statement(s) with implied loops\n", 
         ignored_formatting);

    exit (0);
}



/*  This stuff goes at the top of every jasmin file. */
void
jasminheader (FILE * jasminfp, char *classname)
{

/*  Time doesn't work in Solaris same as linux.  Sucks.  */

/*
   char * time;
   const time_t * TIME;
   const struct tm *bt;
 */
    /*  Let's mark the time when the file was translated.  */
/*
   bt = localtime(TIME);
   time = asctime(bt);
 */

    fprintf (jasminfp, ";  Produced by f2jas.  f2jas is part of the Fortran-\n");
    fprintf (jasminfp, ";  -to-Java project at the University of Tennessee Netlib\n");
    fprintf (jasminfp, ";  numerical software repository.\n");
    /*   fprintf (jasminfp, ";  Fortran input file: %s, translated: %s\n",
       inputfilename, time);  */
    fprintf (jasminfp, ";  David M. Doolin, doolin@cs.utk.edu\n\n");
    fprintf (jasminfp, "\n; Conventions:\n");
    fprintf (jasminfp, ";\t1. S_label<n> refers to a label n from the Fortran source.\n");
    fprintf (jasminfp, ";\t2. Variable names, constants and operators from the Fortran\n");
    fprintf (jasminfp, ";\t   are listed when possible as comments "
	     "to each instruction.\n");
    fprintf(jasminfp, ";\t3. Jasmin opcodes are indent 3 spaces.\n");
    fprintf (jasminfp, ";\t4. Jasmin directives start with a `.' and are not indented.\n");
    fprintf (jasminfp, "\n");
    fprintf (jasminfp, ".class public %s\n", classname);
    fprintf (jasminfp, ".super java/lang/Object\n\n");
    fprintf (jasminfp, "; The instance initialization method.\n");
    fprintf (jasminfp, ".method public <init>()V\n");
    fprintf (jasminfp, "   ;  Just call the initializer for Object.\n");
    fprintf (jasminfp, "   aload_0\n");
    fprintf (jasminfp, "   invokespecial java/lang/Object/<init>()V\n");
    fprintf (jasminfp, "   return\n");
    fprintf (jasminfp, ".end method\n");

}				/* Close jasminheader(). */


/*  The header for the Java source will depend on whether the
   BLAS or LAPACK routines are being compiled.  The way this
   works is to have the CLASSPATH point at the directory that
   contains directories that contain the actual classes. 
   The preprocessor junk is a necessary evil, at least temporarily. */
void
javaheader (FILE * fp, char *classname, char *reflect)
{
    fprintf (fp, "/*\n");
    fprintf (fp, " *  Produced by f2java.  f2java is part of the Fortran-\n");
    fprintf (fp, " *  -to-Java project at the University of Tennessee Netlib\n");
    fprintf (fp, " *  numerical software repository.\n *\n");
    fprintf (fp, " *  Original authorship for the BLAS and LAPACK numerical\n");
    fprintf (fp, " *  routines may be found in the Fortran source, available at\n");
    fprintf (fp, " *  www.netlib.org.\n *\n");
    fprintf (fp, " *  Fortran input file: %s\n *\n", inputfilename);
    /*  The time functions provided by the GNU libc do not work under sun4.
	It would however be nice to have the translations time-stamped. */
    /* fprintf (fp, " *  translated: %s\n", time); */
    fprintf (fp, " *  The f2j compiler code was written by\n");
    fprintf (fp, " *  David M. Doolin (doolin@cs.utk.edu) and\n");
    fprintf (fp, " *  Keith  Seymour (seymour@cs.utk.edu)\n");
    fprintf (fp, " */\n\n");

    if(package_name != NULL)
      fprintf(fp,"package %s;\n",package_name);

    fprintf (fp, "import java.lang.*;\n");
    fprintf (fp, "import org.netlib.util.*;\n\n");
    fprintf (fp, "%s", reflect);   /* the import stmt for reflection capability */

    fprintf (fp, "\n\n");

    fprintf (fp, "public class %s {\n\n", classname);
}

/* Take care of some other crap that cannot be handled in the
   parser.  Basically, I should initialize ALL of the symbol
   tables in here, then access all of them as externs.  As a
   matter of fact, I should put all initializations into their
   own file.  */
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


/*  This should be located in some other file
    than main().  Procedure simply  uppercases
    every character in a string.  */

void 
uppercase(char * name)
{
while (*name)
  {
    *name = toupper(*name);
    name++;
  }
} 

void handle_segfault(int x)
{
  fflush(stdout);
  fprintf(stderr,"Segmentation Fault, stdout flushed.\n");
  fflush(stderr);
  exit(1);
}
