#include<stdio.h>
#include<stdarg.h>
#include<ctype.h>
#include<string.h>
#include<time.h>
#include"f2j.h"
#include"f2jparse.tab.h"

extern yydebug;
char *inputfilename;		/* Hack for getting input file to write
				   output header.  */
main (int argc, char **argv)
{


    /* I loath all these character arrays.  There has to be
       a better way to do this using char *.  */
    char classname[130];
    extern char *inputfilename;
    extern FILE *ifp;
    extern FILE *jasminfp;
    extern FILE *javafp;
    extern int lineno;
    extern int statementno;

    char sourcename[130];
    char jasminname[130];

    yydebug = DEBUGGEM;

    assert(argc == 2);

    inputfilename = strdup (argv[1]);

    ifp = fopen (argv[1], "r");

    argv[1] = strtok (argv[1], ".");
    *argv[1] = toupper (*argv[1]);
    /* Loathsome hacks... */
    strcpy (classname, argv[1]);
    strcpy (sourcename, argv[1]);
    strcpy (jasminname, argv[1]);
    strcat (sourcename, ".java");
    strcat (jasminname, ".j");

    initialize ();

#if JAS
    jasminfp = fopen (jasminname, "w");
    jasminheader (jasminfp, classname);
#endif
#if JAVA
    javafp = fopen (sourcename, "w");
    javaheader (javafp, classname);
    /* Write to standard out to debug code
       generation.  */
    /*  javafp = stdout;     */
#endif

    yyparse ();

#if JAS
    fclose (jasminfp);
#endif
#if JAVA
    fclose (javafp);
#endif
    printf("Line number: %d\n", lineno);
    printf("Statement number: %d\n", statementno);
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
javaheader (FILE * javafp, char *classname)
{
    fprintf (javafp, "/*\n");
    fprintf (javafp, " *  Produced by f2java.  f2java is part of the Fortran-\n");
    fprintf (javafp, " *  -to-Java project at the University of Tennessee Netlib\n");
    fprintf (javafp, " *  numerical software repository.\n *\n");
    fprintf (javafp, " *  Original authorship for the BLAS and LAPACK numerical\n");
    fprintf (javafp, " *  routines may be found in the Fortran source, available at\n");
    fprintf (javafp, " *  www.netlib.org.\n *\n");
    fprintf (javafp, " *  Fortran input file: %s\n *\n", inputfilename);
    /*  The time functions provided by the GNU libc do not work under sun4.
	It would however be nice to have the translations time-stamped. */
    /* fprintf (javafp, " *  translated: %s\n", time); */
    fprintf (javafp, " *  The f2j compiler code was written by\n");
    fprintf (javafp, " *  David M. Doolin (doolin@cs.utk.edu) and\n");
    fprintf (javafp, " *  Keith  Seymour (seymour@cs.utk.edu)\n");
    fprintf (javafp, " */\n\n");
#if LAPACK
    fprintf (javafp, "package lapack;\n");
    fprintf (javafp, "import blas.*;\n");
#endif
#if BLAS
    fprintf (javafp, "package blas;\n");
#endif
    fprintf (javafp, "import java.lang.*;\n\n");
    fprintf (javafp, "public class %s {\n\n", classname);
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
  extern int statmentno;
    int tablesize = 211;
    extern SYMTABLE *array_table;	/* Variables of type array. */

lineno = 0;
statementno = 0;
    array_table = (SYMTABLE *) new_symtable (tablesize);

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
