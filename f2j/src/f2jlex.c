/* 
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */



#include<stdio.h>
#include<string.h>
#include"initialize.h"
/* #include"f2jparse.tab.h"  */

/* Had a segfault on a very long continued line
   in a lapack routine.  This is a hack, should
   reallaoc when buffer overflows instead.  */
#define BIGBUFF 1000



/* Some of these variable may not be used anymore.
   gcc -Wall needs to be run to clean up this stuff
   before they turn into confusion factors. */
int eofflag = 1;
char yytext[50];
BOOLEAN typedecs = FALSE;

/* Stuff for Sale's algorithm when I get around to it. 
   They need to be global for now to set contexts.  It is 
   probably possible to rewrite in terms of a struct to 
   pass around in the lexer, the contexts will have to be 
   global for the parser to use.
 */
/*  I am setting these in the `collapse_white_space()
    routine. */

BOOLEAN letterseen;
BOOLEAN equalseen;
BOOLEAN commaseen;

int lexdebug = 0;



typedef struct _buffer
  {
      char stmt[BIGBUFF];
      char text[BIGBUFF];
  }
BUFFER;

int yylex ();
void prelex (BUFFER *);
void collapse_white_space (BUFFER *);
void check_continued_lines (FILE *, char *);




/* STANDALONE is defined in the makefile when compiling the
   lex file as a stand alone program for debugging the lexer.
   */
#ifdef STANDALONE


union yylval_ {
       struct ast_node *ptnode;
       int tok;
       enum returntype type;
       char lexeme[30];
} yylval;

main (int argc, char **argv)
{

    extern FILE *ifp;
    int token = 1;
    ifp = fopen (argv[1], "r");

    while (token != 0)
      {
	  token = yylex ();
	  /* This prints out some random int on the EOF 
	     condition. 
	   */
	  printf ("From main: %d\n", token);
	  printf ("yytext: %s\n\n", yytext);
      }
    if(lexdebug)printf ("EOF\n");
}				/*   Close main().  */

#endif /*    STANDALONE   */

/*  yylex() has to call prelex() to take of all the
   fortran nasties such as initial whitespace, unreserved
   keywords, context sensitivity, etc.  prelex() returns
   a "card image" of characters to be tokenized.  
 */
int
yylex ()
{
    static int tokennumber;
    static int firsttoken;
    static int parencount = 0;
    int token = 0;

    /* yyparse() makes a call to yylex() each time it needs a
       token.  To get a statement to parse, yylex() calls
       prelex() with the statement buffer.  This occurs
       when the value of the statement buffer is 0.
       Since we don't want the statement to change between
       calls, we declare it static, and initialize it to
       null at the start of the program.  We may also need
       the actual text of the fortran input, so we grab
       that also.  */
    static BUFFER buffer = 
    {
	{0},               /* Token string.  */
	{0}                /* Text string.   */
    };

/* Test so that yylex will know when to call prelex to get 
   another character string.  */
    if (*buffer.stmt == 0)
      {
	  prelex (&buffer);   /* No more tokens? Get another statement. */
          tokennumber = 0;    /* Reset for each statement. */
	  parencount = 0;     /* Reset for each statement. */
      }


/* Check for end of file condition.  */
    /*  I am not sure exactly what is going on here...
	I may later comment this out to investigate the
	behavior.  If this does work, it is confusing with
	what I said above.  */
    if (*buffer.stmt == '\0')
	return 0;

/* All the context handling will need to be handled 
   before keyscanning.  Contexts will include `if' 
   and `do' statements.  This is "Sale's algorithm" 
   stuff.  The global vars commaseen, letterseen and 
   equalseen are boolean flags set in the
   `collapse_white_space()' procedure. */
#define SALES 1  
#if SALES
    /*  Fortran statements begin with a keyword except under
	certain very specific circumstances (detailed in
	technical report.  */
    if (tokennumber == 0)
      {
	if (commaseen == FALSE &&
	    equalseen == TRUE &&
	    letterseen == FALSE)
	  {
	    if (isalpha (*buffer.stmt))
	      token = name_scan (&buffer);
            if (token)
              {
	        tokennumber++;
	        return token;
              }
	    /*  Trap errors.  */
	  }

	else /* Other three cases. */
	  {
	    token = keyscan (tab_type, &buffer);
            if (token)
	      {
		printf("Here...\n");
		firsttoken = token;
		tokennumber++;
		printf("Token number: %d,First token %d\n", tokennumber, firsttoken);
                return token;
	      }

	    token = keyscan (tab_stmt, &buffer);
            if (token)
	      {
		firsttoken = token;
		tokennumber++;
	        return token;
	      }

	    /*  Scan for a labeled (numbered) statement. */
            if (isdigit (*buffer.stmt))
	      token = number_scan (&buffer);
              if (token)
		{
		  firsttoken = token;
		  tokennumber++;
		  return token;
		}
	      /*  Should probably trap errors here.  */
	  }
	/*  Should probably trap errors here.  */
      } /* Close if (firsttoken == 0).  */

    if(statementno == 1 && firsttoken == TYPE && tokennumber ==1)
      {
	 token = keyscan (tab_stmt, &buffer);
	 if (token)
	      {
		tokennumber++;
	        return token;
	      }
      }
    
    /* Since we are tracking parentheses, we need to
       scan for miscellaneous tokens.  We are really
       sniffing for parens... */
    token = keyscan (tab_toks, &buffer);
    if (token)
      {
	if (token == OP)
	  parencount++;
	if (token == CP)
	  parencount--;
	tokennumber++;
	return token;
      }
    /* Now check context again. This should be the only other
       place necessary to scan for keywords.  The keywords we
       expect to find are THEN, CONTINUE, and logical if
       statement keywords.  */
    if ((letterseen == TRUE        &&
	(firsttoken == IF      ||
	 firsttoken == ELSEIF)     &&
	 parencount == 0)          ||
	/*  Takes care of labeled (numbered) statements,
	    i.e. 10 CONTINUE.  */
	firsttoken == INTEGER)  
      {
	if (equalseen == TRUE)
	  {
            if (isalpha (*buffer.stmt))
	       token = name_scan (&buffer);
            if (token)
	      {
		tokennumber++;
	        return token;
	      }
	  }
	else  /*  equalseen == FALSE.  */
	  {
	    token = keyscan (tab_stmt, &buffer);
	    /* There should probably be a trap in here to catch
	       bad keywords. */
            if (token)  
	      {
		tokennumber++;
	        return token;
	      }
	  }
	}
    
    
    if (isalpha (*buffer.stmt))
	token = name_scan (&buffer);
    if (token)
      {
	tokennumber++;
	return token;
      }
    if (isdigit (*buffer.stmt))
	token = number_scan (&buffer);
    if (token)
      {
	tokennumber++;
	return token;
      }
    token = string_or_char_scan (&buffer);
    if (token)
      {
	tokennumber++;
	return token;
      }
    
#endif   /* SALES  */

#if NOTSALES	    
    token = keyscan (tab_type, &buffer);
    if (token) 
        return token;
    token = keyscan (tab_toks, &buffer);
    if (token)
	return token;
    token = keyscan (tab_stmt, &buffer);
    if (token)
	return token;
    /* Else... we gotta scan the silly string for NAMES or
       CONSTS. */
    if (isalpha (*buffer.stmt))
	token = name_scan (&buffer);
    if (token)
	return token;
    if (isdigit (*buffer.stmt))
	token = number_scan (&buffer);
    if (token)
	return token;
    token = string_or_char_scan (&buffer);
    if (token)
	return token;
#endif  /* NOTSALES  */


/*  This code below appears to never get called.
   Not sure why not.
 */
    if(lexdebug)printf ("Token (yylex): %d\n");
    return 0;
}				/* Close yylex().  */


/* Ok, here is how it is going to work.  yylex() will
   call prelex() to get a statement that has all of the 
   comments pounded out of it, all the white space
   collapsed, and all of the line labels, contexts,
   continuations, etc., set.  What prelex does NOT check
   is whether there is six spaces of white at the
   beginning of each statement.
 */
void
prelex (BUFFER * bufstruct)
{
    extern FILE *ifp;
    extern int lineno;
    extern int statementno;

    while (fgets (bufstruct->stmt, BIGBUFF, ifp) != NULL)
      {
	  /* Dispose of comments and blank lines for now.
	     Later, a COMMENT token can be defined and the
	     comment returned for inclusion in either
	     source or assembler code. */
	  if (bufstruct->stmt[0] == 'c' ||
	      bufstruct->stmt[0] == 'C' ||
	      bufstruct->stmt[0] == '*' ||
	      bufstruct->stmt[0] == '\n')
	    {
		lineno++;
		if(lexdebug)printf ("First char in buffer: %c\n", 
                                     bufstruct->stmt[0]);
		continue;  /* Found a comment, get another line. */
	    }
	  if(lexdebug)printf ("First char in buffer: %c\n", 
                               bufstruct->stmt[0]);

	  /*  Ok, we have a line that is not a comment and that 
	     does not start and end with a newline, i.e. blank.
	     If the current statement is continued on the 
	     next line, that statement is catenated to the
	     current statement.
	   */
	  check_continued_lines (ifp, bufstruct->stmt);
	  collapse_white_space (bufstruct);
	  if(lexdebug)printf ("From prelex: %s\n", bufstruct->stmt);
	  lineno++;
	  statementno++;
	  return;
      }
    /* EOF conditions. */
    if(lexdebug)printf ("EOF\n");
    bufstruct->stmt[0] = '\0';
    eofflag = -1;
}


/*  Get rid of all of the white space, newlines, etc. in the 
   statement.  Literal strings are handled by keeping the 
   quoting ticks (') and copying the quoted text verbatim
   into the returned string. This routine modifies the
   character array stored in the fields of `bufstruct'.
   This procedure also implements Sale's algorithm to trap
   keywords.
 */
void
collapse_white_space (BUFFER * bufstruct)
{
  /* `cp' is character pointer, `tcp' is temporary cp and
     `yycp' points at the text buffer for some (what?) reason.
     */
    register char *cp, *tcp, *yycp;
    int colno = 1;                /* Column number. Not used? */
    char tempbuf[BIGBUFF];
    int c;                        /* Not used? */
    int parens = 0;
    extern BOOLEAN  commaseen, equalseen, letterseen;
    commaseen = FALSE, equalseen = FALSE, letterseen = FALSE; 

    tcp = tempbuf;
    yycp = bufstruct->text;

    for (cp = bufstruct->stmt; *cp; cp++)
      {
	  /* Get rid of all of the newlines, tabs, whitespace.  */
	  if (*cp == ' ' ||
	      *cp == '\t' ||
	      *cp == '\n')
	      continue;

	  /* If a single front tick is seen, stand by 
	     to copy a literal string, delimited between  
	     two tick marks.  This section in here was left out
	     the string in the prelexed statement.  This was
	     handled with at hack.
	   */
	  if (*cp == '\'')  /* Escape the tick mark with a slash "\" */
	    {
		*tcp = *cp;
		 tcp++;
                 /* Hack... */
		*yycp = *cp;
		*yycp++;
		 cp++;
		 while (*cp != '\'')  /* Literal copy until next tick. */
		  {
		      *tcp = *cp;
		       tcp++;
                       /*  Hack...  All this while loop does is increment
			   without using the toupper function.  The next
			   two lines were left out originally. */
		      *yycp = *cp;
		      *yycp++;
		       cp++;
		  }  /*  End while() for copying strings.  */
	    }  /* End if() for copying character strings. */

	  /* We need to track the number of opening and closing
	     parentheses "(" and ")" to implement Sale's algorithm. */
	  if (*cp == '(') parens++;
	  if (*cp == ')') parens--;

	  /*  Examine the characters outside of matching parentheses.
	      Whats between matching parentheses doesn't matter. */
	  if (parens == 0)
	    {
    	      if (*cp == ',') commaseen = TRUE;
	      if (*cp == '=') equalseen = TRUE;
	      if (*cp == ')')
	        {
	          char * lpp;  /* Last parens pointer, temporary. */
	          /*  Ok, lets peep ahead to the next non-whitespace
		      character and see if its a letter.  The for()
		      loop merely sets the pointer for look-ahead. */
	          for (lpp=cp+1;isspace(*lpp);lpp++);
		  /*  Since we have an opportunity, let's trap the
		      error condition of having isspace() pick up
		      a newline following the last paren.  */
		  /*
		  if (*lpp == '\n')
		    {
		      printf("Bad syntax, \" followed by \"\\n\"\n");
		      exit(-1);
		    }
		  else
		    */
		      if (isalpha(*lpp)) letterseen = TRUE;
	        }  /*  End if for ")".  */
	    }      /*  End if for no parens. */
	  *yycp = *cp;
	   yycp++;
	  *tcp = toupper (*cp);
	   tcp++;
      }  /* End of for() loop. */

     /*  Insert newline for statement separator.  This helps parse
         situations where end and beginning of adjacent statements
         are NAME tokens, i.e. NAME NAME, etc.  Also, newlines are
	 natural fortran statement separators.  */
    *yycp = '\n';
    *tcp = '\n';
    
     /* Insert an null character to mark the end of the string.  */
    *(yycp+1) = 0;
    *(tcp+1) = 0;
     /* Our new string is ready for lexing!  */
     strcpy (bufstruct->stmt, tempbuf);
} 


/* Check and see whether the next line continues the
   present line.  Marker for line continuation is `$'
   in column 6. */
void
check_continued_lines (FILE * ifp, char *current_line)
{
    int items;
    char next_line[100];

    /* Now we have to determine whether the statement
       is continued on the next line by getting another 
       line and examining column 6 for a continuation marker.
     */
    while (1)
      {
	  items = fread (next_line, 6, 1, ifp);
	  /* If we are NOT at the end of file, reset the 
	     pointer to the start of the line so that 
	     the next fgets will grab the entire line.
	   */
	  if (items == 0)
	      return;		/* End of file. */
	  /*  Accept either "$" or "*"  in column 6 to
	      indicate that the statement continues on
	      the next line.  */
	  if (next_line[5] != '*' &&
	      next_line[5] != '$')
	      /*  There is no continuation marker.  Reset the 
	         pointer to the start of the line, and return. */
	    {
		fseek (ifp, -6, 1);
		return;
	    }
	  else
	      /* We have a continuation marker.  Get another line
	         and cat it to the previous. */
	    {
		if(lexdebug)
		  printf ("char 6, next_line: %c\n", next_line[5]);
		fgets (next_line, 100, ifp);
		strcat (current_line, next_line);
		lineno++;
	    }
      }
}          /* End of check_continued_lines().  */

/*
 * Step through the symbol tables to see if the current string can be
 * recognized as a token in the symbol table.  Also, set yytext here.
 */
int 
keyscan (register KWDTAB * tab, BUFFER * bufstruct)
{
    int tokenlength;
    char *scp, *yycp;  
    scp = bufstruct->stmt;
    yycp = bufstruct->text;

    while (tab->kwd)
      {
      	/* Get the stringlength of the token in the symbol table.
	   A better way to do this might be to include the length
	   of the keyword in the table instead of computing it
	   everytime. */
	  tokenlength = strlen (tab->kwd);	
	  /* Try to match a substring of the  current string (scp).*/
	  if (!strncmp (scp, tab->kwd, tokenlength))
	    {
		strncpy (yytext, yycp, tokenlength);
		yycp += tokenlength;
		yytext[tokenlength] = '\0';
		strcpy (bufstruct->text, yycp);
		/*  Save the type or kind of relational operator
		    for immediate reduction in the parser.  This
		    implementation is pretty lame, a hold over
		    from Levine's quick and dirty lexer.  */
                if(tab->ktok == TYPE) 
                  yylval.type = tab->klex;
                if(tab->ktok == RELOP)
                    yylval.tok = tab->klex;
		/*  Now set the string pointer to point at the first
		   character past the end of the string.
		 */
		scp += tokenlength;
		strcpy (bufstruct->stmt, scp);
		return tab->ktok;
	    }
	  tab++;		/* Check the next table entry.  */
      }				/* Close the while() loop.  */
    return 0;			/* Not a recognized token. */
}				/* Close keyscan(). */


/* Called after hash lookup indicates there is java method
   equivalent in the fortran source code.  Returns a pointer
   to the java string equivalent to the fortran source code.
   This is surely a hack. */
char *
methodscan (METHODTAB * tab, char * name)
{

    /*  The method translation table is initialized in
	the header block of this file.  We treat the table
	as a linear linked list by stepping through the
	array entries with the pointer `*tab'. Note that
	`NULL' last entry in the table shuts down the for()
	loop.  */
/*  for (temptab;temptab->fortran_name  != NULL;temptab++)  */

  while (tab->fortran_name != NULL) { 
    /* if (*tab->fortran_name == NULL) return 0; */
    if (tab->fortran_name == NULL) return 0;

    if (!strcmp (tab->fortran_name,name)) {
      if(lexdebug)printf("java_name: %s\n", tab->java_method); 
      printf("java_name: %s\n", tab->java_method); 

      return tab->java_method; 
    }
    tab++;
  }                           /* Close for() loop.   */

  printf("Segfault after here\n");

  return 0;                     /* Not in table.       */
}				/* Close methodscan(). */


/*  Scan a card image for a named identifier.
   */
int
name_scan (BUFFER * bufstruct)
{
    char *ncp, *tcp;
    int tokenlength = 0;
    /*    int type = INTEGER;  */
    ncp = bufstruct->stmt;
    tcp = bufstruct->text;

/*  Find the name. 
   We checked the first character in yylex to make sure 
   it was alphabetic. 
 */
    while (isalnum (*ncp))
      {
	  ncp++;
	  tokenlength++;
      }
    strncpy (yylval.lexeme, tcp, tokenlength);
    yylval.lexeme[tokenlength] = '\0';
    tcp += tokenlength;
    strcpy (bufstruct->text, tcp);
    strcpy (bufstruct->stmt, ncp);
    return NAME;
}				/*  Close name_scan().  */


/*  Scan a card image for a numerical constant.
    Need to add code in here to change exp numbers
    to doubles, or at least to replace the instances
    of 'd' and 'D' with 'e'.  */
int
number_scan (BUFFER * bufstruct)
{
  extern KWDTAB tab_toks[];
  
    char *ncp, *tcp;
    BUFFER tempbuf;
    int token;
    int tokenlength = 0;
    int type = INTEGER;  /* Default, in case we find nothing else. */
    ncp = bufstruct->stmt; /*  Number character pointer. */
    tcp = bufstruct->text;  /* Literal text character pointer. */

/*  Test and see whether it is a number (constant).
   If so, store the literal text in yytext.  These
   long logical expressions are probably not very 
   efficient, but they should be easy to read.
 */
    while (isdigit (*ncp) ||
	   *ncp == '.' ||
	   *ncp == 'D' ||
	   *ncp == 'd' ||
	   *ncp == 'E' ||
	   *ncp == 'e')
      {
	  switch (*ncp)
	    {
	    case '.':

/*
   If there is a dot, there may be a float or double or
   exponential, or an integer followed by a keyword such as
   .AND., .OR., etc.
 */
		strcpy (tempbuf.stmt, ncp);
		strcpy (tempbuf.text, tcp);
		token = keyscan (tab_toks, &tempbuf);
		if (token) break; /* Leave the while() loop. */

/* Else if there is no token returned, check for 
   the usual double or exponential number. */

		/* If the next character, i.e. *(ncp+1) is a digit, 
		   increment and continue while loop,
		   else get out of while loop.
		 */
		if (isdigit (*(ncp + 1)))
		  {
		      ncp += 2;
		      tokenlength += 2;
		      type = DOUBLE;	/* Case of `nn.dd...' */
		      continue;	/* Control passes to back to 
				   while() loop; get another 
				   character. */
		  }
		else
		  {
		      ncp++;
		      tokenlength++;
		      type = DOUBLE;	/* Case of `nn.' */
		      continue; /* Back to while() loop
				   for another character.*/
		  }
		/* This exponential notation stuff works pretty good.
		   It will need to be modified to express the
		   number in exponential notation as an equivalent
		   double. 
		   */
	    case 'E':
	    case 'e':
	    case 'D':
	    case 'd':
		/*  First, take care of the case that looks like this:
		   1.0e+1 or 1.0e-1. */
		if (*(ncp + 1) == '+' ||
		    *(ncp + 1) == '-')
		  {
		      ncp += 2;
		      tokenlength += 2;
		      type = EXPONENTIAL;
		      continue;	/*  Loop again.  */
		  }
		/*  Now take care of cases that look like 
		   this:  1.0e1.
		 */
		if (isdigit (*(ncp + 1)))
		  {
		      ncp++;
		      tokenlength++;
		      type = EXPONENTIAL;
		      continue;	/*  Loop again.  */
		  }
		else
		    break;	/*  Break switch. */

	    default:		/*  All digits do this.  */
		ncp++;
		tokenlength++;
		continue;	/*  Loop again.  */
	    }			/* Close switch(). */
	  break;
      }				/* Close while() loop. */

    strncpy (yylval.lexeme, tcp, tokenlength);
    yylval.lexeme[tokenlength] = '\0';
    if(lexdebug)printf ("Number: %s\n", yytext);
    tcp += tokenlength;
    strcpy (bufstruct->text, tcp);
    strcpy (bufstruct->stmt, ncp);
    return type;
}				/* Close name_ident_scan().  */


int
string_or_char_scan (BUFFER * bufstruct)
{
    int tokenlength = 0;
    char *scp, *textcp;
    scp = bufstruct->stmt;
    textcp = bufstruct->text;

/*  Test and see if there is a tic (`'') mark.  */
    if (*scp == '\'')
      {
	  scp++;
	  textcp++;
	  if(lexdebug)printf ("scp: %s\n", scp);
	  /* Loop until we find another tick (') mark. */
	  while (*scp != '\'')
	    {
		scp++;
		tokenlength++;
	    }
	  strncpy (yytext, textcp, tokenlength);
	  yytext[tokenlength] = '\0'; /* Terminate the string at tick. */
	  strcpy(yylval.lexeme, yytext); 
	  textcp += tokenlength;
          /* Now increment to get past the tic marks. */
	  scp++;
	  textcp++;
	  strcpy (bufstruct->stmt, scp);
	  strcpy (bufstruct->text, textcp);
	  /* Reset the value; strlen does not include the value
	     of '\0' that terminates the string.  */
          tokenlength = strlen(yylval.lexeme);
/*	  printf("Tokenlength: %d\n", tokenlength); */
	  if (tokenlength == 1)
	    {
	      /*     printf("Got Char, %s\n", yylval.lexeme); */
	      return CHAR;
	    }
	  else
	    {
/*	      printf("Got String, %s\n", yylval.lexeme); */
	      return STRING;
	    }
      }
}				/* Close string_or_char_scan(). */