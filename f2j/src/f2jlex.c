/*
 * $Source$
 * $Revision$
 * $Date$
 * $Author$
 */

/*****************************************************************************
 * f2jlex.c                                                                  *
 *                                                                           *
 * This is a lexer for a Fortran front-end written to                        *
 * translate Fortran numerical linear algebra code into                      *
 * Java.  The lexer interacts with a yacc generated parser                   *
 * and implements a subset of the commands used by the                       *
 * flex scanner.  Due to the nature of yacc (uses globals)                   *
 * the scanner takes no arguments, but examines the globally                 *
 * declared input source buffer.  It returns a single token                  *
 * and it's associated lexical value at each call. EOF                       *
 * condition passes control back to main() for program                       *
 * termination.                                                              *
 *                                                                           *
 *****************************************************************************/

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>
#include"initialize.h"
#include"f2jmem.h"
#include"f2j_externs.h"

char line_buffer[BIGBUFF];
char *comment_buffer[COMMENT_BUFLEN];

char yytext[YYTEXTLEN];          /* token text                               */

/*****************************************************************************
 * Stuff for Sale's algorithm when I get around to it.                       *
 * They need to be global for now to set contexts.  It is                    *
 * probably possible to rewrite in terms of a struct to                      *
 * pass around in the lexer, the contexts will have to be                    *
 * global for the parser to use.                                             *
 *                                                                           *
 * I am setting these in the `collapse_white_space()                         *
 * routine.                                                                  *
 *****************************************************************************/

BOOL letterseen;                 /* we have seen a letter in this line       */
BOOL equalseen;                  /* we have seen an equals in this line      */
BOOL commaseen;                  /* we have seen a comma in this line        */
BOOL progseen;                   /* we have seen a PROGRAM decl in this unit */
BOOL in_iolist;                  /* we are inside an I/O specifier list      */
BOOL iolist_finished;            /* we have finished lexing the I/O spec list*/

/*****************************************************************************
 * A couple of globals for keeping track of tokens we want to pass on        *
 * subsequent calls to yylex().  This is sort of a hack to let us peek       *
 * ahead in the token stream.                                                *
 *****************************************************************************/

int next_tok[NT_NUM];
YYSTYPE next_yylval[NT_NUM];

/*****************************************************************************
 * a couple of buffers for manipulating the text of the current line.        *
 *****************************************************************************/

typedef struct _buffer
{
  char stmt[BIGBUFF];            
  char text[BIGBUFF];            
}
BUFFER;

/*****************************************************************************
 * Function prototypes:                                                      *
 *****************************************************************************/

int 
  yylex(void),
  prelex(BUFFER *);

char 
  *tok2str(int),
  *f2j_fgets(char *, int, FILE *);

FILE
  *open_included_file(char *);

int
  name_scan(BUFFER *),
  keyscan(register KWDTAB *, BUFFER *),
  number_scan(BUFFER *, int, int),
  string_or_char_scan(BUFFER *);

void
  truncate_bang_comments(char *),
  check_continued_lines(FILE *, char *),
  collapse_white_space(BUFFER *),
  collapse_white_space_internal(BUFFER *, int);

METHODTAB
  * methodscan(METHODTAB *, char *);

extern Dlist
  file_stack;

/*****************************************************************************
 * STANDALONE is defined in the makefile when compiling the                  *
 * lex file as a stand alone program for debugging the lexer.                *
 *****************************************************************************/

#ifdef STANDALONE

union yylval_ {
  struct ast_node *ptnode;            /* pointer to AST node                 */
  int tok;                            /* token ID                            */
  enum returntype type;               /* data type                           */
  char lexeme[30];                    /* text token                          */
} yylval;

/*****************************************************************************
 * This main function is used for testing the lexer.  It is only compiled    *
 * if STANDALONE is defined.                                                 *
 *****************************************************************************/

main(int argc, char **argv)
{
  extern FILE *ifp;
  int token = 1;
  ifp = fopen(argv[1], "rb");

  while(token != 0)
  {
    token = yylex();

    /* This prints out some random int on the EOF 
     * condition. 
     */

    if(lexdebug) {
      printf("From main: %d\n", token);
      printf("yytext: %s\n\n", yytext);
    }
  }

  if(lexdebug)
    printf("EOF\n");
}				/*   Close main().  */

#endif /*    STANDALONE   */

/*****************************************************************************
 *                                                                           *
 * yylex                                                                     *
 *                                                                           *
 *  yylex() has to call prelex() to take of all the                          *
 * fortran nasties such as initial whitespace, unreserved                    *
 * keywords, context sensitivity, etc.  prelex() returns                     *
 * a "card image" of characters to be tokenized.                             *
 *****************************************************************************/

int
yylex()
{
  static int tokennumber;
  static int firsttoken;
  static int parencount = 0;
  static int format_stmt;    /* are we lexing a format statement */
  static int first_call = 1;
  int i, token = 0;

  /* yyparse() makes a call to yylex() each time it needs a
   * token.  To get a statement to parse, yylex() calls
   * prelex() with the statement buffer.  This occurs
   * when the value of the statement buffer is 0.
   * Since we don't want the statement to change between
   * calls, we declare it static, and initialize it to
   * null at the start of the program.  We may also need
   * the actual text of the fortran input, so we grab
   * that also.  
   */

  static BUFFER buffer = 
  {
    {0},               /* Token string.  */
    {0}                /* Text string.   */
  };

  /* on the first call to yylex(), initialize some variables */

  if(first_call) {
    first_call = 0;
    lineno = 0;
    progseen = FALSE;

    for(i=0;i<NT_NUM;i++) {
      next_tok[i] = 0;
      memset(&next_yylval[i], 0, sizeof(next_yylval[i]));
    }

    for(i=0;i<COMMENT_BUFLEN;i++)
      comment_buffer[i] = NULL;
  }

  /* if there were tokens saved from a previous call, then
   * return them now.  as mentioned above, this is a hack to
   * provide some lookahead.
   */
  for(i=0;i<NT_NUM;i++) {
    if(next_tok[i]) {
      if(lexdebug)
         printf("0: lexer returns previous saved token %s (%s)\n",
            tok2str(next_tok[i]),buffer.stmt);

      token = next_tok[i];

      /* clear out this entry so we don't try to return it next time */

      next_tok[i] = 0;
      memcpy(&yylval, &next_yylval[i], sizeof(next_yylval[i]));
      memset(&next_yylval[i], 0, sizeof(next_yylval[i]));

      return token;
    }
  }

  /* Test so that yylex will know when to call prelex to get 
   * another character string.  
   */

  if(buffer.stmt[0] == '\0')
  {
    tokennumber = 0;    /* Reset for each statement. */
    parencount  = 0;    /* Reset for each statement. */
    format_stmt = 0;    /* Reset for each statement. */
    firsttoken  = 0;    /* Reset for each statement. */

    if(lexdebug) printf("calling prelex\n");
    token = prelex(&buffer);   /* No more tokens? Get another statement. */

    if(token == INCLUDE) {
      INCLUDED_FILE *newfile;
      FILE *tempfp;
      Dlist lp;
      int tmplen;

      buffer.stmt[0] = '\n'; buffer.stmt[1] = '\0';
      buffer.text[0] = '\n'; buffer.text[1] = '\0';

      /* check for cycle in the include stack */
      dl_traverse(lp, file_stack) {
        newfile = (INCLUDED_FILE *)dl_val(lp);
        if( !strcmp(newfile->name, yylval.lexeme) ) {
          fprintf(stderr,"Warning: loop in include (not including %s)\n",
             yylval.lexeme);
          strcpy(yylval.lexeme,"Include error\n");

          return COMMENT;
        }
      }

      tempfp = open_included_file(yylval.lexeme);

      /* add the newline since we will send a COMMENT token back
       * to the parser, with yylval containing the file name.  the
       * parser expects all comments to be terminated with \n\0.
       */
      tmplen = strlen(yylval.lexeme);
      yylval.lexeme[ tmplen ] = '\n';
      yylval.lexeme[ tmplen + 1] = '\0';

      if(!tempfp) {
        fprintf(stderr,"Error: could not open include file %s",
          yylval.lexeme);
        return COMMENT;
      }

      current_file_info->line_num = lineno+1;

      newfile = (INCLUDED_FILE *)f2jalloc(sizeof(INCLUDED_FILE));

      /* for internal use, strip the newline from the file name */
      newfile->name = strdup(yylval.lexeme);
      newfile->name[strlen(newfile->name)-1] = '\0';
      newfile->line_num = 0;
      newfile->fp = tempfp;

      dl_insert_b(file_stack, current_file_info);
      ifp = tempfp;
      current_file_info = newfile;
      lineno = 0;

      return COMMENT;
    }

    if(token == BLOCK_COMMENT) {
      if(lexdebug)
         printf("0.1: lexer returns COMMENT [was buffered] (%s)\n",
            buffer.stmt);

      next_tok[0] = NL;
      memset(&next_yylval[0], 0, sizeof(next_yylval[0]));

      return COMMENT;
    }

    if(token == COMMENT) {
      if(lexdebug)
         printf("0.2: lexer returns %s (%s)\n", tok2str(token),buffer.stmt);
      buffer.stmt[0] = '\n'; buffer.stmt[1] = '\0';
      buffer.text[0] = '\n'; buffer.text[1] = '\0';
      return COMMENT;
    }
  }

  if(lexdebug)
    printf("here in yylex(), buffer.stmt = \"%s\"\n",buffer.stmt);

  /* Check for end of file condition.  */

  if(*buffer.stmt == '\0') {
    /* I am not sure exactly what is going on here...
     * I may later comment this out to investigate the
     * behavior.  If this does work, it is confusing with
     * what I said above.  
     */

    if(lexdebug) 
      printf("(first): lexer returning 0 \n");
    return 0;
  }

  /* All the context handling will need to be handled 
   * before keyscanning.  Contexts will include `if' 
   * and `do' statements.  This is "Sale's algorithm" 
   * stuff.  The global vars commaseen, letterseen and 
   * equalseen are boolean flags set in the
   * `collapse_white_space()' procedure. 
   */

  /* This section of code has grown to the point where it needs
   * to broken into one or two smaller procedures.  It 
   * is getting difficult to follow.  -dmd 9/26/97 
   */

#define SALES 1  

#if SALES

  /* Fortran statements begin with a keyword except under
   * certain very specific circumstances (detailed in
   * technical report.  */

  if(tokennumber == 0)
  {
    if(commaseen == FALSE &&
       equalseen == TRUE &&
       letterseen == FALSE)
    {
      if(isalpha( (int) *buffer.stmt))
        token = name_scan(&buffer);

      if(token)
      {
        /* at this point we are looking at a NAME token */

        if(!progseen) {
          /* we haven't seen a PROGRAM declaration yet and we
           * are looking at a NAME token, so there cannot be
           * an explicit PROGRAM decl, so we return NO_PROGRAM
           * here.  it's a bit messy here, but it makes things
           * easier in the parser (avoiding conflicts).
           */
          progseen = TRUE;

          /* here we set up the next two tokens:
           *   NO_PROGRAM - return on this call
           *   next_tok[0] = current token (i.e. NAME), return on next call
           *   next_tok[1] = unused
           */

          next_tok[0] = token;
          memcpy(&next_yylval[0], &yylval, sizeof(yylval));
          next_tok[1] = 0;
          memset(&next_yylval[1], 0, sizeof(next_yylval[0]));
          token = NO_PROGRAM;
          if(lexdebug)
            printf("0.8: lexer returns %s (%s)\n",
               tok2str(token),buffer.stmt);
          return token;
        }
        tokennumber++;
        if(lexdebug)
          printf("1: lexer returns %s (%s)\n",
             tok2str(token),buffer.stmt);
        return token;
      }
      /*  Trap errors.  */
    }
    else /* Other three cases. */
    {
      if(lexdebug)
        printf("keyscanning %s, ",buffer.stmt);

      /* check for type declarations, e.g. INTEGER, REAL, LOGICAL, etc. */

      token = keyscan(tab_type, &buffer);

      if(lexdebug)
        printf("token = %d\n",token);

      if(token)
      {
        if(!progseen) {
          /* we haven't seen a PROGRAM declaration yet and we
           * are looking at either an ARITH_TYPE or CHAR_TYPE token,
           * (INTEGER, REAL, CHARACTER, etc).  This can either be a
           * variable declaration or a function declaration,
           * depending on the following token.  if this is a type
           * declaration and we haven't seen a PROGRAM declaration,
           * then we return NO_PROGRAM here.  if this is a function
           * declaration, then this is the beginning of a new
           * FUNCTION program unit, so we do not want to return
           * NO_PROGRAM, just return the tokens as usual.
           */
          progseen = TRUE;

          /* here we set up the next tokens:
           *   NO_PROGRAM - return on this call
           *   next_tok[0] = current token (i.e. NAME), return on next call
           *   next_tok[1] = unused
           */

          /* NOTE: the order of the statements here is important.
           * we don't want to set any next_tok[..] before the call to yylex()
           * because then it will just return that token instead of lexing the
           * next token.
           *
           * first, save yylval (it's ok to write to next_yylval[0] before
           * the yylex() call because it only checks whether next_tok[..] is
           * set).  then call yylex() to get the next token (to check if
           * it's a function or variable name).  then we can save the tokens
           * in next_tok[..] for the next call.
           */
          memcpy(&next_yylval[0], &yylval, sizeof(yylval));
          next_tok[1] = yylex();
          memcpy(&next_yylval[1], &yylval, sizeof(yylval));
          next_tok[0] = token;

          if(next_tok[1] == FUNCTION) {
            /* if this declaration is for a FUNCTION, then we basically
             * want to set things back to the original state - but since
             * we've already lexed the next token to check for the function
             * decl, we need to use next_tok[..] here.  'token' still
             * contains the original token, so just return that now and
             * put the FUNCTION token in next_tok[0] to be returned next time.
             */
            memcpy(&yylval, &next_yylval[0], sizeof(yylval));
            memcpy(&next_yylval[0], &next_yylval[1], sizeof(yylval));
            next_tok[0] = next_tok[1];
            next_tok[1] = 0;
            memset(&next_yylval[1], 0, sizeof(next_yylval[1]));
          }
          else {
            /* not a function, so return NO_PROGRAM as described above */
            token = NO_PROGRAM;
          }

          if(lexdebug)
            printf("0.9: lexer returns %s (%s)\n",
               tok2str(token),buffer.stmt);
          return token;
        }

        firsttoken = token;
        tokennumber++;
        if(lexdebug)
          printf("2: lexer returns %s (%s)\n",
            tok2str(token),buffer.stmt);
        return token;
      }

      token = keyscan(tab_stmt, &buffer);

      if(token)
      {
        firsttoken = token;
        tokennumber++;

        if((token == PROGRAM) || (token == SUBROUTINE) || (token == FUNCTION))
          progseen = TRUE;

        yylval.lexeme[0] = '\0';

        if(!progseen) {
          /* see comments above for a description of the NO_PROGRAM token.
           * here we are looking at a statement token - DO, IF, READ, WRITE,
           * CALL, etc. (see tab_stmt[]).  
           */
          progseen = TRUE;

          /* here we set up the next two tokens:
           *   NO_PROGRAM - return on this call
           *   next_tok[0] = current token, return on next call
           *   next_tok[1] = unused
           */

          next_tok[0] = token;
          memcpy(&next_yylval[0], &yylval, sizeof(yylval));
          next_tok[1] = 0;
          memset(&next_yylval[1], 0, sizeof(next_yylval[1]));

          if(token == END) {
            /* this handles a pretty obscure case - if the only line in
             * the file is "END", then return NO_PROGRAM, DUMMY, and
             * finally the current token (END).  DUMMY is just a non-terminal
             * for the parser to pick up on.
             */
            func_stmt_num = 0;
            next_tok[0] = DUMMY;
            memset(&next_yylval[0], 0, sizeof(next_yylval[0]));
            next_tok[1] = token;
            memcpy(&next_yylval[1], &yylval, sizeof(yylval));
          }

          token = NO_PROGRAM;

          if(lexdebug)
            printf("2.9: lexer returns %s (%s)\n",
               tok2str(token),buffer.stmt);
          return token;
        }

        if(token == END) {
          func_stmt_num = 0;
          progseen = FALSE;
          next_tok[0] = token;
          memset(&next_yylval[0], 0, sizeof(next_yylval[0]));
          next_tok[1] = 0;
          token = DUMMY;
        }

        if(lexdebug)
          printf("3: lexer returns %s (%s)\n",
            tok2str(token),buffer.stmt);
        return token;
      }

      /*  Scan for a labeled (numbered) statement. */
      if(isdigit((int) *buffer.stmt))
        token = number_scan(&buffer, format_stmt, tokennumber);

      if(token)
      {
        firsttoken = token;
        tokennumber++;

        /* this is really a hack.  I'm trying to sniff out
         * labeled else/elseif/endif statements and avoid
         * passing the integer token back to the parser.
         * I was getting several shift/reduce conflicts and
         * didn't want to sort them out, especially since
         * the label is ignored for else and elseif.  For
         * endif, we let the label get passed back to the
         * parser in yylval.lexeme.
         */

        if(!strncasecmp(buffer.stmt, "else", 4) ||
           !strncasecmp(buffer.stmt, "elseif", 6) ||
           !strncasecmp(buffer.stmt, "endif", 5))
        {
          token = keyscan(tab_stmt, &buffer);

          if(!token) {
            fprintf(stderr, "Error: expected keyword token.\n");
            exit(-1);
          }

          if(lexdebug)
            printf("3.9: lexer returns %s (%s)\n",
              tok2str(token),buffer.stmt);
          return token;
        }

        if(!progseen) {
          /* see comments above for a description of the NO_PROGRAM token.
           * here we are looking at a labeled statement.  handle this the same
           * way as above.
           */
          progseen = TRUE;

          /* here we set up the next two tokens:
           *   NO_PROGRAM - return on this call
           *   next_tok[0] = current token, return on next call
           *   next_tok[1] = unused
           */

          next_tok[0] = token;
          memcpy(&next_yylval[0], &yylval, sizeof(yylval));
          next_tok[1] = 0;
          memset(&next_yylval[1], 0, sizeof(next_yylval[0]));
          token = NO_PROGRAM;
          if(lexdebug)
            printf("3.9: lexer returns %s (%s)\n",
               tok2str(token),buffer.stmt);
          return token;
        }

        if(lexdebug)
          printf("4: lexer returns %s (%s)\n",
            tok2str(token),buffer.stmt);
        return token;
      }
      /*  Should probably trap errors here.  */
    }
    /*  Should probably trap errors here.  */
  } /* Close if (firsttoken == 0).  */

  if(lexdebug)
    printf("func_stmt_num = %d, firsttoken = %d, and tokennumber = %d\n",
      func_stmt_num,firsttoken,tokennumber);

  if((func_stmt_num == 1) && 
     ((firsttoken == ARITH_TYPE) || (firsttoken == CHAR_TYPE)) && 
      (tokennumber ==1))
  {
    token = keyscan(tab_stmt, &buffer);

    if(token)
    {
      tokennumber++;
      if(lexdebug)
        printf("5: lexer returns %s (%s)\n",tok2str(token),
          buffer.stmt);
      return token;
    }
  }
    
  /* If we're tokenizing an IMPLICIT statement, then we need to check
   * whether we're inside the parens or not.  If not, then this must
   * be a type (integer, real, etc).  If inside the parens, then this
   * must be a letter or hyphen.  We pass the letter as a NAME token.
   */

  if(firsttoken == IMPLICIT) {
    if(lexdebug)
      printf("first tok is IMPLICIT, parentcount = %d\n",parencount);

    if(parencount > 0) {
      if(isalpha( (int) *buffer.stmt))
        token = name_scan(&buffer);
    }
    else {
      token = keyscan(tab_type, &buffer);
    }

    if(token) {
      tokennumber++;
      if(lexdebug)
        printf("5.1: lexer returns %s (%s)\n",tok2str(token),
          buffer.stmt);
      return token;
    }
  }

  /* Since we are tracking parentheses, we need to
   * scan for miscellaneous tokens.  We are really
   * sniffing for parens... 
   */

  token = keyscan(tab_toks, &buffer);

  /* set a flag for when we are lexing the list of IO specifiers.
   * trying to avoid problems with variable names that clash with
   * the specifiers (e.g. WRITE(*,*) ERROR was getting lexed as ERR).
   */
  if((firsttoken == READ) || (firsttoken == WRITE) ||
     (firsttoken == OPEN) || (firsttoken == CLOSE) ||
     (firsttoken == REWIND))
  {
    if((token == OP) && !in_iolist)
      in_iolist = TRUE;

    if((token == CP) && (parencount == 1) && in_iolist)
      iolist_finished = TRUE;
  }

  /* if we found no keyword and this is an I/O statement,
   * check for io specifier keywords (UNIT, ERR, etc).
   */

  if(!iolist_finished) {
    if(!token && (firsttoken == READ))
      token = keyscan(read_write_toks, &buffer);

    if(!token && (firsttoken == WRITE))
      token = keyscan(read_write_toks, &buffer);

    if(!token && (firsttoken == OPEN))
      token = keyscan(open_toks, &buffer);

    if(!token && (firsttoken == CLOSE))
      token = keyscan(close_toks, &buffer);

    if(!token && (firsttoken == REWIND))
      token = keyscan(rewind_toks, &buffer);
  }

  if(token)
  {
    if(token == OP)
      parencount++;
    if(token == CP)
      parencount--;
    tokennumber++;

    if(lexdebug)
      printf("6: lexer returns %s (%s)\n",
        tok2str(token),buffer.stmt);

    return token;
  }

  /* Now check context again. This should be the only other
   * place necessary to scan for keywords.  The keywords we
   * expect to find are THEN, CONTINUE, and logical if
   * statement keywords.  
   */

  if((letterseen == TRUE        &&
      (firsttoken == IF || firsttoken == ELSEIF)     &&
       parencount == 0)          ||
	/*  Takes care of labeled (numbered) statements,
	 *  i.e. 10 CONTINUE.  */
       firsttoken == INTEGER)
  {
    if(equalseen == TRUE)
    {
      char *stmt_copy = strdup(buffer.stmt);
      char *text_copy = strdup(buffer.text);

      /*Changed on 2/27/01 added if statement to catch if variable*/     
      token = keyscan(tab_stmt, &buffer);
      if(  ((token == DO) || (token == IF)) 
         && 
          /* (((tokennumber != 1) && (firsttoken != INTEGER)) || */
          (((tokennumber != 0) && (firsttoken != INTEGER)) ||
          ((tokennumber != 1) && (firsttoken == INTEGER)))
        )
      {
        if(lexdebug)
           printf("got incorrect DO or IF keyword, restoring buffer\n");
        strcpy(buffer.stmt,stmt_copy);
        strcpy(buffer.text,text_copy);
      }
      else{
         /* First, look for labeled DO statement */
         strcpy(buffer.stmt,stmt_copy);
         strcpy(buffer.text,text_copy);
         if((token = keyscan(tab_stmt, &buffer)) == DO)
         {
           if(lexdebug)
             printf("7.1: lexer returns %s (%s)\n",tok2str(token),buffer.stmt);
           f2jfree(stmt_copy, strlen(stmt_copy)+1);
           f2jfree(text_copy, strlen(text_copy)+1);
           return token;
         }
         strcpy(buffer.stmt,stmt_copy);
         strcpy(buffer.text,text_copy);
         if((token = keyscan(tab_stmt, &buffer)) == IF)
         {
           if(lexdebug)
             printf("7.1.2: lexer returns %s (%s)\n", tok2str(token), buffer.stmt);
           return token;
         }
      }

      strcpy(buffer.stmt,stmt_copy);
      strcpy(buffer.text,text_copy);

      if(isalpha((int) *buffer.stmt))
        token = name_scan(&buffer);

      if(token)
      {
        tokennumber++;
        if(lexdebug)
        {
          printf("7.2: lexer returns %s (%s)\n",tok2str(token),buffer.stmt);

          if(token == NAME)
            printf("7.2: ...and the name is %s\n",yylval.lexeme);
        }
        f2jfree(stmt_copy, strlen(stmt_copy)+1);
        f2jfree(text_copy, strlen(text_copy)+1);
        return token;
      }
      f2jfree(stmt_copy, strlen(stmt_copy)+1);
      f2jfree(text_copy, strlen(text_copy)+1);
    }
    else  /*  equalseen == FALSE.  */
    {
      char *stmt_copy = strdup(buffer.stmt);
      char *text_copy = strdup(buffer.text);

      token = keyscan(tab_stmt, &buffer);

      /* There should probably be a trap in here to catch
            bad keywords. */
      if(token)  
      {
        if(  ((token == DO) || (token == IF) || (token == DATA))
           && 
             /* (((tokennumber != 1) && (firsttoken != INTEGER)) || */
             (((tokennumber != 0) && (firsttoken != INTEGER)) ||
              ((tokennumber != 1) && (firsttoken == INTEGER)))
          )
        {
          if(lexdebug)
            printf("got incorrect DO or IF keyword, restoring buffer\n");
          strcpy(buffer.stmt,stmt_copy);
          strcpy(buffer.text,text_copy);
        }
        else {
          tokennumber++;

          if(token == FORMAT)
            format_stmt = 1;

          /* HACK: fix to allow labeled OPEN, READ, WRITE statements.
           * maybe this should be the general case for all tokens..
           * i.e. just always reset firsttoken to the first 'real'
           * token.  however, there are too many other special
           * cases in here checking if firsttoken == INTEGER, so
           * it would probably break something.  --kgs 10/2010
           */
          if((token == OPEN) && (firsttoken == INTEGER))
            firsttoken = OPEN;
          if((token == READ) && (firsttoken == INTEGER))
            firsttoken = READ;
          if((token == WRITE) && (firsttoken == INTEGER))
            firsttoken = WRITE;

          if(lexdebug)
            printf("8: lexer returns %s (%s)\n",
               tok2str(token),buffer.stmt);

          f2jfree(stmt_copy, strlen(stmt_copy)+1);
          f2jfree(text_copy, strlen(text_copy)+1);
          return token;
        }
      }
      else {
        /* trying to trap the TO in ASSIGN integer TO name. 
         * check tokennumber == 3 to avoid checking the name part (since the
         * name could be "TO").   using 3 because we have a label
         * number as the first token and we start numbering at 0, so
         * the TO keyword would be number 3.
         */

        if(!commaseen && (tokennumber == 3)) {
          token = keyscan(assign_toks, &buffer);

          if(token) {
            tokennumber++;

            if(lexdebug)
              printf("8.1: lexer returns %s (%s)\n",
                 tok2str(token), buffer.stmt);
            return token;
          }
        }
      }

      f2jfree(stmt_copy, strlen(stmt_copy)+1);
      f2jfree(text_copy, strlen(text_copy)+1);
    }
  }
    
  /* If we are parsing an ASSIGN statement, trap the TO keyword.
   * There's no label number, so the token number is 2.  (see
   * comment above).
   */

  if((firsttoken == ASSIGN) && (tokennumber == 2)) {
    token = keyscan(assign_toks, &buffer);

    if(token) {
      tokennumber++;

      if(lexdebug)
        printf("8.2: lexer returns %s (%s)\n",
           tok2str(token), buffer.stmt);
      return token;
    }
  }

  if(isalpha((int) *buffer.stmt))
    token = name_scan(&buffer);

  if(token)
  {
    tokennumber++;

    if(lexdebug)
      printf("firsttoken = %s and format_stmt = %s\n",
        tok2str(firsttoken), format_stmt?"TRUE":"FALSE");

    /* check to see if we're parsing a FORMAT statment so
     * that we can look for edit speicification characters 
     */

    if((firsttoken == INTEGER) && (format_stmt)) {
      if(lexdebug)
        printf("****the spec is '%s'\n", yylval.lexeme);

      if((yylval.lexeme[0] == 'X') || (yylval.lexeme[0] == 'P') ||
         (yylval.lexeme[0] == 'x') || (yylval.lexeme[0] == 'p'))
      {
        char *tmp;
    
        token = EDIT_DESC;
        if(strlen(yylval.lexeme) > 1) {
          if(lexdebug)
            printf("now we want to push '%s' back before '%s'\n",
              yylval.lexeme + 1,buffer.stmt);

          tmp = strdup(buffer.stmt);
          strcpy(buffer.stmt,yylval.lexeme + 1);
          strcat(buffer.stmt,tmp);
          yylval.lexeme[1] = '\0';

          if(lexdebug)
            printf("now lexeme = '%s' and buffer.stmt = '%s'\n",
              yylval.lexeme,buffer.stmt);

          strcpy(buffer.text,buffer.stmt);
        }
      }

      if( (yylval.lexeme[0] == 'A') || (yylval.lexeme[0] == 'a') ||
          (yylval.lexeme[0] == 'F') || (yylval.lexeme[0] == 'f') ||
          (yylval.lexeme[0] == 'I') || (yylval.lexeme[0] == 'i') ||
          (yylval.lexeme[0] == 'D') || (yylval.lexeme[0] == 'd') ||
          (yylval.lexeme[0] == 'G') || (yylval.lexeme[0] == 'g') ||
          (yylval.lexeme[0] == 'E') || (yylval.lexeme[0] == 'e') ||
          (yylval.lexeme[0] == 'L') || (yylval.lexeme[0] == 'l'))
      {
        token = EDIT_DESC;

        /* the following if statment grabs format specs like
         * G10.3 (although, at this point, we've already got
         * G10 so now we want to grab the rest and append it) 
         */

        if( buffer.stmt[0] == '.' )
        {
          char *bufptr = strdup(buffer.stmt);
          int len=1;
 
          /* len is initialized to 1, so we skip the '.' char */
          while(!isdigit((int) bufptr[len]))
            len++;
              
          bufptr[len+1] = '\0';
          strcat(yylval.lexeme,bufptr);
          f2jfree(bufptr, strlen(bufptr)+1);
          bufptr = strdup(buffer.stmt + len + 1);

          strcpy(buffer.stmt,bufptr);
          strcpy(buffer.text,bufptr);

          f2jfree(bufptr, strlen(bufptr)+1);
        }

        if(lexdebug)
          printf("8.5: lexer returns %s (%s)\n", 
            tok2str(token),buffer.stmt);
        return token;
      }
    }

    if((firsttoken == IMPLICIT) && 
       (!strcmp(yylval.lexeme,"NONE") || !strcmp(yylval.lexeme,"none")))
         token = NONE;

    if(lexdebug)
      printf("9: lexer returns %s (%s)\n",tok2str(token),buffer.stmt);

    return token;
  }

  if(isdigit((int) *buffer.stmt) || *buffer.stmt == '.') {
    token = number_scan(&buffer,format_stmt, tokennumber);
  }

  if(token)
  {
    tokennumber++;

    if(lexdebug) {
      printf("10: lexer returns %s (%s)\n",tok2str(token),buffer.stmt);
      printf("10: lexeme is '%s'\n",yylval.lexeme);
    }

    return token;
  }

  token = string_or_char_scan(&buffer);

  if(token)
  {
    tokennumber++;
    if(lexdebug)
      printf("11: lexer returns %s (%s)\n",tok2str(token),buffer.stmt);
    return token;
  }
    
#endif   /* SALES  */

#if NOTSALES	    
  token = keyscan(tab_type, &buffer);
  if(token)
    return token;

  token = keyscan(tab_toks, &buffer);
  if(token)
    return token;

  token = keyscan(tab_stmt, &buffer);
  if(token)
    return token;

  /* Else... we gotta scan the silly string for NAMES or CONSTS. */

  if(isalpha(*buffer.stmt))
    token = name_scan(&buffer);
  if(token)
    return token;

  if(isdigit(*buffer.stmt))
    token = number_scan(&buffer,format_stmt, tokennumber);
  if(token)
    return token;

  token = string_or_char_scan(&buffer);
  if(token)
    return token;
#endif  /* NOTSALES  */


  /*  This code below appears to never get called.
   * Not sure why not.
   */

  if(lexdebug) {
    printf("Token (yylex): %d\n",token);
    printf("(second): lexer returning 0\n");
  }

  return 0;
}				/* Close yylex().  */


/*****************************************************************************
 *                                                                           *
 * open_included_file                                                        *
 *                                                                           *
 * search all the include paths specified on the command line with -I (note  *
 * that the current directory is always included first).  return NULL if     *
 * the file could not be found in any directory.                             *
 *                                                                           *
 *****************************************************************************/

FILE *
open_included_file(char *filename)
{
  Dlist tmp;
  FILE *tempfp;
  char *prefix, *full_file = NULL;

  dl_traverse(tmp, include_paths) {
    prefix = (char *)dl_val(tmp);
    full_file = (char *)f2jrealloc(full_file, 
       strlen(prefix) + strlen(filename) + 2);

    strcpy(full_file, prefix);
    strcat(full_file, FILE_DELIM);
    strcat(full_file, filename);

    if((tempfp = fopen(full_file,"rb")) != NULL)
      return tempfp;
  }

  return NULL;
}

/*****************************************************************************
 *                                                                           *
 * prelex                                                                    *
 *                                                                           *
 * Ok, here is how it is going to work.  yylex() will                        *
 * call prelex() to get a statement that has all of the                      *
 * comments pounded out of it, all the white space                           *
 * collapsed, and all of the line labels, contexts,                          *
 * continuations, etc., set.  What prelex does NOT check                     *
 * is whether there is six spaces of white at the                            *
 * beginning of each statement.                                              *
 *                                                                           *
 *****************************************************************************/

int
prelex(BUFFER * bufstruct)
{
  if(lexdebug)
    printf("entering prelex() \n");

  do {
    if(f2j_fgets(bufstruct->stmt, BIGBUFF, ifp) != NULL)
    {
      lineno++;

      if(lexdebug)
        printf("lineno:%d, line is [%s](%d)\n", lineno, bufstruct->stmt,
           (int)strlen(bufstruct->stmt));

      /* truncate anything beyond 72 characters */
      bufstruct->stmt[72] = '\n';
      bufstruct->stmt[73] = '\0';

      /* Dispose of comments and blank lines for now.
       * Later, a COMMENT token can be defined and the
       * comment returned for inclusion in either
       * source or assembler code. 
       */

      if(bufstruct->stmt[0] == 'c' ||
         bufstruct->stmt[0] == 'C' ||
         bufstruct->stmt[0] == '*' ||
         bufstruct->stmt[0] == '!' ||
         bufstruct->stmt[0] == '\n')
      {
        strcpy(yylval.lexeme, bufstruct->stmt);
        return COMMENT;
      }
  
      if(lexdebug)
        printf("First char in buffer: %c\n", bufstruct->stmt[0]);
  
      /* Ok, we have a line that is not a comment and that 
       * does not start and end with a newline, i.e. blank.
       * If the current statement is continued on the 
       * next line, that statement is catenated to the
       * current statement.
       */
  
      truncate_bang_comments(bufstruct->stmt);
      check_continued_lines(ifp, bufstruct->stmt);
      collapse_white_space(bufstruct);

      if(comment_buffer[0] != NULL)
        return BLOCK_COMMENT;

      if(bufstruct->stmt[0] == '\n') {
        strcpy(yylval.lexeme, bufstruct->stmt);
        return COMMENT;
      }
  
      if( ! strncmp(bufstruct->stmt, "INCLUDE", 7) ) {
        /* we are probably looking at an include statement */
        int iidx, yidx;
        BOOL ftickseen;
  
#define FTICK 39
#define INC_OFFSET 8

        if(bufstruct->stmt[7] != FTICK) {
          fprintf(stderr,"Badly formed INCLUDE statement\n");
          strcpy(yylval.lexeme, bufstruct->stmt);
          return COMMENT;
        }
  
        yidx = 0;
        iidx = INC_OFFSET;
        ftickseen = FALSE;
  
        while( (bufstruct->stmt[iidx] != '\0') && (iidx < BIGBUFF)) {
          if(bufstruct->stmt[iidx] == FTICK) {
            if((bufstruct->stmt[iidx+1] == FTICK)) {
              yylval.lexeme[yidx] = bufstruct->stmt[iidx];
              yylval.lexeme[yidx+1] = bufstruct->stmt[iidx+1];
  
              iidx+=2;
              yidx+=2;
  
              continue;
            }
            else {
              ftickseen = TRUE;
              break;
            }
          }
  
          yylval.lexeme[yidx] = bufstruct->stmt[iidx];
          iidx++;
          yidx++;
        }

        if(! ftickseen) {
          fprintf(stderr,"Badly formed INCLUDE statement\n");
          strcpy(yylval.lexeme, bufstruct->stmt);
          return COMMENT;
        }
  
        yylval.lexeme[yidx] = '\0';

        return INCLUDE;
      }

      if(lexdebug)
        printf("From prelex: %s\n", bufstruct->stmt);

      statementno++;
      func_stmt_num++;
      return 0;
    }

    /* EOF conditions. */

    if(lexdebug)
      printf("EOF\n");

    current_file_info = (INCLUDED_FILE *)dl_pop(file_stack);
    if(current_file_info != NULL) {
      ifp = current_file_info->fp;
      lineno = current_file_info->line_num;
    }
  }while(current_file_info != NULL);

  bufstruct->stmt[0] = '\0';
  return 0;
}

/*****************************************************************************
 *                                                                           *
 * truncate_bang_comments                                                    *
 *                                                                           *
 * This routine takes the buffer and removes "!" style comments.             *
 *                                                                           *
 *****************************************************************************/

void
truncate_bang_comments(char *line)
{
  BOOL in_string = FALSE;
  char *cp;

  for(cp = line; *cp; cp++)
  {
    /* if we see a '!' and we're not in the middle of a string, then
     * truncate the remaining comment.
     */

    if(*cp == '!' && !in_string) {
      *cp = '\n';
      *(cp+1) = '\0';
      break;
    }

    if(*cp == '\'') {
      if(in_string) {
        if(*(cp+1) != '\'')
          in_string = FALSE;
        else
          cp++;
      }
      else {
        in_string = TRUE;
      }
    }
  }
}

/*****************************************************************************
 *                                                                           *
 * collapse_white_space                                                      *
 *                                                                           *
 *  Get rid of all of the white space, newlines, etc. in the                 *
 * statement.  Literal strings are handled by keeping the                    *
 * quoting ticks (') and copying the quoted text verbatim                    *
 * into the returned string. This routine modifies the                       *
 * character array stored in the fields of `bufstruct'.                      *
 * This procedure also implements Sale's algorithm to trap                   *
 * keywords.                                                                 *
 *****************************************************************************/

void
collapse_white_space(BUFFER *buf)
{
  BUFFER buffer_copy;
  char *cp;
  
  /* copy the buffer, collapse white space and then check whether we are
   * going to be looking at a FORMAT statement.  if so, then we will want
   * to be looking out for Hollerith format descriptors.
   */
  memcpy((void *)buffer_copy.stmt, (void *)buf->stmt, BIGBUFF);
  memcpy((void *)buffer_copy.text, (void *)buf->text, BIGBUFF);

  collapse_white_space_internal(&buffer_copy, 0);

  cp = buffer_copy.stmt;

  if(isdigit(*cp)) {
    while(cp && isdigit(*cp))
      cp++;

    if(cp && !strncasecmp(cp, "format", 6)) {
      collapse_white_space_internal(buf, 1);
      return;
    }
  }

  collapse_white_space_internal(buf, 0);
}

void
collapse_white_space_internal(BUFFER * bufstruct, int fmt)
{
  /* `cp' is character pointer, `tcp' is temporary cp and
   * `yycp' points at the text buffer for some (what?) reason.
   */

  register char *cp, *tcp, *yycp;
  char tempbuf[BIGBUFF];
  int i, parens = 0;

  commaseen = FALSE, equalseen = FALSE, letterseen = FALSE; 
  iolist_finished = in_iolist = FALSE;

  tcp = tempbuf;
  yycp = bufstruct->text;

  if(lexdebug)
    printf("entering collapse_white_space, buffer is [%s]\n",
      bufstruct->stmt);
 
  for(cp = bufstruct->stmt; *cp; cp++)
  {
    /* Get rid of all of the newlines, tabs, whitespace.  */
    if(*cp == ' ' ||
       *cp == '\t' ||
       *cp == '\n')
      continue;

    /* if we are looking at a format statement and this is a digit,
     * then try to figure out if this is a Hollerith format descriptor.
     */
    if(fmt && isdigit(*cp)) {
      char *end_digit, *next_non_white, *hp, hnstr[128];
      int hlen;

      /* first copy the digits to hnstr.  this will represent the
       * Hollerith string length.
       */
      hp = hnstr;
      end_digit = cp;
      while(end_digit && isdigit(*end_digit)) {
        *hp = *end_digit;
        end_digit++;
        hp++;
      }

      *hp = '\0'; 
      hlen = atoi(hnstr);

      /* now skip all whitespace beyond the digits */
      next_non_white = end_digit;
      while(next_non_white && isspace(*next_non_white))
        next_non_white++;

      /* if the first non-whitespace character beyond the digits is an H or h,
       * then this is a Hollerith descriptor.
       */

      if(*next_non_white == 'h' || *next_non_white == 'H') {
        *tcp = *yycp = '\'';
        tcp++;
        yycp++;

        cp = next_non_white+1;

        for(i=0; cp && (i < hlen); i++) {
          *tcp = *yycp = *cp;
          tcp++;
          yycp++;
          cp++;
        }

        *tcp = *yycp = '\'';
        tcp++;
        yycp++;

        /* decrement cp by one since continuing the for loop will increment it */
        cp--;

        continue;
      }
    }

    /* If a single front tick is seen, stand by 
     * to copy a literal string, delimited between  
     * two tick marks.  This section in here was left out
     * the string in the prelexed statement.  This was
     * handled with at hack.
     */

    if(*cp == '\'')  /* Escape the tick mark with a slash "\" */
    {
      int done=FALSE;

      *tcp = *cp;
      tcp++;

      /* Hack... */
      *yycp = *cp;
      yycp++;
      cp++;

      while(!done) 
      {
        while(*cp != '\'')  /* Literal copy until next tick. */
        {
          *tcp = *cp;
          tcp++;

          /* Hack...  All this while loop does is increment
           * without using the toupper function.  The next
           * two lines were left out originally. 
           */
 
          *yycp = *cp;
          yycp++;
          cp++;
        }  /*  End while() for copying strings.  */

        /* At this point, we have seen a tick, but now we
         * determine whether it is really the end of the string
         * or an escape sequence e.g. 
         *   str = 'doesn''t parse' 
         * 9/30/97 --Keith  
         */

        if(*(cp+1) == '\'')   /* if next char after tick is a tick */
        {
          *tcp = *cp;
          tcp++;
          *yycp = *cp;
          yycp++;
          cp++;
          *tcp = *cp;
          tcp++;
          *yycp = *cp;
          yycp++;
          cp++;
        }
        else
          done = TRUE;
      } /* end while(not done) */
    }  /* End if() for copying character strings. */

    /* We need to track the number of opening and closing
     * parentheses "(" and ")" to implement Sale's algorithm. 
     */

    if(*cp == '(') 
      parens++;
    if(*cp == ')') 
      parens--;

    /* Examine the characters outside of matching parentheses.
     * Whats between matching parentheses doesn't matter. 
     */

    if(parens == 0)
    {
      if(*cp == ',') 
        commaseen = TRUE;
      if(*cp == '=') 
        equalseen = TRUE;

      if(*cp == ')')
      {
        char * lpp;  /* Last parens pointer, temporary. */

        /* Ok, lets peep ahead to the next non-whitespace
         * character and see if its a letter.  The for()
         * loop merely sets the pointer for look-ahead. 
         */

        for(lpp=cp+1;isspace((int) *lpp);lpp++);

        /* Since we have an opportunity, let's trap the
         * error condition of having isspace() pick up
         * a newline following the last paren.  */

        /*
         * if(*lpp == '\n')
         * {
         *   printf("Bad syntax, \" followed by \"\\n\"\n");
         *   exit(EXIT_FAILURE);
         * }
         * else
         */

        if(isalpha((int) *lpp)) letterseen = TRUE;

      }  /*  End if for ")".  */
    }    /*  End if for no parens. */

    *yycp = *cp;
    yycp++;
    *tcp = toupper(*cp);
    tcp++;
  }  /* End of for() loop. */

  /* Insert newline for statement separator.  This helps parse
   * situations where end and beginning of adjacent statements
   * are NAME tokens, i.e. NAME NAME, etc.  Also, newlines are
   * natural fortran statement separators.  
   */

  *yycp = '\n';
  *tcp = '\n';
    
  /* Insert an null character to mark the end of the string.  */

  *(yycp+1) = 0;
  *(tcp+1) = 0;

  /* Our new string is ready for lexing!  */

  strcpy(bufstruct->stmt, tempbuf);
  strcpy(line_buffer, tempbuf);
} 


/*****************************************************************************
 *                                                                           *
 * Check and see whether the next line continues the                         *
 * present line.  Marker for line continuation is any character              *
 * in column 6.                                                              *
 *****************************************************************************/

void
check_continued_lines(FILE * fp, char *current_line)
{
  int items, short_line;
  char next_line[100];
  int i,j ; /* rws indexes for chopping off end of line */
  int c_idx;

  c_idx = 0;

  /* Now we have to determine whether the statement
   * is continued on the next line by getting another 
   * line and examining column 6 for a continuation marker.
   */

  for(;;)
  {
    next_line[0] = '\0';
    items = fread(next_line, 1, 6, fp);

    /* If we are NOT at the end of file, reset the 
     * pointer to the start of the line so that 
     * the next fgets will grab the entire line.
     */

    if(items == 0)
      return;       /* End of file. */

    /* check for a newline within the first 6 characters
     * of the next line.  if one exists, it cannot be a
     * continued line.
     */
    short_line = 0;
    for(i=0;i<items;i++)
      if(next_line[i] == '\n') {
        short_line = 1;
        break;
      }

    /* check if the next line is a comment.  we are dealing with
     * this here in case there is a continuation after the comment(s).
     */
    if(next_line[0] == 'c' ||
       next_line[0] == 'C' ||
       next_line[0] == '*' ||
       next_line[0] == '!' ||
       next_line[0] == '\n')
    {
      if(fseek(fp, -items, SEEK_CUR) < 0 ) {
        printf("could not seek\n");
        perror("reason");
      }

      f2j_fgets(next_line, 100, fp);
      comment_buffer[c_idx++] = strdup(next_line);
      lineno++;

      continue;
    }

    if(short_line || (next_line[0] != ' '))
    {
      if(fseek(fp, -items, SEEK_CUR) < 0 ) {
        printf("could not seek\n");
        perror("reason");
      }
      return;
    }

    /* F77 spec says that any character other than a
     * blank or 0 signifies a continuation 
     */

    /* changed the following comparison since there
     * will be no null-termination of next_line after
     * calling fread().
     *
     *  if((strlen(next_line) < 6) ||
     *
     * instead, just compare the number of items read in
     * by fread().
     *
     * --kgs 4/18/00
     */

    if((items < 6) ||
       (next_line[5] == ' ') || 
       (next_line[5] == '0'))
    {
      /*  There is no continuation marker.  Reset the 
       * pointer to the start of the line, and return. 
       */

      if(lexdebug)
        printf("no continuation marker.\n");

      if( fseek(fp, -items, SEEK_CUR) < 0 ) {
        printf("could not seek\n");
        perror("reason");
      }
      return;
    }
    else
    {
      /* We have a continuation marker.  Get another line
       * and cat it to the previous.
       */

      if(lexdebug)
        printf("char 6, next_line: %c\n", next_line[5]);

      f2j_fgets(next_line, 100, fp);
      lineno++;

      if(lexdebug)
        printf("the next_line is [%s](%d)\n",next_line,
           (int)strlen(next_line));

      /* rws August 21, 2003
       * added next four lines
       */

      /*  truncate anything beyond 72 characters (72-6=66) */
      j = strlen(next_line);

      for(i=66;i<j;i++)
        next_line[i] = '\0';

      /* also truncate '!' style comments */
      truncate_bang_comments(next_line);

      /* rws August 21, 2003
       * next line no longer needed
       */
      /* next_line[strlen(next_line)-1] = '\0'; */

      if(current_line[strlen(current_line)-1] == '\n')
        current_line[strlen(current_line)-1] = '\0';

      strcat(current_line, next_line);
    }
  }
}   /* End of check_continued_lines().  */

/*****************************************************************************
 *                                                                           *
 * keyscan                                                                   *
 *                                                                           *
 * Step through the symbol tables to see if the current string can be        *
 * recognized as a token in the symbol table.  Also, set yytext here.        *
 *****************************************************************************/

int 
keyscan(register KWDTAB * tab, BUFFER * bufstruct)
{
  unsigned int tokenlength;
  char *scp, *yycp, swap_buf[BIGBUFF];
  scp = bufstruct->stmt;
  yycp = bufstruct->text;

  while(tab->kwd)
  {
    /* Get the stringlength of the token in the symbol table.
     * A better way to do this might be to include the length
     * of the keyword in the table instead of computing it
     * everytime. 
     */

    tokenlength = strlen(tab->kwd);	

    /* Try to match a substring of the  current string (scp).*/
    if(!strncmp(scp, tab->kwd, tokenlength))
    {
      if(tokenlength > YYTEXTLEN-1)
        fprintf(stderr,"Warning: going to write past yytext (%d)\n",
          tokenlength);

      strncpy(yytext, yycp, tokenlength);
      yycp += tokenlength;
      yytext[tokenlength] = '\0';

      strcpy(swap_buf, yycp);
      strcpy(bufstruct->text, swap_buf);

      /* Save the type or kind of relational operator
       * immediate reduction in the parser.  This
       * implementation is pretty lame, a hold over
       * from Levine's quick and dirty lexer.  
       */

      if((tab->ktok == ARITH_TYPE) || (tab->ktok == CHAR_TYPE))
        yylval.type = tab->klex;
      if(tab->ktok == RELOP)
        yylval.tok = tab->klex;

      /* Now set the string pointer to point at the first
       * character past the end of the string.
       */

      scp += tokenlength;

      strcpy(swap_buf, scp);
      strcpy(bufstruct->stmt, swap_buf);

      return tab->ktok;
    }
    tab++;     /* Check the next table entry.  */
  }            /* Close the while() loop.  */
  return 0;    /* Not a recognized token. */
}              /* Close keyscan(). */


/*****************************************************************************
 *                                                                           *
 * methodscan                                                                *
 *                                                                           *
 * Called after hash lookup indicates there is java method                   *
 * equivalent in the fortran source code.  Returns a pointer                 *
 * to the java string equivalent to the fortran source code.                 *
 * This is surely a hack.                                                    *
 *****************************************************************************/

METHODTAB *
methodscan(METHODTAB * tab, char * name)
{

  /*  The method translation table is initialized in
   *  the header block of this file.  We treat the table
   *  as a linear linked list by stepping through the
   *  array entries with the pointer `*tab'. Note that
   *  `NULL' last entry in the table shuts down the for()
   *  loop.  
   */

  while(tab->fortran_name != NULL) { 
    if(tab->fortran_name == NULL) 
      return NULL;

    if(!strcmp(tab->fortran_name,name)) {
      if(lexdebug)
        printf("java_name: %s\n", tab->java_method); 

      return tab; 
    }
    tab++;
  }                           /* Close for() loop.   */

  return NULL;                   /* Not in table.       */
}                             /* Close methodscan(). */


/*****************************************************************************
 *                                                                           *
 * name_scan                                                                 *
 *                                                                           *
 * Scan a card image for a named identifier.                                 *
 *****************************************************************************/

int
name_scan(BUFFER * bufstruct)
{
  char *ncp, *tcp, swap_buf[BIGBUFF];
  unsigned int tokenlength = 0;

  ncp = bufstruct->stmt;
  tcp = bufstruct->text;

  /*  Find the name. 
   * We checked the first character in yylex to make sure 
   * it was alphabetic. 
   */

  while(isalnum ((int) *ncp) || (*ncp == '_'))
  {
    ncp++;
    tokenlength++;
  }

  strncpy(yylval.lexeme, tcp, tokenlength);
  yylval.lexeme[tokenlength] = '\0';
  tcp += tokenlength;

  strcpy(swap_buf, tcp);
  strcpy(bufstruct->text, swap_buf);

  strcpy(swap_buf, ncp);
  strcpy(bufstruct->stmt, swap_buf);

  return NAME;
}				/*  Close name_scan().  */


/*****************************************************************************
 *                                                                           *
 * number_scan                                                               *
 *                                                                           *
 * Scan a card image for a numerical constant.                               *
 * Need to add code in here to change exp numbers                            *
 * to doubles, or at least to replace the instances                          *
 * of 'd' and 'D' with 'e'.                                                  *
 *                                                                           *
 * 9/30/97 -  Added fmt parameter which is a boolean                         *
 *    representing whether or not this number occurs                         *
 *    within a format statement.   If so, we only                            *
 *    want to return the integer part of the spec...                         *
 *    e.g., if our input is 2D36.8, just return 2                            *
 *  --Keith                                                                  *
 *****************************************************************************/

int
number_scan(BUFFER * bufstruct, int fmt, int toknum)
{
  char *ncp, *tcp, swap_buf[BIGBUFF];
  BUFFER tempbuf;
  int token;
  unsigned int tokenlength = 0;
  int type = INTEGER;  /* Default, in case we find nothing else. */

  ncp = bufstruct->stmt; /*  Number character pointer. */
  tcp = bufstruct->text;  /* Literal text character pointer. */

  if(lexdebug) {
    printf("here in number scan\n   buf.stmt = '%s'\n",bufstruct->stmt);
    printf("   buf.text = '%s'\n",bufstruct->text);
  }

  if(fmt || (toknum == 0)) {
    while(isdigit((int) *ncp)) {
      ncp++;
      tokenlength++;
    }
  }
  else {

    /*  Test and see whether it is a number (constant).
     *  If so, store the literal text in yytext.  These
     *  long logical expressions are probably not very 
     *  efficient, but they should be easy to read.
     */

    while(isdigit ((int) *ncp) ||
           *ncp == '.' ||
           *ncp == 'D' ||
           *ncp == 'd' ||
           *ncp == 'E' ||
           *ncp == 'e')
    {
      switch(*ncp)
      {
        case '.':

          /* If there is a dot, there may be a float or double or
           * exponential, or an integer followed by a keyword such as
           * .AND., .OR., etc.
           */

          strcpy(tempbuf.stmt, ncp);
          strcpy(tempbuf.text, tcp);
          token = keyscan(tab_toks, &tempbuf);

          if(token) 
            break; /* Leave the while() loop. */

          /* Else if there is no token returned, check for 
           * the usual double or exponential number. 
           */

          /* If the next character, i.e. *(ncp+1) is a digit, 
           * increment and continue while loop,
           * else get out of while loop.
           */

          if(isdigit((int) *(ncp + 1)))
          {
            ncp += 2;
            tokenlength += 2;
            type = FLOAT;	/* Case of `nn.dd...' */
            
            /* Control passes to back to 
             * while() loop; get another 
             * character. 
             */

            continue;	
          }
          else
          {
            ncp++;
            tokenlength++;
            type = FLOAT;	/* Case of `nn.' */

            /* Back to while() loop
             * for another character.*/

            continue; 
          }
        case 'E':
        case 'e':
        case 'D':
        case 'd':
          /* This exponential notation stuff works pretty good.
           * It will need to be modified to express the
           * number in exponential notation as an equivalent
           * double. 
           *
           *  First, take care of the case that looks like this:
           * 1.0e+1 or 1.0e-1. 
           */

          if(*(ncp + 1) == '+' || *(ncp + 1) == '-')
          {
            if(*ncp == 'e' || *ncp == 'E')
              type = E_EXPONENTIAL;
            else 
              type = D_EXPONENTIAL;
            
            ncp += 2;
            tokenlength += 2;

            continue;	/*  Loop again.  */
          }

          /*  Now take care of cases that look like this:  1.0e1.  */

          if(isdigit((int) *(ncp + 1)))
          {
            if(*ncp == 'e' || *ncp == 'E')
              type = E_EXPONENTIAL;
            else 
              type = D_EXPONENTIAL;

            ncp++;
            tokenlength++;

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
    }  /* Close while() loop. */
  }

  if(lexdebug) {
    printf("ok that was fun, ncp = '%s', tcp = '%s'",ncp,tcp);
    printf(" and tokenlength = %d\n",tokenlength);
  }

  strncpy(yylval.lexeme, tcp, tokenlength);
  yylval.lexeme[tokenlength] = '\0';

  if(lexdebug)
    printf("Number: %s\n", yytext);

  tcp += tokenlength;

  strcpy(swap_buf, tcp);
  strcpy(bufstruct->text, swap_buf);

  strcpy(swap_buf, ncp);
  strcpy(bufstruct->stmt, swap_buf);

  return type;
}				/* Close name_ident_scan().  */


/*****************************************************************************
 *                                                                           *
 * string_or_char_scan                                                       *
 *                                                                           *
 * Scan a string, making sure to check for escaped ticks in the text.        *
 *****************************************************************************/

int
string_or_char_scan(BUFFER * bufstruct)
{
  unsigned int tokenlength = 0;
  char *scp, *textcp, swap_buf[BIGBUFF];
  scp = bufstruct->stmt;
  textcp = bufstruct->text;

  /*  Test and see if there is a tic (`'') mark.  */

  if(*scp == '\'')
  {
    int done = FALSE;

    scp++;
    textcp++;

    if(lexdebug)
      printf("scp: %s\n", scp);

    /* Loop until we find another tick (') mark. */

    while(!done)
    {
      while(*scp != '\'')
      {
        scp++;
        tokenlength++;
      }

      /* Now we determine whether this is the final tick
       * or just an escape sequence to actually print a
       * tick.  If it's an escape, substitute a backslash
       * for the first tick.  that is,  '' -> \'
       * 9/30/97 --Keith 
       *
       * I'm not sure why I was using backslash here, but
       * it wasn't necessary, so changing it to just blank
       * the first tick.
       * 7/5/04 --keith
       */

      if( *(scp + 1) == '\'' )
      {
        *(textcp + tokenlength) = ' ';
        scp+=2;
        tokenlength+=2;
      }
      else
        done = TRUE;
    }

    if(tokenlength > YYTEXTLEN-1)
      fprintf(stderr,"Warning: going to write past yytext (%d)\n",
        tokenlength);

    strncpy(yytext, textcp, tokenlength);
    yytext[tokenlength] = '\0'; /* Terminate the string at tick. */
    strcpy(yylval.lexeme, yytext); 
    textcp += tokenlength;

    /* Now increment to get past the tic marks. */
    scp++;
    textcp++;

    strcpy(swap_buf, scp);
    strcpy(bufstruct->stmt, swap_buf);

    strcpy(swap_buf, textcp);
    strcpy(bufstruct->text, swap_buf);

    /* Reset the value; strlen does not include the value
     * of '\0' that terminates the string.  
     */

    tokenlength = strlen(yylval.lexeme);

    if(tokenlength == 1)
      return CHAR;
    else
      return STRING;
  }
  else
    return 0;
}				/* Close string_or_char_scan(). */

char *
f2j_fgets(char *s, int n, FILE *f)
{
  char *rv;
  int len;

  rv = fgets(s, n, f);

  if(rv == NULL) return NULL;

  len = strlen(s);

  switch(len) {
    case 0:
      s[0] = '\0';
      break;
    case 1:
      s[0] = '\n';
      s[1] = '\0';
      break;
    default:
      if( s[len-2] == '\r' ) {
        s[len -2] = '\n';
        s[len -1] = '\0';
      }
      break;
  }

  return s;
}

/*****************************************************************************
 *                                                                           *
 * tok2str                                                                   *
 *                                                                           *
 * Return the string representation of a token.  This function is used       *
 * primarily for debugging purposes.                                         *
 *****************************************************************************/

char *
tok2str(int tok)
{
  switch(tok) 
  {
    case PLUS:
      return("PLUS");
    case MINUS:
      return("MINUS");
    case OP:
      return("OP");
    case CP:
      return("CP");
    case STAR:
      return("STAR");
    case POW:
      return("POW");
    case DIV:
      return("DIV");
    case CAT:
      return("CAT");
    case CM:
      return("CM");
    case EQ:
      return("EQ");
    case COLON:
      return("COLON");
    case NL:
      return("NL");
    case NOT:
      return("NOT");
    case AND:
      return("AND");
    case OR:
      return("OR");
    case RELOP:
      return("RELOP");
    case EQV:
      return("EQV");
    case NEQV:
      return("NEQV");
    case NAME:
      return("NAME");
    case DOUBLE:
      return("DOUBLE");
    case INTEGER:
      return("INTEGER");
    case E_EXPONENTIAL:
      return("E_EXPONENTIAL");
    case D_EXPONENTIAL:
      return("D_EXPONENTIAL");
    case CONST_EXP:
      return("CONST_EXP");
    case TrUE:
      return("TrUE");
    case FaLSE:
      return("FaLSE");
    case ICON:
      return("ICON");
    case RCON:
      return("RCON");
    case LCON:
      return("LCON");
    case CCON:
      return("CCON");
    case FLOAT:
      return("FLOAT");
    case CHARACTER:
      return("CHARACTER");
    case LOGICAL:
      return("LOGICAL");
    case COMPLEX:
      return("COMPLEX");
    case NONE:
      return("NONE");
    case IF:
      return("IF");
    case THEN:
      return("THEN");
    case ELSE:
      return("ELSE");
    case ELSEIF:
      return("ELSEIF");
    case ENDIF:
      return("ENDIF");
    case ENDDO:
      return("ENDDO");
    case DO:
      return("DO");
    case GOTO:
      return("GOTO");
    case ASSIGN:
      return("ASSIGN");
    case TO:
      return("TO");
    case CONTINUE:
      return("CONTINUE");
    case STOP:
      return("STOP");
    case PAUSE:
      return("PAUSE");
    case RDWR:
      return("RDWR");
    case END:
      return("END");
    case STRING:
      return("STRING");
    case CHAR:
      return("CHAR");
    case OPEN:
      return("OPEN");
    case CLOSE:
      return("CLOSE");
    case BACKSPACE:
      return("BACKSPACE");
    case REWIND:
      return("REWIND");
    case ENDFILE:
      return("ENDFILE");
    case FORMAT:
      return("FORMAT");
    case PROGRAM:
      return("PROGRAM");
    case FUNCTION:
      return("FUNCTION");
    case SUBROUTINE:
      return("SUBROUTINE");
    case ENTRY:
      return("ENTRY");
    case CALL:
      return("CALL");
    case RETURN:
      return("RETURN");
    case ARITH_TYPE:
      return("ARITH_TYPE");
    case CHAR_TYPE:
      return("CHAR_TYPE");
    case DIMENSION:
      return("DIMENSION");
    case COMMON:
      return("COMMON");
    case EQUIVALENCE:
      return("EQUIVALENCE");
    case EXTERNAL:
      return("EXTERNAL");
    case PARAMETER:
      return("PARAMETER");
    case INTRINSIC:
      return("INTRINSIC");
    case IMPLICIT:
      return("IMPLICIT");
    case SAVE:
      return("SAVE");
    case DATA:
      return("DATA");
    case COMMENT:
      return("COMMENT");
    case WRITE:
      return("WRITE");
    case READ:
      return("READ");
    case EDIT_DESC:
      return("EDIT_DESC");
    case REPEAT:
      return("REPEAT");
    case BLOCK_COMMENT:
      return("BLOCK_COMMENT");
    case IOSPEC_IOSTAT:
      return("IOSPEC_IOSTAT");
    case IOSPEC_ERR:
      return("IOSPEC_ERR");
    case IOSPEC_END:
      return("IOSPEC_END");
    case IOSPEC_FILE:
      return("IOSPEC_FILE");
    case IOSPEC_STATUS:
      return("IOSPEC_STATUS");
    case IOSPEC_ACCESS:
      return("IOSPEC_ACCESS");
    case IOSPEC_FORM:
      return("IOSPEC_FORM");
    case IOSPEC_UNIT:
      return("IOSPEC_UNIT");
    case IOSPEC_FMT:
      return("IOSPEC_FMT");
    case IOSPEC_RECL:
      return("IOSPEC_RECL");
    case IOSPEC_REC:
      return("IOSPEC_REC");
    case IOSPEC_BLANK:
      return("IOSPEC_BLANK");
    case LOWER_THAN_COMMENT:
      return("LOWER_THAN_COMMENT");
    case NO_PROGRAM:
      return("NO_PROGRAM");
    case DUMMY:
      return("DUMMY");
    default:
    {
      static char asdf[20];

      sprintf(asdf,"Unknown token: %d\n",tok);
      return(asdf);
    }
  }
}
