# $Author$
# $Date$
# $Source$
# Revision:$

.PHONY: depend clean goto

#CFLAGS= -g -ansi -Wall -pedantic
CFLAGS= -g -ansi -Wall -pedantic -DDEBUG_MEM

# overkill CFLAGS:
#CFLAGS= -g -ansi -Wall -pedantic -Wtraditional -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations -Wredundant-decls -Wnested-externs
CC=gcc
PURIFY=/usr/local/pure/purify/purify
PFLAGS=-cache-dir=/tmp
YFLAGS= --debug --defines --verbose 
SRC=f2jparse.tab.c f2jlex.c f2jmain.c  symtab.c  \
     codegen.c vcg_emitter.c typeassign.c opcode.c dlist.c typecheck.c \
     optimize.c constant_pool.c class.c globals.c f2jmem.c
OBJS=$(SRC:.c=.o)
LIBS=
YACC=bison -t
GOTO_TRANS=../goto_trans
JAVAB=../bin/javab

#  The main executable.
../bin/f2java: f2j.h f2jparse.y $(OBJS) $(JAVAB)
	${CC} $(CFLAGS) -o $@ $(OBJS) $(LIBS)

#  The purify version...
puref2j:	f2j.h f2jparse.y $(OBJS) $(JAVAB)
	$(PURIFY) $(PFLAGS) ${CC} $(CFLAGS) -o $@ $(OBJS) $(LIBS)
	mv $@ ../bin

lexdebug:  f2jlex.c
	$(CC) $(FLAGS) -o lexdebug -DSTANDALONE  f2jlex.c 

#  Fortran has gotos, java does not.  So we need to 
#  translate goto statements.
$(JAVAB):	
	cd $(GOTO_TRANS);make

f2jparse.tab.c f2jparse.tab.h:  f2jparse.y class.h constant_pool.h
	$(YACC) $(YFLAGS) f2jparse.y

depend:	f2j.dep
f2j.dep:
	rm -f f2j.dep
	$(CC) -M $(SRC) > f2j.dep

include f2j.dep

clean:
	rm -f *.o *.class f2jparse.c f2jparse.tab.c f2jparse.tab.h \
              tmp f2j core a.out *.output *~  *.vcg

goto:
	cd $(GOTO_TRANS); $(MAKE) realclean

realclean: clean goto
	rm -f ../bin/f2java ../bin/puref2j f2j.dep
