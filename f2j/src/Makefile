# $Author$
# $Date$
# $Source$
# Revision:$

.PHONY: clean goto

# defining DEBUG_MEM includes some code that will
# trash any freed memory, thus helping to expose
# some memory-related bugs in f2j.
#
CFLAGS= -g -ansi -Wall -pedantic -DDEBUG_MEM
#CFLAGS= -g -ansi -Wall -pedantic -DDEBUG_MEM -DF2J_ARRAYS_STATIC

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
	cd $(GOTO_TRANS); $(MAKE)

f2jparse.tab.c f2jparse.tab.h:  f2jparse.y class.h constant_pool.h
	$(YACC) $(YFLAGS) f2jparse.y

f2jparse.tab.o: f2jparse.tab.c f2j.h symtab.h dlist.h class.h graph.h \
 opcodes.h constant_pool.h f2jmem.h

f2jlex.o: f2jlex.c initialize.h f2j.h symtab.h dlist.h class.h graph.h \
 opcodes.h f2jparse.tab.h f2jmem.h constant_pool.h

f2jmain.o: f2jmain.c f2j.h symtab.h dlist.h class.h graph.h opcodes.h \
 f2jparse.tab.h constant_pool.h f2jmem.h

symtab.o: symtab.c f2j.h symtab.h dlist.h class.h graph.h opcodes.h \
 f2jmem.h constant_pool.h

codegen.o: codegen.c codegen.h f2j.h symtab.h dlist.h class.h graph.h \
 opcodes.h f2jparse.tab.h constant_pool.h f2jmem.h

vcg_emitter.o: vcg_emitter.c f2j.h symtab.h dlist.h class.h graph.h \
 opcodes.h f2jparse.tab.h

typeassign.o: typeassign.c f2j.h symtab.h dlist.h class.h graph.h \
 opcodes.h f2jparse.tab.h

opcode.o: opcode.c f2j.h symtab.h dlist.h class.h graph.h opcodes.h \
 f2jparse.tab.h

dlist.o: dlist.c dlist.h f2j.h symtab.h class.h graph.h opcodes.h \
 f2jmem.h constant_pool.h

typecheck.o: typecheck.c f2j.h symtab.h dlist.h class.h graph.h \
 opcodes.h f2jparse.tab.h f2jmem.h constant_pool.h

optimize.o: optimize.c f2j.h symtab.h dlist.h class.h graph.h \
 opcodes.h codegen.h f2jparse.tab.h constant_pool.h f2jmem.h

constant_pool.o: constant_pool.c constant_pool.h class.h dlist.h f2j.h \
 symtab.h graph.h opcodes.h f2jparse.tab.h f2jmem.h

class.o: class.c class.h dlist.h codegen.h f2j.h symtab.h graph.h \
 opcodes.h f2jparse.tab.h constant_pool.h f2jmem.h

globals.o: globals.c f2j.h symtab.h dlist.h class.h graph.h opcodes.h \
 f2jparse.tab.h

f2jmem.o: f2jmem.c f2jmem.h f2j.h symtab.h dlist.h class.h graph.h \
 opcodes.h constant_pool.h

clean:
	rm -f *.o *.class f2jparse.c f2jparse.tab.c f2jparse.tab.h \
              tmp f2j core a.out *.output *~  *.vcg

goto:
	cd $(GOTO_TRANS); $(MAKE) realclean

realclean: clean goto
	rm -f ../bin/f2java ../bin/puref2j
