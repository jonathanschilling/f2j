# $Author$
# $Date$
# $Source$
# Revision:$

#CFLAGS= -g -ansi -Wall -pedantic
CFLAGS= -g -ansi -Wall -pedantic -DDEBUG_MEM

# overkill CFLAGS:
#CFLAGS= -g -ansi -Wall -pedantic -Wtraditional -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations -Wredundant-decls -Wnested-externs
CC=gcc
PURIFY=/usr/local/pure/purify/purify
PFLAGS=-cache-dir=/tmp
YFLAGS= --debug --defines --verbose 
OBJS=f2jparse.tab.o f2jlex.o f2jmain.o  symtab.o  \
     codegen.o vcg_emitter.o typeassign.o opcode.o dlist.o typecheck.o \
     optimize.o constant_pool.o class.o globals.o f2jmem.o
LIBS=
DEPENDS = f2jparse.y f2jlex.c f2j.h 
YACC=bison -t
JFILES = *.j
JAVAFILES = *.java
GOTO_TRANS=../goto_trans
JAVAB=../bin/javab

#  The main executable.
../bin/f2java: f2j.h f2jparse.y $(OBJS) $(JAVAB)
	${CC} $(CFLAGS) -o $@ $(OBJS) $(LIBS)

#  The purify version...
puref2j:	f2j.h f2jparse.y $(OBJS) $(JAVAB)
	$(PURIFY) $(PFLAGS) ${CC} $(CFLAGS) -o $@ $(OBJS) $(LIBS)
	mv $@ ../bin


#  Fortran has gotos, java does not.  So we need to 
#  translate goto statements.
$(JAVAB):	
	cd $(GOTO_TRANS);make

f2jlex: f2jlex.o initialize.h
	$(CC) $(CFLAGS) -o f2jlex f2jlex.o

f2jlex.o: f2jlex.c initialize.h

f2jparse.tab.c f2jparse.tab.h:  f2jparse.y class.h constant_pool.h
	$(YACC) $(YFLAGS) f2jparse.y

f2jmain.o:  f2jmain.c f2j.h f2jparse.tab.h

f2jmem.o:  f2jmem.c f2j.h

dlist.o: dlist.c
 
dlist.o: dlist.c dlist.h

f2jparse.tab.o: f2jparse.tab.c f2j.h

codegen.o:  codegen.c f2j.h class.h f2jparse.tab.h constant_pool.h codegen.h graph.h opcodes.h

globals.o:	globals.c f2j.h f2jparse.tab.h opcodes.h

constant_pool.o:	constant_pool.c constant_pool.h

#  vcg = visualization of compiler graphs.

vcg_emitter.o:  vcg_emitter.c f2j.h f2jparse.tab.h

opcode.o: opcode.c f2j.h f2jparse.tab.h

symtab.o: symtab.c symtab.h

typeassign.o: typeassign.c f2j.h f2jparse.tab.h

typecheck.o: typecheck.c f2j.h f2jparse.tab.h

optimize.o: optimize.c f2j.h f2jparse.tab.h

class.o:	class.c class.h constant_pool.h f2jparse.tab.h graph.h

lexdebug:  f2jlex.c
	$(CC) $(FLAGS) -o lexdebug -DSTANDALONE  f2jlex.c 

clean:
	rm -f *.o *.class f2jparse.c f2jparse.tab.c f2jparse.tab.h \
              tmp f2j core a.out *.output *~  *.vcg

goto:
	cd $(GOTO_TRANS); $(MAKE) realclean

realclean: clean goto
	rm -f ../bin/f2java ../bin/puref2j
