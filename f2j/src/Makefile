# $Author$
# $Date$
# $Source$
# Revision:$

.PHONY: clean goto

BYTE_DIR = ../libbytecode

# defining DEBUG_MEM includes some code that will
# trash any freed memory, thus helping to expose
# some memory-related bugs in f2j.
#

CFLAGS= -g -Wall -DDEBUG_MEM -I $(BYTE_DIR)

# overkill CFLAGS:
#CFLAGS= -g -ansi -Wall -pedantic -Wtraditional -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations -Wredundant-decls -Wnested-externs
CC=gcc
PURIFY=/usr/local/rational/releases/PurifyPlusFamily.2002a.06.00/sun4_solaris2/bin/purify 
PFLAGS=-cache-dir=/tmp
YFLAGS= --debug --defines --verbose 
SRC=f2jparse.tab.c f2jlex.c f2jmain.c  symtab.c  \
     codegen.c vcg_emitter.c dlist.c typecheck.c \
     optimize.c globals.c f2jmem.c
OBJS=$(SRC:.c=.o)
LIBS=-lbytecode
YACC=bison -t
GOTO_TRANS=../goto_trans
JAVAB=../bin/javab

#  The main executable.
../bin/f2java: bytecode_library f2j.h f2jparse.y $(OBJS) $(JAVAB) 
	${CC} $(CFLAGS) -o $@ $(OBJS) -L $(BYTE_DIR) $(LIBS)

bytecode_library:
	cd $(BYTE_DIR); $(MAKE)
         
#  The purify version...
puref2j:	f2j.h f2jparse.y $(OBJS) $(JAVAB) bytecode_library
	$(PURIFY) $(PFLAGS) ${CC} $(CFLAGS) -o $@ $(OBJS) -L $(BYTE_DIR) $(LIBS)
	mv $@ ../bin

lexdebug:  f2jlex.c
	$(CC) $(FLAGS) -o lexdebug -DSTANDALONE  f2jlex.c 

lint:
	lint codegen.c dlist.c f2jlex.c\
 f2jmain.c f2jmem.c f2jparse.tab.c globals.c optimize.c\
 symtab.c typecheck.c vcg_emitter.c

#  Fortran has gotos, java does not.  So we need to 
#  translate goto statements.
$(JAVAB):	
	cd $(GOTO_TRANS); $(MAKE)

f2jparse.tab.c f2jparse.tab.h:  f2jparse.y 
	$(YACC) $(YFLAGS) f2jparse.y

f2jparse.tab.o: f2jparse.tab.c f2j.h symtab.h dlist.h \
 opcodes.h  f2jmem.h

f2jlex.o: f2jlex.c initialize.h f2j.h symtab.h dlist.h \
 opcodes.h f2jparse.tab.h f2jmem.h 

f2jmain.o: f2jmain.c f2j.h symtab.h dlist.h opcodes.h \
 f2jparse.tab.h  f2jmem.h 

symtab.o: symtab.c f2j.h symtab.h dlist.h opcodes.h \
 f2jmem.h 

codegen.o: codegen.c codegen.h f2j.h symtab.h dlist.h \
 opcodes.h f2jparse.tab.h  f2jmem.h

vcg_emitter.o: vcg_emitter.c f2j.h symtab.h dlist.h \
 opcodes.h f2jparse.tab.h

dlist.o: dlist.c dlist.h f2j.h symtab.h opcodes.h \
 f2jmem.h 

typecheck.o: typecheck.c f2j.h symtab.h dlist.h \
 opcodes.h f2jparse.tab.h f2jmem.h 

optimize.o: optimize.c f2j.h symtab.h dlist.h \
 opcodes.h codegen.h f2jparse.tab.h  f2jmem.h

globals.o: globals.c f2j.h symtab.h dlist.h opcodes.h \
 f2jparse.tab.h

f2jmem.o: f2jmem.c f2jmem.h f2j.h symtab.h dlist.h \
 opcodes.h

clean:
	rm -f *.o *.class f2jparse.c f2jparse.tab.c f2jparse.tab.h \
	tmp f2j core a.out *.output *~  *.vcg $(BYTE_DIR)/*.o \
	$(BYTE_DIR)/*.a

goto:
	cd $(GOTO_TRANS); $(MAKE) realclean

realclean: clean goto
	rm -f ../bin/f2java ../bin/puref2j 
