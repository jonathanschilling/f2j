# $Author$
# $Date$
# $Source$
# Revision:$

CFLAGS= -g -ansi -Wall
CC=gcc
PURIFY=/usr/local/pure/purify/purify
YFLAGS= --debug --defines --verbose 
OBJS=f2jparse.tab.o f2jlex.o f2jmain.o  symtab.o  \
     codegen.o vcg_emitter.o typeassign.o opcode.o dlist.o typecheck.o
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
	$(PURIFY) ${CC} $(CFLAGS) -o $@ $(OBJS) $(LIBS)
	mv $@ ../bin


#  Fortran has gotos, java does not.  So we need to 
#  translate goto statements.
$(JAVAB):	
	cd $(GOTO_TRANS);make

f2jmain.o:  f2jmain.c f2j.h f2jparse.y
	$(CC) -c $(CFLAGS) f2jmain.c

f2jlex: f2jlex.o f2j.h
	$(CC) $(CFLAGS) -o f2jlex f2jlex.o

f2jlex.o: f2jlex.c
	$(CC) -c $(CFLAGS) f2jlex.c

dlist.o: dlist.c dlist.h
	$(CC) -c $(CFLAGS) dlist.c

f2jparse.tab.o: f2jparse.tab.c f2j.h
	$(CC) -c $(CFLAGS) f2jparse.tab.c

f2jparse.tab.c f2jparse.tab.h:  f2jparse.y
	$(YACC) $(YFLAGS) f2jparse.y

dlist.o: dlist.c
 
codegen.o:  codegen.c f2j.h

#  vcg = visualization of compiler graphs.
vcg_emitter.o:  vcg_emitter.c f2j.h

opcode.o: opcode.c f2j.h

symtab.o: symtab.c symtab.h

typeassign.o: typeassign.c f2j.h

typecheck.o: typecheck.c f2j.h

#initialize.o: initialize.c f2j.h

lexdebug:  f2jlex.c
	$(CC) $(FLAGS) -o lexdebug -DSTANDALONE  f2jlex.c 

clean:
	rm -f *.o *.class f2jparse.c f2jparse.tab.c f2jparse.tab.h \
              tmp f2j core a.out *.output *~  *.vcg

goto:
	cd $(GOTO_TRANS); $(MAKE) realclean

realclean: clean goto
	rm -f ../bin/f2java ../bin/puref2j
