# $Author$
# $Date$
# $Source$
# Revision:$

HOME=/shag/homes/doolin
CFLAGS= -g -ansi #-p
CC=gcc
PURIFY=/usr/local/pure/purify/purify
YFLAGS= --debug --defines --verbose 
OBJS=f2jparse.tab.o f2jlex.o f2jmain.o  symtab.o  codegen.o vcg_emitter.o typeassign.o opcode.o list.o typecheck.o
LIBS=
DEPENDS = f2jparse.y f2jlex.c f2j.h 
YACC=bison -t
JFILES = *.j
JAVAFILES = *.java



f2java: f2j.h f2jparse.y $(OBJS)
	${CC} $(CFLAGS) -o $@ $(OBJS) $(LIBS)

puref2j:	f2j.h f2jparse.y $(OBJS)
	/usr/local/pure/purify/purify  ${CC} $(CFLAGS) -o $@ $(OBJS) $(LIBS)

javab:	
	cd goto_trans;make

f2jmain.o:  f2jmain.c f2j.h f2jparse.y
	$(CC) -c $(CFLAGS) f2jmain.c

f2jlex: f2jlex.o f2j.h
	$(CC) $(CFLAGS) -o f2jlex f2jlex.o

f2jlex.o: f2jlex.c
	$(CC) -c $(CFLAGS) f2jlex.c

list.o: list.c list.h
	$(CC) -c $(CFLAGS) list.c

f2jparse.tab.o: f2jparse.tab.c f2j.h
	$(CC) -c $(CFLAGS) f2jparse.tab.c

f2jparse.tab.c f2jparse.tab.h:  f2jparse.y
	$(YACC) $(YFLAGS) f2jparse.y

dlist.o: dlist.c
 
codegen.o:  codegen.c f2j.h

vcg_emitter.o:  vcg_emitter.c f2j.h

opcode.o: opcode.c f2j.h

symtab.o: symtab.c symtab.h

typeassign.o: typeassign.c f2j.h

typecheck.o: typecheck.c f2j.h

#initialize.o: initialize.c f2j.h

lexdebug:  f2jlex.c
	$(CC) $(FLAGS) -o lexdebug -DSTANDALONE  f2jlex.c 

#%.o:	%.c
#	${CC} -c ${CFLAGS} $<

clean:
	rm -f *.o *.class f2jparse.c f2jparse.tab.c f2jparse.tab.h \
              tmp f2j core a.out *.output *~  *.vcg

realclean: clean
	rm -f f2java 
