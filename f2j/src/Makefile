HOME=/shag/homes/doolin
CFLAGS= -g -ansi #-p
CC=gcc
PURIFY=/usr/local/pure/purify/purify
YFLAGS= --debug --defines --verbose 
OBJS=f2jparse.tab.o f2jlex.o f2jmain.o  symtab.o  codegen.o vcg_emitter.o typeassign.o opcode.o
DEPENDS = f2jparse.y f2jlex.c f2j.h 
YACC=bison
JFILES = *.j
JAVAFILES = *.java



f2java: f2j.h f2jparse.y $(OBJS) 
	${CC} $(CFLAGS) -o $@ $(OBJS) 

f2jmain.o:  f2jmain.c f2j.h f2jparse.y
	$(CC) -c $(CFLAGS) f2jmain.c

f2jlex: f2jlex.o f2j.h
	$(CC) $(FLAGS) -o f2jlex f2jlex.o

f2jlex.o: f2jlex.c
	$(CC) -c $(FLAGS) f2jlex.c

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

#initialize.o: initialize.c f2j.h

lexdebug:  f2jlex.c
	$(CC) $(FLAGS) -o lexdebug -DSTANDALONE  f2jlex.c 

#%.o:	%.c
#	${CC} -c ${CFLAGS} $<

clean:
	rm -f *.o *.class f2jparse.c f2jparse.tab.c f2jparse.h tmp f2j core a.out *.output *~
