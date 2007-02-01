# $Author$
# $Date$
# $Source$
# Revision:$

.PHONY: clean

include make.def

OBJS=y.tab.o f2jlex.o f2jmain.o  symtab.o  \
     codegen.o vcg_emitter.o dlist.o typecheck.o \
     optimize.o globals.o f2jmem.o
F2J_LIBS= -L$(BYTE_DIR) -lbytecode $(LIBS)

.c.o:
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

#  The main executable.
f2java: f2j.h f2jparse.y $(OBJS) $(BYTE_DIR)/libbytecode.a
	$(CC) $(CFLAGS) $(INCLUDES) -o $@ $(OBJS) $(F2J_LIBS)

#  The purify version...
puref2j:	f2j.h f2jparse.y $(OBJS) $(JAVAB)
	$(PURIFY) $(PFLAGS) $(CC) $(CFLAGS) $(INCLUDES) -o $@ $(OBJS) $(F2J_LIBS)

install: f2java
	install -d -m 755 $(F2J_BINDIR)
	install -m 755 f2java $(F2J_BINDIR)

lexdebug:  f2jlex.c
	$(CC) $(CFLAGS) $(INCLUDES) -o lexdebug -DSTANDALONE  f2jlex.c 

lint:
	lint codegen.c dlist.c f2jlex.c\
 f2jmain.c f2jmem.c y.tab.c globals.c optimize.c\
 symtab.c typecheck.c vcg_emitter.c

y.tab.c y.tab.h:  f2jparse.y 
	$(YACC) $(YFLAGS) f2jparse.y

y.tab.o: y.tab.c f2j.h symtab.h dlist.h \
 opcodes.h  f2jmem.h

f2jlex.o: f2jlex.c initialize.h f2j.h symtab.h dlist.h \
 opcodes.h y.tab.h f2jmem.h 

f2jmain.o: f2jmain.c f2j.h symtab.h dlist.h opcodes.h \
 y.tab.h  f2jmem.h 

symtab.o: symtab.c f2j.h symtab.h dlist.h opcodes.h \
 f2jmem.h 

codegen.o: codegen.c codegen.h f2j.h symtab.h dlist.h \
 opcodes.h y.tab.h  f2jmem.h

vcg_emitter.o: vcg_emitter.c f2j.h symtab.h dlist.h \
 opcodes.h y.tab.h

dlist.o: dlist.c dlist.h f2j.h symtab.h opcodes.h \
 f2jmem.h 

typecheck.o: typecheck.c f2j.h symtab.h dlist.h \
 opcodes.h y.tab.h f2jmem.h 

optimize.o: optimize.c f2j.h symtab.h dlist.h \
 opcodes.h codegen.h y.tab.h  f2jmem.h

globals.o: globals.c f2j.h symtab.h dlist.h opcodes.h \
 codegen.h y.tab.h

f2jmem.o: f2jmem.c f2jmem.h f2j.h symtab.h dlist.h \
 opcodes.h

clean:
	rm -f *.o *.class f2jparse.c y.tab.c y.tab.h \
	tmp f2j f2java core a.out *.output *~  *.vcg
	cd test; $(MAKE) clean

realclean: clean
	rm -f ../bin/f2java ../bin/puref2j 
