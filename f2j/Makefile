#  Top level makefile for the f2j system.

# $Author$
# $Date$
# $Source$
# $Revision$

all:	f2java javab packages

f2java:
	cd src; $(MAKE)

javab:
	cd goto_trans; $(MAKE)

srcdist: srcdist_common
	zip -r f2jsrc.zip f2j

srcdist_tgz: srcdist_common
	tar cvf - f2j | gzip > f2jsrc.tgz

srcdist_common:
	mkdir -p f2j/bin
	mkdir -p f2j/src
	mkdir -p f2j/goto_trans
	cd goto_trans; cp README class.h symtab.c LICENSE byte.c dump.c symtab.h Makefile class.c main.c ../f2j/goto_trans
	cd src; cp codegen.c f2jlex.c initialize.h typeassign.c codegen.h f2jmain.c opcode.c typecheck.c constant_pool.c f2jmem.c opcodes.h vcg_emitter.c LICENSE constant_pool.h f2jmem.h optimize.c Makefile dlist.c f2jparse.y symtab.c class.c dlist.h globals.c symtab.h class.h f2j.h graph.h ../f2j/src
	cd f2j/src; $(MAKE) clean
	cd f2j/goto_trans; $(MAKE) clean
	cp srcdist_makefile f2j/Makefile
	cp srcdist_readme f2j/README

# install:
#	mv src/f2java src/f2jas bin/

clean:
	/bin/rm -rf f2j
	cd docs; $(MAKE) clean
	cd goto_trans; $(MAKE) realclean
	cd src; $(MAKE) clean
