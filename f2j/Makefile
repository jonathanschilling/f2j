#  Top level makefile for the f2j system.

# $Author$
# $Date$
# $Source$
# $Revision$

all:	f2java javab

f2java:
	cd src; $(MAKE)

javab:
	cd goto_trans; $(MAKE)

srcdist: srcdist_common
	zip -r f2jsrc.zip f2j

srcdist_tgz: srcdist_common
	tar cvf - f2j | gzip > f2jsrc.tgz

srcdist_common:
	cd src; $(MAKE) f2jparse.tab.c
	mkdir -p f2j/bin
	mkdir -p f2j/src
	mkdir -p f2j/goto_trans
	cd goto_trans; cp *.[ch] README LICENSE Makefile ../f2j/goto_trans
	cd src; cp *.[chy] LICENSE Makefile ../f2j/src
	cp srcdist_makefile f2j/Makefile
	cp srcdist_readme f2j/README

clean:
	/bin/rm -rf f2j f2jsrc.tgz f2jsrc.zip
	cd goto_trans; $(MAKE) realclean
	cd src; $(MAKE) clean
