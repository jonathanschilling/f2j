#  Top level makefile for the f2j system.

# $Author$
# $Date$
# $Source$
# $Revision$

all:	f2java javab packages

f2java:
	cd src; make f2java

javab:
	cd goto_trans; make

packages: javab
	cd netlib; make

# install:
#	mv src/f2java src/f2jas bin/

clean:
	cd docs; $(MAKE) clean
	cd goto_trans; $(MAKE) realclean
	cd src; $(MAKE) clean
	cd interface; $(MAKE) clean
	cd netlib; $(MAKE) clean
