.SUFFIXES: .f .java

ROOT=../..
include $(ROOT)/make.def

F2JFLAGS=-c .:$(OUTDIR) -p $(ERR_PACKAGE) -o $(OUTDIR)

$(ROOT)/$(ERR_IDX): err.f
	$(F2J) $(F2JFLAGS) $? > /dev/null
	cd $(OUTDIR); $(JAR) cvf ../$(ERR_JAR) `find . -name "*.class"`

nojar: err.f
	$(F2J) $(F2JFLAGS) $? > /dev/null


javasrc:
	$(MAKE) -f Makefile_javasrc

clean:
	/bin/rm -rf *.java *.class *.f2j $(OUTDIR) $(JAVASRC_OUTDIR) $(ERR_JAR)
