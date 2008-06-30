.SUFFIXES: .f .java

ROOT=..
include $(ROOT)/make.def

F2JFLAGS=-c .:$(OUTDIR) -p $(ERR_PACKAGE) -o $(OUTDIR) $(F2J_JAVADOC_FLAG)

$(ERR_JAR): xerbla.f
	$(F2J) $(F2JFLAGS) $? > /dev/null
	cd $(OUTDIR); $(JAR) cvf ../$(ERR_JAR) `find . -name "*.class"`

javasrc: xerbla.f
	$(F2J) $(F2JFLAGS) $? > /dev/null
	/bin/rm -f `find . -name "*.class"`
	mkdir -p $(JAVASRC_OUTDIR)
	$(JAVAC) -classpath $(ROOT)/$(F2J_UTIL_JAR) -d $(JAVASRC_OUTDIR) $(OUTDIR)/$(ERR_PDIR)/*.java
	/bin/rm -f $(JAVASRC_OUTDIR)/$(ERR_PDIR)/*.old
	$(JAVAB) $(JAVASRC_OUTDIR)/$(ERR_PDIR)/*.class
	cd $(JAVASRC_OUTDIR); $(JAR) cvf ../$(ERR_JAR) `find . -name "*.class"`
	
verify: $(ERR_JAR)
	cd $(OUTDIR); $(JAVA) $(MORE_MEM_FLAG) -classpath .:$(JUSTICE):$(BCEL):$(ROOT)/$(F2J_UTIL_JAR):$(ERR_JAR) $(VERIFY) $(ERR_PDIR)/*.class


clean:
	/bin/rm -rf *.java *.class *.f2j $(OUTDIR) $(JAVASRC_OUTDIR) $(ERR_JAR)
