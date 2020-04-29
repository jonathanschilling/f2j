.SUFFIXES: .f .java

ROOT=..
include $(ROOT)/make.def

F2JFLAGS=-c .:$(OUTDIR) -p $(ARPACK_UTIL_PACKAGE) -o $(OUTDIR) $(F2J_JAVADOC_FLAG)

$(ARPACK_UTIL_JAR): util.f
	$(F2J) $(F2JFLAGS) $? > /dev/null
	cd $(OUTDIR); $(JAR) cvf ../$(ARPACK_UTIL_JAR) `find . -name "*.class"`

javasrc: $(ROOT)/$(ERR_DIR)/$(ERR_JAR) $(ROOT)/$(BLAS_DIR)/$(BLAS_JAR) $(ROOT)/$(LAPACK_DIR)/$(LAPACK_JAR) util.f
	$(F2J) $(F2JFLAGS) util.f > /dev/null
	/bin/rm -f `find . -name "*.class"`
	mkdir -p $(JAVASRC_OUTDIR)
	$(JAVAC) -classpath $(ROOT)/$(F2J_UTIL_JAR):$(ROOT)/$(ERR_OBJ):$(ROOT)/$(BLAS_OBJ):$(ROOT)/$(LAPACK_OBJ) -d $(JAVASRC_OUTDIR) $(OUTDIR)/$(ARPACK_UTIL_PDIR)/*.java
	/bin/rm -f $(JAVASRC_OUTDIR)/$(ARPACK_UTIL_PDIR)/*.old
	$(JAVAB) $(JAVASRC_OUTDIR)/$(ARPACK_UTIL_PDIR)/*.class
	cd $(JAVASRC_OUTDIR); $(JAR) cvf ../$(ARPACK_UTIL_JAR) `find . -name "*.class"`

verify: $(ARPACK_UTIL_JAR)
	cd $(OUTDIR); $(JAVA) $(MORE_MEM_FLAG) -classpath .:$(JUSTICE):$(BCEL):$(ROOT)/$(F2J_UTIL_JAR):$(ARPACK_UTIL_JAR) $(VERIFY) $(ARPACK_UTIL_PDIR)/*.class

clean:
	/bin/rm -rf *.java *.class *.f2j $(OUTDIR) $(JAVASRC_OUTDIR) $(ARPACK_UTIL_JAR)
