.SUFFIXES: .f .java

ROOT=..
include $(ROOT)/make.def

F2JFLAGS=-c .:$(OUTDIR):$(ROOT)/$(ERR_OBJ) -p $(BLAS_PACKAGE) -o $(OUTDIR) -s $(F2J_JAVADOC_FLAG) $(STATIC)

$(BLAS_JAR): $(ROOT)/$(ERR_DIR)/$(ERR_JAR) blas.f
	$(F2J) $(F2JFLAGS) blas.f > /dev/null
	cd $(OUTDIR); $(JAR) cvf ../$(BLAS_JAR) `find . -name "*.class"`

$(ROOT)/$(ERR_DIR)/$(ERR_JAR):
	cd $(ROOT)/$(ERR_DIR);$(MAKE)

javasrc: $(ROOT)/$(ERR_DIR)/$(ERR_JAR) blas.f
	$(F2J) $(F2JFLAGS) blas.f > /dev/null
	/bin/rm -f `find . -name "*.class"`
	mkdir -p $(JAVASRC_OUTDIR)
	$(JAVAC) -classpath $(ROOT)/$(F2J_UTIL_JAR):$(ROOT)/$(ERR_OBJ) -d $(JAVASRC_OUTDIR) $(OUTDIR)/$(BLAS_PDIR)/*.java
	/bin/rm -f $(JAVASRC_OUTDIR)/$(BLAS_PDIR)/*.old
	$(JAVAB) $(JAVASRC_OUTDIR)/$(BLAS_PDIR)/*.class
	cd $(JAVASRC_OUTDIR); $(JAR) cvf ../$(BLAS_JAR) `find . -name "*.class"`

verify: $(BLAS_JAR)
	cd $(OUTDIR); $(JAVA) $(MORE_MEM_FLAG) -classpath .:$(JUSTICE):$(BCEL):$(ROOT)/$(F2J_UTIL_JAR):$(ROOT)/../$(ERR_DIR)/$(ERR_JAR):$(BLAS_JAR) $(VERIFY) $(BLAS_PDIR)/*.class

clean:
	/bin/rm -rf *.java *.class *.f2j $(BLAS_JAR) $(OUTDIR) $(JAVASRC_OUTDIR)
