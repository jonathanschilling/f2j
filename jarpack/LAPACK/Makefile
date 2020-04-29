.SUFFIXES: .f .java

ROOT=..

include $(ROOT)/make.def

F2JFLAGS=-c .:$(OUTDIR):$(ROOT)/$(ERR_OBJ):$(ROOT)/$(BLAS_OBJ) -p $(LAPACK_PACKAGE) -o $(OUTDIR) -s $(F2J_JAVADOC_FLAG) $(STATIC)

$(LAPACK_JAR): $(ROOT)/$(BLAS_DIR)/$(BLAS_JAR) $(ROOT)/$(ERR_DIR)/$(ERR_JAR) lapack.f
	$(F2J) $(F2JFLAGS) lapack.f > /dev/null
	cd $(OUTDIR); $(JAR) cvf ../$(LAPACK_JAR) `find . -name "*.class"`

$(ROOT)/$(BLAS_DIR)/$(BLAS_JAR):
	cd $(ROOT)/$(BLAS_DIR);$(MAKE)

$(ROOT)/$(ERR_DIR)/$(ERR_JAR):
	cd $(ROOT)/$(ERR_DIR);$(MAKE)

javasrc: $(ROOT)/$(ERR_DIR)/$(ERR_JAR) $(ROOT)/$(BLAS_DIR)/$(BLAS_JAR) lapack.f
	$(F2J) $(F2JFLAGS) lapack.f > /dev/null
	/bin/rm -f `find . -name "*.class"`
	mkdir -p $(JAVASRC_OUTDIR)
	$(JAVAC) -classpath $(ROOT)/$(F2J_UTIL_JAR):$(ROOT)/$(ERR_OBJ):$(ROOT)/$(BLAS_OBJ) -d $(JAVASRC_OUTDIR) $(OUTDIR)/$(LAPACK_PDIR)/*.java
	/bin/rm -f $(JAVASRC_OUTDIR)/$(LAPACK_PDIR)/*.old
	$(JAVAB) $(JAVASRC_OUTDIR)/$(LAPACK_PDIR)/*.class
	cd $(JAVASRC_OUTDIR); $(JAR) cvf ../$(LAPACK_JAR) `find . -name "*.class"`

verify: $(LAPACK_JAR)
	cd $(OUTDIR); $(JAVA) $(MORE_MEM_FLAG) -classpath .:$(JUSTICE):$(BCEL):$(ROOT)/$(F2J_UTIL_JAR):$(ROOT)/../$(ERR_DIR)/$(ERR_JAR):$(ROOT)/../$(BLAS_DIR)/$(BLAS_JAR) $(VERIFY) $(LAPACK_PDIR)/*.class

clean:
	/bin/rm -rf *.java *.class *.f2j $(LAPACK_JAR) $(OUTDIR) $(JAVASRC_OUTDIR)
