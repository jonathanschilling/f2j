.SUFFIXES: .f .java

ROOT=..
include $(ROOT)/make.def

ERR=$(ROOT)/$(ERR_DIR)/$(ERR_JAR)
BLAS=$(ROOT)/$(BLAS_DIR)/$(BLAS_JAR)
LAPACK=$(ROOT)/$(LAPACK_DIR)/$(LAPACK_JAR)
ARPACK_UTIL=$(ROOT)/$(ARPACK_UTIL_DIR)/$(ARPACK_UTIL_JAR)

F2JFLAGS=-c .:$(ROOT)/$(ERR_OBJ):$(ROOT)/$(BLAS_OBJ):$(ROOT)/$(LAPACK_OBJ):$(ROOT)/$(ARPACK_UTIL_OBJ) -p $(ARPACK_PACKAGE) -o $(OUTDIR) $(F2J_JAVADOC_FLAG) $(STATIC)

$(ARPACK_JAR): $(BLAS) $(LAPACK) $(ARPACK_UTIL) arpack.f
	$(F2J) $(F2JFLAGS) arpack.f > /dev/null
	/bin/rm -f $(ARPACK_JAR)
	cd $(OUTDIR); $(JAR) cvf ../$(ARPACK_JAR) `find . -name "*.class"`

$(BLAS):
	cd $(ROOT)/$(BLAS_DIR); $(MAKE)

$(LAPACK):
	cd $(ROOT)/$(LAPACK_DIR); $(MAKE)

$(ARPACK_UTIL):
	cd $(ROOT)/$(ARPACK_UTIL_DIR); $(MAKE)

javasrc: $(ROOT)/$(ERR_DIR)/$(ERR_JAR) $(ROOT)/$(BLAS_DIR)/$(BLAS_JAR) $(ROOT)/$(LAPACK_DIR)/$(LAPACK_JAR) $(ROOT)/$(ARPACK_UTIL_DIR)/$(ARPACK_UTIL_JAR) arpack.f
	$(F2J) $(F2JFLAGS) arpack.f > /dev/null
	/bin/rm -f `find . -name "*.class"`
	mkdir -p $(JAVASRC_OUTDIR)
	$(JAVAC) -classpath $(ROOT)/$(F2J_UTIL_JAR):$(ROOT)/$(ERR_OBJ):$(ROOT)/$(BLAS_OBJ):$(ROOT)/$(LAPACK_OBJ):$(ROOT)/$(ARPACK_UTIL_OBJ) -d $(JAVASRC_OUTDIR) $(OUTDIR)/$(ARPACK_PDIR)/*.java
	/bin/rm -f $(JAVASRC_OUTDIR)/$(ARPACK_PDIR)/*.old
	$(JAVAB) $(JAVASRC_OUTDIR)/$(ARPACK_PDIR)/*.class
	cd $(JAVASRC_OUTDIR); $(JAR) cvf ../$(ARPACK_JAR) `find . -name "*.class"`

verify: $(ARPACK_JAR)
	cd $(OUTDIR); $(JAVA) $(MORE_MEM_FLAG) -classpath .:..:$(ROOT)/$(F2J_UTIL_JAR):$(JUSTICE):$(BCEL):$(ROOT)/../$(UTIL_DIR)/$(UTIL_JAR):$(ERR):$(ROOT)/../$(ARPACK_UTIL_DIR)/$(ARPACK_UTIL_JAR):$(ROOT)/../$(BLAS_DIR)/$(BLAS_JAR):$(ROOT)/../$(LAPACK_DIR)/$(LAPACK_JAR) $(VERIFY) $(ARPACK_PDIR)/*.class


clean:
	/bin/rm -rf *.java *.class *.f2j $(JAVASRC_OUTDIR) $(OUTDIR) $(ARPACK_JAR)
