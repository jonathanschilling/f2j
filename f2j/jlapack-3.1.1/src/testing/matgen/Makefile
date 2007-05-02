.SUFFIXES: .f .java

ROOT=../../..

include $(ROOT)/make.def

BLAS=$(ROOT)/$(BLAS_DIR)/$(BLAS_JAR)
LAPACK=$(ROOT)/$(LAPACK_DIR)/$(LAPACK_JAR)

F2JFLAGS=-c .:$(ROOT)/$(BLAS_OBJ):$(ROOT)/$(ERR_OBJ):$(ROOT)/$(LAPACK_OBJ) -p $(MATGEN_PACKAGE) -o $(OUTDIR) $(STATIC)

tester: $(BLAS) $(LAPACK) $(ROOT)/$(MATGEN_IDX)
	/bin/rm -f $(MATGEN_JAR)
	cd $(OUTDIR); $(JAR) cvf ../$(MATGEN_JAR) `find . -name "*.class"`

nojar: $(BLAS) $(LAPACK) $(ROOT)/$(MATGEN_IDX)

javasrc:
	$(MAKE) -f Makefile_javasrc

$(ROOT)/$(MATGEN_IDX):	matgen.f
	$(F2J) $(F2JFLAGS) $< > /dev/null

$(BLAS):
	cd $(ROOT)/$(BLAS_DIR); $(MAKE)

$(LAPACK):
	cd $(ROOT)/$(LAPACK_DIR); $(MAKE)

verify: $(ROOT)/$(MATGEN_IDX)
	cd $(OUTDIR); $(JAVA) $(MORE_MEM_FLAG) -classpath .:$(JUSTICE):$(BCEL):$(ROOT)/../$(UTIL_DIR)/$(UTIL_JAR):$(ROOT)/../$(ERR_DIR)/$(ERR_JAR):$(ROOT)/../$(BLAS_DIR)/$(BLAS_JAR):$(ROOT)/../$(LAPACK_DIR)/$(LAPACK_JAR) $(VERIFY) $(MATGEN_PDIR)/*.class

clean:
	/bin/rm -rf *.java *.class *.f2j $(JAVASRC_OUTDIR) $(OUTDIR) $(MATGEN_JAR)
