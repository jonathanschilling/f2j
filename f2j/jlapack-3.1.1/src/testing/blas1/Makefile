.SUFFIXES: .f .java

ROOT=../../..

include $(ROOT)/make.def

F2JFLAGS=-c .:$(ROOT)/$(BLAS_OBJ) -p $(BLASTEST_PACKAGE) -o $(OUTDIR) $(STATIC)

tester: $(ROOT)/$(BLAS_DIR)/$(BLAS_JAR) $(ROOT)/$(BLAS1TEST_IDX) $(ROOT)/$(UTIL_DIR)/$(UTIL_JAR)
	/bin/rm -f $(BLAS1TEST_JAR)
	cd $(OUTDIR); $(JAR) cvf ../$(BLAS1TEST_JAR) `find . -name "*.class"`

nojar: $(ROOT)/$(BLAS_DIR)/$(BLAS_JAR) $(ROOT)/$(BLAS1TEST_IDX) $(ROOT)/$(UTIL_DIR)/$(UTIL_JAR)

$(ROOT)/$(BLAS1TEST_IDX):	dblat1.f
	$(F2J) $(F2JFLAGS) $< > /dev/null

$(ROOT)/$(BLAS_DIR)/$(BLAS_JAR):
	cd $(ROOT)/$(BLAS_DIR); $(MAKE)

$(ROOT)/$(UTIL_DIR)/$(UTIL_JAR):
	cd $(ROOT)/$(UTIL_DIR); $(MAKE)

runtest:	tester
	$(JAVA) $(JFLAGS) -cp .:$(BLAS1TEST_JAR):$(ROOT)/$(BLAS_DIR)/$(BLAS_JAR):$(ROOT)/$(UTIL_DIR)/$(UTIL_JAR):$(ROOT)/$(ERR_DIR)/$(ERR_JAR) $(BLASTEST_PACKAGE).Dblat1

srctest:
	$(MAKE) -f Makefile_javasrc runtest

verify: $(ROOT)/$(BLAS1TEST_IDX)
	cd $(OUTDIR); $(JAVA) $(MORE_MEM_FLAG) -classpath .:$(JUSTICE):$(BCEL):$(ROOT)/../$(UTIL_DIR)/$(UTIL_JAR):$(ROOT)/../$(ERR_DIR)/$(ERR_JAR):$(ROOT)/../$(BLAS_DIR)/$(BLAS_JAR) $(VERIFY) $(BLASTEST_PDIR)/*.class


clean:
	/bin/rm -rf *.java *.class *.f2j $(OUTDIR) $(JAVASRC_OUTDIR) $(BLAS1TEST_JAR)
