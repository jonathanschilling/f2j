.SUFFIXES: .f .java

ROOT=../../..

include $(ROOT)/make.def

XERBLAFLAGS= -c .:$(ROOT)/$(BLAS_OBJ) -p $(ERR_PACKAGE)
F2JFLAGS=-c .:$(ROOT)/$(BLAS_OBJ) -p $(BLASTEST_PACKAGE) -o $(OUTDIR) $(STATIC)

tester: $(ROOT)/$(BLAS_DIR)/$(BLAS_JAR) $(ROOT)/$(BLAS3TEST_IDX) $(ROOT)/$(UTIL_DIR)/$(UTIL_JAR)
	/bin/rm -f $(BLAS3TEST_JAR)
	cd $(OUTDIR); $(JAR) cvf ../$(BLAS3TEST_JAR) `find . -name "*.class"`
	$(JAR) uvf $(BLAS3TEST_JAR) `find org -name "*.class"`

nojar: $(ROOT)/$(BLAS_DIR)/$(BLAS_JAR) $(ROOT)/$(BLAS3TEST_IDX) $(ROOT)/$(UTIL_DIR)/$(UTIL_JAR)

$(ROOT)/$(BLAS3TEST_IDX):	dblat3.f
	$(F2J) $(XERBLAFLAGS) xerbla.f > /dev/null
	$(F2J) $(F2JFLAGS) $< > /dev/null

$(ROOT)/$(BLAS_DIR)/$(BLAS_JAR):
	cd $(ROOT)/$(BLAS_DIR); $(MAKE)

$(ROOT)/$(UTIL_DIR)/$(UTIL_JAR):
	cd $(ROOT)/$(UTIL_DIR); $(MAKE)

runtest:	tester
	$(JAVA) $(JFLAGS) -cp .:$(BLAS3TEST_JAR):$(ROOT)/$(BLAS_DIR)/$(BLAS_JAR):$(ROOT)/$(UTIL_DIR)/$(UTIL_JAR):$(ROOT)/$(ERR_DIR)/$(ERR_JAR) $(BLASTEST_PACKAGE).Dblat3 < dblat3.in

srctest:
	$(MAKE) -f Makefile_javasrc

verify: $(ROOT)/$(BLAS3TEST_IDX)
	cd $(OUTDIR); $(JAVA) $(MORE_MEM_FLAG) -classpath .:..:$(JUSTICE):$(BCEL):$(ROOT)/../$(UTIL_DIR)/$(UTIL_JAR):$(ROOT)/../$(ERR_DIR)/$(ERR_JAR):$(ROOT)/../$(BLAS_DIR)/$(BLAS_JAR) $(VERIFY) $(BLASTEST_PDIR)/*.class

clean:
	/bin/rm -rf *.java *.class *.f2j org $(OUTDIR) $(JAVASRC_OUTDIR) $(BLAS3TEST_JAR)
