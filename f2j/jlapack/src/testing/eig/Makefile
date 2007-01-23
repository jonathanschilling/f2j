.PHONY:	DUMMY
.SUFFIXES: .f .java

ROOT=../../..
include $(ROOT)/make.def

BLAS=$(ROOT)/$(BLAS_DIR)/$(BLAS_JAR)
LAPACK=$(ROOT)/$(LAPACK_DIR)/$(LAPACK_JAR)
MATGEN=$(ROOT)/$(MATGEN_DIR)/$(MATGEN_JAR)

XERBLAFLAGS= -c .:$(ROOT)/$(BLAS_OBJ) -p $(ERR_PACKAGE)
F2JFLAGS=-c .:$(ROOT)/$(BLAS_OBJ):$(ROOT)/$(LAPACK_OBJ):$(ROOT)/$(MATGEN_OBJ) -p $(EIGTEST_PACKAGE) -o $(OUTDIR) $(STATIC)

tester: $(BLAS) $(LAPACK) $(MATGEN) $(OUTDIR)/Eigtest.f2j util
	/bin/rm -f $(EIGTEST_JAR)
	cd $(OUTDIR); $(JAR) cvf ../$(EIGTEST_JAR) `find . -name "*.class"`
	$(JAR) uvf $(EIGTEST_JAR) `find org -name "*.class"`

nojar: $(BLAS) $(LAPACK) $(MATGEN) $(OUTDIR)/Eigtest.f2j util

$(OUTDIR)/Eigtest.f2j:	eigtest.f
	$(F2J) $(XERBLAFLAGS) xerbla.f > /dev/null
	$(F2J) $(F2JFLAGS) $< > /dev/null

$(BLAS):
	cd $(ROOT)/$(BLAS_DIR); $(MAKE)

$(LAPACK):
	cd $(ROOT)/$(LAPACK_DIR); $(MAKE)

$(MATGEN):
	cd $(ROOT)/$(MATGEN_DIR); $(MAKE)

util:
	cd $(ROOT)/$(UTIL_DIR); $(MAKE)

runtest:	tester *.in

srctest:
	$(MAKE) -f Makefile_javasrc runtest

verify: $(ROOT)/$(EIGTEST_IDX)
	cd $(OUTDIR); $(JAVA) $(MORE_MEM_FLAG) -classpath .:..:$(JUSTICE):$(BCEL):$(ROOT)/../$(UTIL_DIR)/$(UTIL_JAR):$(ROOT)/../$(ERR_DIR)/$(ERR_JAR):$(ROOT)/../$(MATGEN_DIR)/$(MATGEN_JAR):$(ROOT)/../$(BLAS_DIR)/$(BLAS_JAR):$(ROOT)/../$(LAPACK_DIR)/$(LAPACK_JAR) $(VERIFY) $(EIGTEST_PDIR)/*.class


*.in:   DUMMY
	$(JAVA) $(JFLAGS) -cp .:$(EIGTEST_JAR):$(MATGEN):$(ROOT)/$(BLAS_DIR)/$(BLAS_JAR):$(ROOT)/$(LAPACK_DIR)/$(LAPACK_JAR):$(ROOT)/$(UTIL_DIR)/$(UTIL_JAR) $(EIGTEST_PACKAGE).Dchkee < $@

clean:
	/bin/rm -rf *.java *.class *.f2j org $(JAVASRC_OUTDIR) $(OUTDIR) $(EIGTEST_JAR)
