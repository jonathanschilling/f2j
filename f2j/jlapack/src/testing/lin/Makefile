.SUFFIXES: .f .java

ROOT=../../..
include $(ROOT)/make.def

BLAS=$(ROOT)/$(BLAS_DIR)/$(BLAS_JAR)
LAPACK=$(ROOT)/$(LAPACK_DIR)/$(LAPACK_JAR)
MATGEN=$(ROOT)/$(MATGEN_DIR)/$(MATGEN_JAR)

XERBLAFLAGS= -c .:$(ROOT)/$(BLAS_OBJ) -p $(ERR_PACKAGE)
F2JFLAGS=-c .:$(ROOT)/$(BLAS_OBJ):$(ROOT)/$(LAPACK_OBJ):$(ROOT)/$(MATGEN_OBJ) -p $(LINTEST_PACKAGE) -o $(OUTDIR) $(STATIC)

tester: $(BLAS) $(LAPACK) $(MATGEN) $(ROOT)/$(LINTEST_IDX) util
	/bin/rm -f $(LINTEST_JAR)
	cd $(OUTDIR); $(JAR) cvf ../$(LINTEST_JAR) `find . -name "*.class"`
	$(JAR) uvf $(LINTEST_JAR) `find org -name "*.class"`

nojar: $(BLAS) $(LAPACK) $(MATGEN) $(ROOT)/$(LINTEST_IDX) util

$(ROOT)/$(LINTEST_IDX):	lintest.f
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

runtest:	tester
	$(JAVA) $(JFLAGS) -cp .:$(LINTEST_JAR):$(MATGEN):$(BLAS):$(LAPACK):$(ROOT)/$(UTIL_OBJ) $(LINTEST_PACKAGE).Dchkaa < dtest.in

srctest:
	$(MAKE) -f Makefile_javasrc runtest

verify: $(ROOT)/$(LINTEST_IDX)
	cd $(OUTDIR); $(JAVA) $(MORE_MEM_FLAG) -classpath .:..:$(JUSTICE):$(BCEL):$(ROOT)/../$(UTIL_DIR)/$(UTIL_JAR):$(ROOT)/../$(ERR_DIR)/$(ERR_JAR):$(ROOT)/../$(MATGEN_DIR)/$(MATGEN_JAR):$(ROOT)/../$(BLAS_DIR)/$(BLAS_JAR):$(ROOT)/../$(LAPACK_DIR)/$(LAPACK_JAR) $(VERIFY) $(LINTEST_PDIR)/*.class


clean:
	/bin/rm -rf *.java *.class *.f2j org $(JAVASRC_OUTDIR) $(OUTDIR) $(LINTEST_JAR)
