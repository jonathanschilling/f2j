.SUFFIXES: .f .java

ROOT=../../..
include $(ROOT)/make.def

BLAS=$(ROOT)/$(BLAS_DIR)/$(BLAS_JAR)
LAPACK=$(ROOT)/$(LAPACK_DIR)/$(LAPACK_JAR)
SMATGEN=$(ROOT)/$(SMATGEN_DIR)/$(SMATGEN_JAR)

XERBLAFLAGS= -c .:$(ROOT)/$(BLAS_OBJ) -p $(ERR_PACKAGE)
F2JFLAGS=-c .:$(ROOT)/$(BLAS_OBJ):$(ROOT)/$(LAPACK_OBJ):$(ROOT)/$(SMATGEN_OBJ) -p $(SLINTEST_PACKAGE) -o $(OUTDIR) $(STATIC)

tester: $(BLAS) $(LAPACK) $(SMATGEN) $(ROOT)/$(SLINTEST_IDX) util
	/bin/rm -f $(SLINTEST_JAR)
	cd $(OUTDIR); $(JAR) cvf ../$(SLINTEST_JAR) `find . -name "*.class"`
	$(JAR) uvf $(SLINTEST_JAR) `find org -name "*.class"`

nojar: $(BLAS) $(LAPACK) $(SMATGEN) $(ROOT)/$(SLINTEST_IDX) util

$(ROOT)/$(SLINTEST_IDX):	slintest.f
	$(F2J) $(XERBLAFLAGS) xerbla.f > /dev/null
	$(F2J) $(F2JFLAGS) $< > /dev/null

$(BLAS):
	cd $(ROOT)/$(BLAS_DIR); $(MAKE)

$(LAPACK):
	cd $(ROOT)/$(LAPACK_DIR); $(MAKE)

$(SMATGEN):
	cd $(ROOT)/$(SMATGEN_DIR); $(MAKE)

util:
	cd $(ROOT)/$(UTIL_DIR); $(MAKE)

runtest:	tester
	$(JAVA) $(JFLAGS) -cp .:$(SLINTEST_JAR):$(SMATGEN):$(BLAS):$(LAPACK):$(ROOT)/$(UTIL_DIR)/$(UTIL_JAR) $(SLINTEST_PACKAGE).Schkaa < stest.in

srctest:
	$(MAKE) -f Makefile_javasrc runtest

verify: $(ROOT)/$(SLINTEST_IDX)
	cd $(OUTDIR); $(JAVA) $(MORE_MEM_FLAG) -classpath .:..:$(JUSTICE):$(BCEL):$(ROOT)/../$(UTIL_DIR)/$(UTIL_JAR):$(ROOT)/../$(ERR_DIR)/$(ERR_JAR):$(ROOT)/../$(SMATGEN_DIR)/$(SMATGEN_JAR):$(ROOT)/../$(BLAS_DIR)/$(BLAS_JAR):$(ROOT)/../$(LAPACK_DIR)/$(LAPACK_JAR) $(VERIFY) $(SLINTEST_PDIR)/*.class


clean:
	/bin/rm -rf *.java *.class *.f2j org $(JAVASRC_OUTDIR) $(OUTDIR) $(SLINTEST_JAR)
