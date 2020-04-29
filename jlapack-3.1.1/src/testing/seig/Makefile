.PHONY:	DUMMY
.SUFFIXES: .f .java

ROOT=../../..
include $(ROOT)/make.def

BLAS=$(ROOT)/$(BLAS_DIR)/$(BLAS_JAR)
LAPACK=$(ROOT)/$(LAPACK_DIR)/$(LAPACK_JAR)
SMATGEN=$(ROOT)/$(SMATGEN_DIR)/$(SMATGEN_JAR)

XERBLAFLAGS= -c .:$(ROOT)/$(BLAS_OBJ) -p $(ERR_PACKAGE)
F2JFLAGS=-c .:$(ROOT)/$(BLAS_OBJ):$(ROOT)/$(LAPACK_OBJ):$(ROOT)/$(SMATGEN_OBJ) -p $(SEIGTEST_PACKAGE) -o $(OUTDIR) $(STATIC)

tester: $(BLAS) $(LAPACK) $(SMATGEN) $(OUTDIR)/Seigtest.f2j util
	/bin/rm -f $(SEIGTEST_JAR)
	cd $(OUTDIR); $(JAR) cvf ../$(SEIGTEST_JAR) `find . -name "*.class"`
	$(JAR) uvf $(SEIGTEST_JAR) `find org -name "*.class"`

nojar: $(BLAS) $(LAPACK) $(SMATGEN) $(OUTDIR)/Seigtest.f2j util

$(OUTDIR)/Seigtest.f2j:	seigtest.f
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

runtest:	tester *.in

srctest:
	$(MAKE) -f Makefile_javasrc runtest

verify: $(ROOT)/$(SEIGTEST_IDX)
	cd $(OUTDIR); $(JAVA) $(MORE_MEM_FLAG) -classpath .:..:$(JUSTICE):$(BCEL):$(ROOT)/../$(UTIL_DIR)/$(UTIL_JAR):$(ROOT)/../$(ERR_DIR)/$(ERR_JAR):$(ROOT)/../$(SMATGEN_DIR)/$(SMATGEN_JAR):$(ROOT)/../$(BLAS_DIR)/$(BLAS_JAR):$(ROOT)/../$(LAPACK_DIR)/$(LAPACK_JAR) $(VERIFY) $(SEIGTEST_PDIR)/*.class


*.in:   DUMMY
	$(JAVA) $(JFLAGS) -cp .:$(SEIGTEST_JAR):$(SMATGEN):$(ROOT)/$(BLAS_DIR)/$(BLAS_JAR):$(ROOT)/$(LAPACK_DIR)/$(LAPACK_JAR):$(ROOT)/$(UTIL_DIR)/$(UTIL_JAR) $(SEIGTEST_PACKAGE).Schkee < $@

clean:
	/bin/rm -rf *.java *.class *.f2j org $(JAVASRC_OUTDIR) $(OUTDIR) $(SEIGTEST_JAR)
