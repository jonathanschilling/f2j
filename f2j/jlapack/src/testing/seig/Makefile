.PHONY:	DUMMY
.SUFFIXES: .f .java

ROOT=../../..
include $(ROOT)/make.def

SBLAS=$(ROOT)/$(SBLAS_DIR)/$(SBLAS_JAR)
SLAPACK=$(ROOT)/$(SLAPACK_DIR)/$(SLAPACK_JAR)
SMATGEN=$(ROOT)/$(SMATGEN_DIR)/$(SMATGEN_JAR)

XERBLAFLAGS= -c .:$(ROOT)/$(SBLAS_OBJ) -p $(ERR_PACKAGE)
F2JFLAGS=-c .:$(ROOT)/$(SBLAS_OBJ):$(ROOT)/$(SLAPACK_OBJ):$(ROOT)/$(SMATGEN_OBJ) -p $(SEIGTEST_PACKAGE) -o $(OUTDIR) $(STATIC)

tester: $(SBLAS) $(SLAPACK) $(SMATGEN) $(OUTDIR)/Seigtest.f2j util
	/bin/rm -f $(SEIGTEST_JAR)
	cd $(OUTDIR); $(JAR) cvf ../$(SEIGTEST_JAR) `find . -name "*.class"`
	$(JAR) uvf $(SEIGTEST_JAR) `find org -name "*.class"`

nojar: $(SBLAS) $(SLAPACK) $(SMATGEN) $(OUTDIR)/Seigtest.f2j util

$(OUTDIR)/Seigtest.f2j:	seigtest.f
	$(F2J) $(XERBLAFLAGS) xerbla.f > /dev/null
	$(F2J) $(F2JFLAGS) $< > /dev/null

$(SBLAS):
	cd $(ROOT)/$(SBLAS_DIR); $(MAKE)

$(SLAPACK):
	cd $(ROOT)/$(SLAPACK_DIR); $(MAKE)

$(SMATGEN):
	cd $(ROOT)/$(SMATGEN_DIR); $(MAKE)

util:
	cd $(ROOT)/$(UTIL_DIR); $(MAKE)

runtest:	tester *.in

srctest:
	$(MAKE) -f Makefile_javasrc runtest

verify: $(ROOT)/$(SEIGTEST_IDX)
	cd $(OUTDIR); $(JAVA) $(MORE_MEM_FLAG) -classpath .:..:$(JUSTICE):$(BCEL):$(ROOT)/../$(UTIL_DIR)/$(UTIL_JAR):$(ROOT)/../$(ERR_DIR)/$(ERR_JAR):$(ROOT)/../$(SMATGEN_DIR)/$(SMATGEN_JAR):$(ROOT)/../$(SBLAS_DIR)/$(SBLAS_JAR):$(ROOT)/../$(SLAPACK_DIR)/$(SLAPACK_JAR) $(VERIFY) $(SEIGTEST_PDIR)/*.class


*.in:   DUMMY
	$(JAVA) $(JFLAGS) -cp .:$(SEIGTEST_JAR):$(SMATGEN):$(ROOT)/$(SBLAS_DIR)/$(SBLAS_JAR):$(ROOT)/$(SLAPACK_DIR)/$(SLAPACK_JAR):$(ROOT)/$(UTIL_OBJ) $(SEIGTEST_PACKAGE).Schkee < $@

clean:
	/bin/rm -rf *.java *.class *.f2j org $(JAVASRC_OUTDIR) $(OUTDIR) $(SEIGTEST_JAR)
