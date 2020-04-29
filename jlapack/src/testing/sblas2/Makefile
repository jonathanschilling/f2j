.SUFFIXES: .f .java

ROOT=../../..

include $(ROOT)/make.def

XERBLAFLAGS= -c .:$(ROOT)/$(SBLAS_OBJ) -p $(ERR_PACKAGE)
F2JFLAGS=-c .:$(ROOT)/$(SBLAS_OBJ) -p $(SBLASTEST_PACKAGE) -o $(OUTDIR) $(STATIC)

tester: $(ROOT)/$(SBLAS_DIR)/$(SBLAS_JAR) $(ROOT)/$(SBLAS2TEST_IDX) $(ROOT)/$(UTIL_DIR)/$(UTIL_JAR)
	/bin/rm -f $(SBLAS2TEST_JAR)
	cd $(OUTDIR); $(JAR) cvf0 ../$(SBLAS2TEST_JAR) `find . -name "*.class"`
	$(JAR) uvf0 $(SBLAS2TEST_JAR) `find org -name "*.class"`

nojar: $(ROOT)/$(SBLAS_DIR)/$(SBLAS_JAR) $(ROOT)/$(SBLAS2TEST_IDX) $(ROOT)/$(UTIL_DIR)/$(UTIL_JAR)

$(ROOT)/$(SBLAS2TEST_IDX):	sblat2.f
	$(F2J) $(XERBLAFLAGS) xerbla.f > /dev/null
	$(F2J) $(F2JFLAGS) $< > /dev/null

$(ROOT)/$(SBLAS_DIR)/$(SBLAS_JAR):
	cd $(ROOT)/$(SBLAS_DIR); $(MAKE)

$(ROOT)/$(UTIL_DIR)/$(UTIL_JAR):
	cd $(ROOT)/$(UTIL_DIR); $(MAKE)

runtest:	tester
	$(JAVA) $(JFLAGS) -cp .:$(SBLAS2TEST_JAR):$(ROOT)/$(SBLAS_DIR)/$(SBLAS_JAR):$(ROOT)/$(UTIL_DIR)/$(UTIL_JAR):$(ROOT)/$(ERR_DIR)/$(ERR_JAR) $(SBLASTEST_PACKAGE).Sblat2 < sblat2.in

srctest:
	$(MAKE) -f Makefile_javasrc

verify: $(ROOT)/$(SBLAS2TEST_IDX)
	cd $(OUTDIR); $(JAVA) $(MORE_MEM_FLAG) -classpath .:..:$(JUSTICE):$(BCEL):$(ROOT)/../$(UTIL_DIR)/$(UTIL_JAR):$(ROOT)/../$(ERR_DIR)/$(ERR_JAR):$(ROOT)/../$(SBLAS_DIR)/$(SBLAS_JAR) $(VERIFY) $(SBLASTEST_PDIR)/*.class

clean:
	/bin/rm -rf *.java *.class *.f2j org $(OUTDIR) $(JAVASRC_OUTDIR) $(SBLAS2TEST_JAR)
