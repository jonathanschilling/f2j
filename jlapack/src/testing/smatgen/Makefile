.SUFFIXES: .f .java

ROOT=../../..

include $(ROOT)/make.def

SBLAS=$(ROOT)/$(SBLAS_DIR)/$(SBLAS_JAR)
SLAPACK=$(ROOT)/$(SLAPACK_DIR)/$(SLAPACK_JAR)

F2JFLAGS=-c .:$(ROOT)/$(SBLAS_OBJ):$(ROOT)/$(ERR_OBJ):$(ROOT)/$(SLAPACK_OBJ) -p $(SMATGEN_PACKAGE) -o $(OUTDIR) $(STATIC)

tester: $(SBLAS) $(SLAPACK) $(ROOT)/$(SMATGEN_IDX)
	/bin/rm -f $(SMATGEN_JAR)
	cd $(OUTDIR); $(JAR) cvf ../$(SMATGEN_JAR) `find . -name "*.class"`

nojar: $(SBLAS) $(SLAPACK) $(ROOT)/$(SMATGEN_IDX)

javasrc:
	$(MAKE) -f Makefile_javasrc

$(ROOT)/$(SMATGEN_IDX):	smatgen.f
	$(F2J) $(F2JFLAGS) $< > /dev/null

$(SBLAS):
	cd $(ROOT)/$(SBLAS_DIR); $(MAKE)

$(SLAPACK):
	cd $(ROOT)/$(SLAPACK_DIR); $(MAKE)

verify: $(ROOT)/$(SMATGEN_IDX)
	cd $(OUTDIR); $(JAVA) $(MORE_MEM_FLAG) -classpath .:$(JUSTICE):$(BCEL):$(ROOT)/../$(UTIL_DIR)/$(UTIL_JAR):$(ROOT)/../$(ERR_DIR)/$(ERR_JAR):$(ROOT)/../$(SBLAS_DIR)/$(SBLAS_JAR):$(ROOT)/../$(SLAPACK_DIR)/$(SLAPACK_JAR) $(VERIFY) $(SMATGEN_PDIR)/*.class

clean:
	/bin/rm -rf *.java *.class *.f2j $(JAVASRC_OUTDIR) $(OUTDIR) $(SMATGEN_JAR)
