.SUFFIXES: .f .java

ROOT=../../..

include $(ROOT)/make.def

BLAS=$(ROOT)/$(BLAS_DIR)/$(BLAS_JAR)
LAPACK=$(ROOT)/$(LAPACK_DIR)/$(LAPACK_JAR)

F2JFLAGS=-c .:$(ROOT)/$(BLAS_OBJ):$(ROOT)/$(ERR_OBJ):$(ROOT)/$(LAPACK_OBJ) -p $(SMATGEN_PACKAGE) -o $(OUTDIR) $(STATIC)

tester: $(BLAS) $(LAPACK) $(ROOT)/$(SMATGEN_IDX)
	/bin/rm -f $(SMATGEN_JAR)
	cd $(OUTDIR); $(JAR) cvf ../$(SMATGEN_JAR) `find . -name "*.class"`

nojar: $(BLAS) $(LAPACK) $(ROOT)/$(SMATGEN_IDX)

javasrc:
	$(MAKE) -f Makefile_javasrc

$(ROOT)/$(SMATGEN_IDX):	smatgen.f
	$(F2J) $(F2JFLAGS) $< > /dev/null

$(BLAS):
	cd $(ROOT)/$(BLAS_DIR); $(MAKE)

$(LAPACK):
	cd $(ROOT)/$(LAPACK_DIR); $(MAKE)

verify: $(ROOT)/$(SMATGEN_IDX)
	cd $(OUTDIR); $(JAVA) $(MORE_MEM_FLAG) -classpath .:$(JUSTICE):$(BCEL):$(ROOT)/../$(UTIL_DIR)/$(UTIL_JAR):$(ROOT)/../$(ERR_DIR)/$(ERR_JAR):$(ROOT)/../$(BLAS_DIR)/$(BLAS_JAR):$(ROOT)/../$(LAPACK_DIR)/$(LAPACK_JAR) $(VERIFY) $(SMATGEN_PDIR)/*.class

clean:
	/bin/rm -rf *.java *.class *.f2j $(JAVASRC_OUTDIR) $(OUTDIR) $(SMATGEN_JAR)
