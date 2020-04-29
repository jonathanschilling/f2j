.PHONY:	DUMMY util
.SUFFIXES: .f .java

ROOT=../../..

include $(ROOT)/make.def

BLAS=$(ROOT)/$(BLAS_IDX)
LAPACK=$(ROOT)/$(LAPACK_IDX)
SMATGEN=$(ROOT)/$(SMATGEN_IDX)

XERBLAFLAGS= -c .:$(ROOT)/$(BLAS_OBJ) -p $(ERR_PACKAGE)
F2JFLAGS=-c .:eigsrc/$(OUTDIR):$(ROOT)/$(BLAS_OBJ):$(ROOT)/$(LAPACK_OBJ):$(ROOT)/$(SMATGEN_OBJ) -p $(SEIGTIME_PACKAGE) -o $(OUTDIR)

TIMER_CLASSPATH=-cp .:./obj:eigsrc/$(OUTDIR):$(ROOT)/$(SMATGEN_OBJ):$(ROOT)/$(BLAS_OBJ):$(ROOT)/$(LAPACK_OBJ):$(ROOT)/$(UTIL_DIR)/$(UTIL_JAR)

timer: $(BLAS) $(LAPACK) $(SMATGEN) eigsrc/$(OUTDIR)/Seigsrc.f2j $(OUTDIR)/Seigtime.f2j util
	/bin/rm -f $(SEIGTIME_JAR)
	cd eigsrc/$(OUTDIR); $(JAR) cvf ../../$(SEIGTIME_JAR) `find . -name "*.class"`
	cd $(OUTDIR); $(JAR) uvf ../$(SEIGTIME_JAR) `find . -name "*.class"`

eigsrc/$(OUTDIR)/Seigsrc.f2j: eigsrc/seigsrc.f
	cd eigsrc;$(MAKE)

$(OUTDIR)/Seigtime.f2j:	seigtime.f
	$(F2J) $(F2JFLAGS) seigtime.f > /dev/null

$(BLAS):
	cd $(ROOT)/$(BLAS_DIR); $(MAKE)

$(LAPACK):
	cd $(ROOT)/$(LAPACK_DIR); $(MAKE)

$(SMATGEN):
	cd $(ROOT)/$(SMATGEN_DIR); $(MAKE)

util:
	cd $(ROOT)/$(UTIL_DIR); $(MAKE)

runtimer: small

small:	timer s*.in

large:	timer input_files_large/S*.in

*.in:	DUMMY
	java $(MORE_MEM_FLAG) $(TIMER_CLASSPATH) $(SEIGTIME_PACKAGE).Stimee < $@

input_files_large/*.in:	DUMMY
	java $(MORE_MEM_FLAG) $(TIMER_CLASSPATH) $(SEIGTIME_PACKAGE).Stimee < $@

clean:
	cd eigsrc;$(MAKE) clean
	/bin/rm -rf *.java *.class *.f2j $(OUTDIR) $(SEIGTIME_JAR)
