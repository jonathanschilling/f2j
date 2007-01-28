.PHONY:	DUMMY util
.SUFFIXES: .f .java

ROOT=../../..

include $(ROOT)/make.def

SBLAS=$(ROOT)/$(SBLAS_IDX)
SLAPACK=$(ROOT)/$(SLAPACK_IDX)
SMATGEN=$(ROOT)/$(SMATGEN_IDX)

XERBLAFLAGS= -c .:$(ROOT)/$(SBLAS_OBJ) -p $(ERR_PACKAGE)
F2JFLAGS=-c .:eigsrc/$(OUTDIR):$(ROOT)/$(SBLAS_OBJ):$(ROOT)/$(SLAPACK_OBJ):$(ROOT)/$(SMATGEN_OBJ) -p $(SEIGTIME_PACKAGE) -o $(OUTDIR)

timer: $(SBLAS) $(SLAPACK) $(SMATGEN) eigsrc/$(OUTDIR)/Seigsrc.f2j $(OUTDIR)/Seigtime.f2j util

eigsrc/$(OUTDIR)/Seigsrc.f2j:
	cd eigsrc;$(MAKE)

$(OUTDIR)/Seigtime.f2j:	seigtime.f
	$(F2J) $(F2JFLAGS) seigtime.f > /dev/null

$(SBLAS):
	cd $(ROOT)/$(SBLAS_DIR); $(MAKE)

$(SLAPACK):
	cd $(ROOT)/$(SLAPACK_DIR); $(MAKE)

$(SMATGEN):
	cd $(ROOT)/$(SMATGEN_DIR); $(MAKE)

util:
	cd $(ROOT)/$(UTIL_DIR); $(MAKE)

runtimer:	timer *.in

*.in:	DUMMY
	java -cp .:./obj:eigsrc/$(OUTDIR):$(ROOT)/$(SMATGEN_OBJ):$(ROOT)/$(SBLAS_OBJ):$(ROOT)/$(SLAPACK_OBJ):$(ROOT)/$(UTIL_DIR)/$(UTIL_JAR) $(SEIGTIME_PACKAGE).Stimee < $@

clean:
	cd eigsrc;$(MAKE) clean
	/bin/rm -rf *.java *.class *.f2j $(OUTDIR)
