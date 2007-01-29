.PHONY:	DUMMY util
.SUFFIXES: .f .java

ROOT=../../..

include $(ROOT)/make.def

SBLAS=$(ROOT)/$(SBLAS_IDX)
SLAPACK=$(ROOT)/$(SLAPACK_IDX)
SMATGEN=$(ROOT)/$(SMATGEN_IDX)

XERBLAFLAGS= -c .:$(ROOT)/$(SBLAS_OBJ) -p $(ERR_PACKAGE)
F2JFLAGS=-c .:$(OUTDIR):linsrc/$(OUTDIR):$(ROOT)/$(SBLAS_OBJ):$(ROOT)/$(ERR_OBJ):$(ROOT)/$(SLAPACK_OBJ):$(ROOT)/$(SMATGEN_OBJ) -p $(SLINTIME_PACKAGE) -o $(OUTDIR)

timer: $(SBLAS) $(SLAPACK) $(SMATGEN) linsrc/$(OUTDIR)/Slinsrc.f2j $(OUTDIR)/Slintime.f2j util 
	/bin/rm -f $(SLINTIME_JAR)
	cd linsrc/$(OUTDIR); $(JAR) cvf ../../$(SLINTIME_JAR) `find . -name "*.class"`
	cd $(OUTDIR); $(JAR) uvf ../$(SLINTIME_JAR) `find . -name "*.class"`

linsrc/$(OUTDIR)/Slinsrc.f2j:
	cd linsrc;$(MAKE)

$(OUTDIR)/Slintime.f2j:	$(OUTDIR)/Lsamen.f2j slintime.f
	$(F2J) $(F2JFLAGS) slintime.f > /dev/null

$(OUTDIR)/Lsamen.f2j:   lsamen.f
	$(F2J) $(F2JFLAGS) $< > /dev/null

$(SBLAS):
	cd $(ROOT)/$(SBLAS_DIR); $(MAKE)

$(SLAPACK):
	cd $(ROOT)/$(SLAPACK_DIR); $(MAKE)

$(SMATGEN):
	cd $(ROOT)/$(SMATGEN_DIR); $(MAKE)

util:
	cd $(ROOT)/$(UTIL_DIR); $(MAKE)

runtimer: small

small:	timer s*.in

large:	timer S*.in

*.in:	DUMMY
	java -server -Xmx500M -cp .:./obj:$(ROOT)/$(ERR_OBJ):linsrc/$(OUTDIR):$(ROOT)/$(SMATGEN_OBJ):$(ROOT)/$(SBLAS_OBJ):$(ROOT)/$(SLAPACK_OBJ):$(ROOT)/$(UTIL_DIR)/$(UTIL_JAR) $(SLINTIME_PACKAGE).Stimaa < $@

clean:
	cd linsrc;$(MAKE) clean
	/bin/rm -rf *.java *.class *.f2j $(OUTDIR) $(SLINTIME_JAR)
