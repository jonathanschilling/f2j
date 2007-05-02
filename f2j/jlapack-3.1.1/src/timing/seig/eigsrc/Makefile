.SUFFIXES: .f .java

ROOT=../../../..

include $(ROOT)/make.def

SBLAS=$(ROOT)/$(SBLAS_IDX)
SLAPACK=$(ROOT)/$(SLAPACK_IDX)

F2JFLAGS=-c .:$(ROOT)/$(SBLAS_OBJ):$(ROOT)/$(ERR_OBJ):$(ROOT)/$(SLAPACK_OBJ) -p $(SEIGSRC_PACKAGE) -o $(OUTDIR)

tester: $(SBLAS) $(SLAPACK) $(OUTDIR)/Seigsrc.f2j

$(OUTDIR)/Seigsrc.f2j:	seigsrc.f
	$(F2J) $(F2JFLAGS) $< > /dev/null

$(SBLAS):
	cd $(ROOT)/$(SBLAS_DIR); $(MAKE)

$(SLAPACK):
	cd $(ROOT)/$(SLAPACK_DIR); $(MAKE)

clean:
	/bin/rm -rf *.java *.class *.f2j $(OUTDIR)
