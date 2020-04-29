.SUFFIXES: .f .java

ROOT=../../../..

include $(ROOT)/make.def

BLAS=$(ROOT)/$(BLAS_IDX)
LAPACK=$(ROOT)/$(LAPACK_IDX)

F2JFLAGS=-c .:$(ROOT)/$(BLAS_OBJ):$(ROOT)/$(ERR_OBJ):$(ROOT)/$(LAPACK_OBJ) -p $(EIGSRC_PACKAGE) -o $(OUTDIR)

tester: $(BLAS) $(LAPACK) $(OUTDIR)/Eigsrc.f2j

$(OUTDIR)/Eigsrc.f2j:	eigsrc.f
	$(F2J) $(F2JFLAGS) $< > /dev/null

$(BLAS):
	cd $(ROOT)/$(BLAS_DIR); $(MAKE)

$(LAPACK):
	cd $(ROOT)/$(LAPACK_DIR); $(MAKE)

clean:
	/bin/rm -rf *.java *.class *.f2j $(OUTDIR)
