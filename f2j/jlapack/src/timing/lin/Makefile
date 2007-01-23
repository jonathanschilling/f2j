.PHONY:	DUMMY util
.SUFFIXES: .f .java

ROOT=../../..

include $(ROOT)/make.def

BLAS=$(ROOT)/$(BLAS_IDX)
LAPACK=$(ROOT)/$(LAPACK_IDX)
MATGEN=$(ROOT)/$(MATGEN_IDX)

XERBLAFLAGS= -c .:$(ROOT)/$(BLAS_OBJ) -p $(ERR_PACKAGE)
F2JFLAGS=-c .:$(OUTDIR):linsrc/$(OUTDIR):$(ROOT)/$(BLAS_OBJ):$(ROOT)/$(ERR_OBJ):$(ROOT)/$(LAPACK_OBJ):$(ROOT)/$(MATGEN_OBJ) -p $(LINTIME_PACKAGE) -o $(OUTDIR)

timer: $(BLAS) $(LAPACK) $(MATGEN) linsrc/$(OUTDIR)/Linsrc.f2j $(OUTDIR)/Lintime.f2j util 

linsrc/$(OUTDIR)/Linsrc.f2j:
	cd linsrc;$(MAKE)

$(OUTDIR)/Lintime.f2j:	$(OUTDIR)/Lsamen.f2j lintime.f
	$(F2J) $(F2JFLAGS) lintime.f > /dev/null

$(OUTDIR)/Lsamen.f2j:	lsamen.f
	$(F2J) $(F2JFLAGS) $< > /dev/null

$(BLAS):
	cd $(ROOT)/$(BLAS_DIR); $(MAKE)

$(LAPACK):
	cd $(ROOT)/$(LAPACK_DIR); $(MAKE)

$(MATGEN):
	cd $(ROOT)/$(MATGEN_DIR); $(MAKE)

util:
	cd $(ROOT)/$(UTIL_DIR); $(MAKE)

runtimer:	timer *.in

*.in:	DUMMY
	java -server -Xmx500M -cp .:./obj:$(ROOT)/$(ERR_OBJ):linsrc/$(OUTDIR):$(ROOT)/$(MATGEN_OBJ):$(ROOT)/$(BLAS_OBJ):$(ROOT)/$(LAPACK_OBJ):$(ROOT)/$(UTIL_DIR)/$(UTIL_JAR) $(LINTIME_PACKAGE).Dtimaa < $@

clean:
	cd linsrc;$(MAKE) clean
	/bin/rm -rf *.java *.class *.f2j $(OUTDIR)
