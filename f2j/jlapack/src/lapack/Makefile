.SUFFIXES: .f .java

ROOT=../..

include $(ROOT)/make.def

F2JFLAGS=-c .:$(OUTDIR):$(ROOT)/$(ERR_OBJ):$(ROOT)/$(BLAS_OBJ) -p $(LAPACK_PACKAGE) -o $(OUTDIR) -s -d $(STATIC)

$(ROOT)/$(LAPACK_IDX): $(ROOT)/$(BLAS_DIR)/$(BLAS_JAR) $(ROOT)/$(ERR_DIR)/$(ERR_JAR) lapack.f
	$(F2J) $(F2JFLAGS) lapack.f > /dev/null
	cd $(OUTDIR); $(JAR) cvf ../$(LAPACK_JAR) `find . -name "*.class"`
	mkdir -p $(SIMPLE_DIR)/$(LAPACK_PDIR)
	-cp $(OUTDIR)/$(LAPACK_PDIR)/[A-Z][A-Z]*.java $(SIMPLE_DIR)/$(LAPACK_PDIR)
	-$(JAVAC) -classpath .:$(ROOT)/$(BLAS_DIR)/$(BLAS_JAR):$(LAPACK_JAR):$(SIMPLE_DIR):$(ROOT)/$(UTIL_DIR)/$(UTIL_JAR):$(ROOT)/$(ERR_DIR)/$(ERR_JAR) $(SIMPLE_DIR)/$(LAPACK_PDIR)/*.java
	cd $(SIMPLE_DIR); $(JAR) cvf ../$(SIMPLE_LAPACK_JAR) `find . -name "*.class"`

nojar: $(ROOT)/$(BLAS_DIR)/$(BLAS_JAR) $(ROOT)/$(ERR_DIR)/$(ERR_JAR) lapack.f
	$(F2J) $(F2JFLAGS) lapack.f > /dev/null
	
$(ROOT)/$(BLAS_DIR)/$(BLAS_JAR):
	cd $(ROOT)/$(BLAS_DIR);$(MAKE)

$(ROOT)/$(ERR_DIR)/$(ERR_JAR):
	cd $(ROOT)/$(ERR_DIR);$(MAKE)

javasrc:
	$(MAKE) -f Makefile_javasrc

verify: $(LAPACK_JAR)
	cd $(OUTDIR); $(JAVA) $(MORE_MEM_FLAG) -classpath .:$(JUSTICE):$(BCEL):$(ROOT)/../$(UTIL_DIR)/$(UTIL_JAR):$(ROOT)/../$(ERR_DIR)/$(ERR_JAR):$(ROOT)/../$(BLAS_DIR)/$(BLAS_JAR) $(VERIFY) $(LAPACK_PDIR)/*.class

clean:
	/bin/rm -rf *.java *.class *.f2j $(SIMPLE_LAPACK_JAR) $(LAPACK_JAR) $(OUTDIR) $(JAVASRC_OUTDIR) $(SIMPLE_DIR)
