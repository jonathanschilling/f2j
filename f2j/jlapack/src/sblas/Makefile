.SUFFIXES: .f .java

ROOT=../..
include $(ROOT)/make.def

F2JFLAGS=-c .:$(OUTDIR):$(ROOT)/$(ERR_OBJ) -p $(BLAS_PACKAGE) -o $(OUTDIR) -s -d $(STATIC)

$(SBLAS_JAR): $(ROOT)/$(ERR_DIR)/$(ERR_JAR) $(ROOT)/$(UTIL_DIR)/$(UTIL_JAR) sblas.f
	$(F2J) $(F2JFLAGS) sblas.f > /dev/null
	cd $(OUTDIR); $(JAR) cvf ../$(SBLAS_JAR) `find . -name "*.class"`
	mkdir -p $(SIMPLE_DIR)/$(BLAS_PDIR)
	-cp `find $(OUTDIR)/$(BLAS_PDIR) -name "[A-Z][A-Z]*.java"` $(SIMPLE_DIR)/$(BLAS_PDIR)
	-$(JAVAC) -classpath .:$(SBLAS_JAR):$(SIMPLE_DIR):$(ROOT)/$(UTIL_DIR)/$(UTIL_JAR):$(ROOT)/$(ERR_DIR)/$(ERR_JAR) $(SIMPLE_DIR)/$(BLAS_PDIR)/*.java
	cd $(SIMPLE_DIR); $(JAR) cvf ../$(SIMPLE_SBLAS_JAR) `find . -name "*.class"`

nojar: $(ROOT)/$(ERR_DIR)/$(ERR_JAR) sblas.f
	$(F2J) $(F2JFLAGS) sblas.f > /dev/null

$(ROOT)/$(ERR_DIR)/$(ERR_JAR):
	cd $(ROOT)/$(ERR_DIR);$(MAKE)

javasrc:
	$(MAKE) -f Makefile_javasrc

$(ROOT)/$(UTIL_DIR)/$(UTIL_JAR):
	cd $(ROOT)/$(UTIL_DIR); $(MAKE)

verify:	$(SBLAS_JAR)
	./verify_all.csh

clean:
	/bin/rm -rf *.java *.class *.f2j $(SBLAS_JAR) $(SIMPLE_SBLAS_JAR) $(OUTDIR) $(JAVASRC_OUTDIR) $(SIMPLE_DIR)
