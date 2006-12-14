.SUFFIXES: .f .java

ROOT=../..

include $(ROOT)/make.def

F2JFLAGS=-c .:$(OUTDIR):$(ROOT)/$(ERR_OBJ):$(ROOT)/$(SBLAS_OBJ) -p $(SLAPACK_PACKAGE) -o $(OUTDIR) -s -d $(STATIC)

$(ROOT)/$(SLAPACK_IDX): $(ROOT)/$(SBLAS_DIR)/$(SBLAS_JAR) $(ROOT)/$(ERR_DIR)/$(ERR_JAR) slapack.f
	$(F2J) $(F2JFLAGS) slapack.f > /dev/null
	cd $(OUTDIR); $(JAR) cvf ../$(SLAPACK_JAR) `find . -name "*.class"`
	mkdir -p $(SIMPLE_DIR)/$(SLAPACK_PDIR)
	-cp `find $(OUTDIR)/$(SLAPACK_PDIR) -name "[A-Z][A-Z]*.java"` $(SIMPLE_DIR)/$(SLAPACK_PDIR)
	-$(JAVAC) -classpath .:$(ROOT)/$(SBLAS_DIR)/$(SBLAS_JAR):$(SLAPACK_JAR):$(SIMPLE_DIR):$(ROOT)/$(UTIL_DIR)/$(UTIL_JAR):$(ROOT)/$(ERR_DIR)/$(ERR_JAR) $(SIMPLE_DIR)/$(SLAPACK_PDIR)/*.java
	cd $(SIMPLE_DIR); $(JAR) cvf ../$(SIMPLE_SLAPACK_JAR) `find . -name "*.class"`

nojar: $(ROOT)/$(SBLAS_DIR)/$(SBLAS_JAR) $(ROOT)/$(ERR_DIR)/$(ERR_JAR) slapack.f
	$(F2J) $(F2JFLAGS) slapack.f > /dev/null
	
$(ROOT)/$(SBLAS_DIR)/$(SBLAS_JAR):
	cd $(ROOT)/$(SBLAS_DIR);$(MAKE)

$(ROOT)/$(ERR_DIR)/$(ERR_JAR):
	cd $(ROOT)/$(ERR_DIR);$(MAKE)

javasrc:
	$(MAKE) -f Makefile_javasrc

verify: $(SLAPACK_JAR)
	cd $(OUTDIR); $(JAVA) $(MORE_MEM_FLAG) -classpath .:$(JUSTICE):$(BCEL):$(ROOT)/../$(UTIL_DIR)/$(UTIL_JAR):$(ROOT)/../$(ERR_DIR)/$(ERR_JAR):$(ROOT)/../$(SBLAS_DIR)/$(SBLAS_JAR) $(VERIFY) $(SLAPACK_PDIR)/*.class

clean:
	/bin/rm -rf *.java *.class *.f2j $(SIMPLE_SLAPACK_JAR) $(SLAPACK_JAR) $(OUTDIR) $(JAVASRC_OUTDIR) $(SIMPLE_DIR)
