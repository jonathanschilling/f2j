.PHONY:	util

ROOT=../..
include $(ROOT)/make.def

util:	$(UTIL_CLASSES)

$(UTIL_CLASSES):
	mkdir -p $(OUTDIR)
	javac -d $(OUTDIR) $(UTIL_PDIR)/*.java
	cd $(OUTDIR); $(JAR) cvf ../$(UTIL_JAR) .

clean:
	/bin/rm -rf $(OUTDIR) $(UTIL_JAR)
