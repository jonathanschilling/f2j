.PHONY:	util

ROOT=../..
include $(ROOT)/make.def

$(UTIL_JAR):
	if test -f $(ROOT)/../util/$(UTIL_JAR); then \
		cp $(ROOT)/../util/$(UTIL_JAR) .; \
	else \
		$(MAKE) util_deprecated;\
	fi

util_deprecated:	$(UTIL_CLASSES)

$(UTIL_CLASSES):
	mkdir -p $(OUTDIR)
	javac -d $(OUTDIR) $(UTIL_PDIR)/*.java
	cd $(OUTDIR); $(JAR) cvf ../$(UTIL_JAR) .

clean:
	/bin/rm -rf $(OUTDIR) $(UTIL_JAR)
