include make.def

OUTDIR=obj
JAR=jar

UTIL_JAR=f2jutil.jar

$(UTIL_JAR):
	mkdir -p $(OUTDIR)
	$(JAVAC) -source 1.4 -target 1.4 -d $(OUTDIR) org/j_paine/formatter/*.java
	$(JAVAC) -source 1.4 -target 1.4 -d $(OUTDIR) org/netlib/util/*.java
	cd $(OUTDIR); $(JAR) cvf ../$(UTIL_JAR) .

install: $(UTIL_JAR)
	install -d -m 755 $(F2J_LIBDIR)
	install -m 644 $(UTIL_JAR) $(F2J_LIBDIR)

clean:
	/bin/rm -rf $(OUTDIR) $(UTIL_JAR)
