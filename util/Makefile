include make.def

OUTDIR=obj
JAR=jar

UTIL_JAR=f2jutil.jar

VER_TARGET=1.2

$(UTIL_JAR):
	mkdir -p $(OUTDIR)
	$(JAVAC) -source $(VER_TARGET) -target $(VER_TARGET) -d $(OUTDIR) org/j_paine/formatter/*.java
	$(JAVAC) -source $(VER_TARGET) -target $(VER_TARGET) -d $(OUTDIR) org/netlib/util/*.java
	cd $(OUTDIR); $(JAR) cvf ../$(UTIL_JAR) .

install: $(UTIL_JAR)
	install -d -m 755 $(F2J_LIBDIR)
	install -m 644 $(UTIL_JAR) $(F2J_LIBDIR)

clean:
	/bin/rm -rf $(OUTDIR) $(UTIL_JAR)
