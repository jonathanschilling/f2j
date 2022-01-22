include make.def

OUTDIR=obj
JAR=jar

UTIL_JAR=f2jutil.jar

JAVA_SOURCE=1.8
JAVA_TARGET=1.8

$(UTIL_JAR):
	mkdir -p $(OUTDIR)
	$(JAVAC) -source $(JAVA_SOURCE) -target $(JAVA_TARGET) -d $(OUTDIR) org/j_paine/formatter/*.java
	$(JAVAC) -source $(JAVA_SOURCE) -target $(JAVA_TARGET) -d $(OUTDIR) org/netlib/util/*.java
	cd $(OUTDIR); $(JAR) cvf ../$(UTIL_JAR) .

install: $(UTIL_JAR)
	install -d -m 755 $(F2J_LIBDIR)
	install -m 644 $(UTIL_JAR) $(F2J_LIBDIR)

clean:
	/bin/rm -rf $(OUTDIR) $(UTIL_JAR)
