
.SUFFIXES: .f .java .class

ROOT=../..
include $(ROOT)/make.def

BLAS=$(ROOT)/$(BLAS_DIR)/$(BLAS_JAR)
LAPACK=$(ROOT)/$(LAPACK_DIR)/$(LAPACK_JAR)
ARPACK_UTIL=$(ROOT)/$(ARPACK_UTIL_DIR)/$(ARPACK_UTIL_JAR)
ARPACK=$(ROOT)/$(ARPACK_DIR)/$(ARPACK_JAR)
CP_JARS=$(BLAS):$(LAPACK):$(ARPACK_UTIL):$(ARPACK):$(ROOT)/$(F2J_UTIL_JAR)

F2JFLAGS=-c .:$(ROOT)/$(ERR_OBJ):$(ROOT)/$(BLAS_OBJ):$(ROOT)/$(LAPACK_OBJ):$(ROOT)/$(ARPACK_UTIL_OBJ):$(ROOT)/$(ARPACK_OBJ) $(STATIC)

LIB_DEP=$(BLAS) $(LAPACK) $(ARPACK_UTIL) $(ARPACK)

D%.jar: d%.f Dsband.class
	$(F2J) $(F2JFLAGS) $<
	$(JAR) cvf $@ Dsband.class `echo $@ | sed s/\.jar/\.class/`
S%.jar: s%.f Ssband.class
	$(F2J) $(F2JFLAGS) $<
	$(JAR) cvf $@ Ssband.class `echo $@ | sed s/\.jar/\.class/`

D%.class: d%.f
	$(F2J) $(F2JFLAGS) $<
S%.class: s%.f
	$(F2J) $(F2JFLAGS) $<

all: ssbdr dsbdr

ssbdr: Ssbdr1.jar Ssbdr2.jar Ssbdr3.jar Ssbdr4.jar Ssbdr5.jar Ssbdr6.jar
dsbdr: Dsbdr1.jar Dsbdr2.jar Dsbdr3.jar Dsbdr4.jar Dsbdr5.jar Dsbdr6.jar

runtest: ssbdr dsbdr
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Dsbdr1.jar Dsbdr1
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Dsbdr2.jar Dsbdr2
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Dsbdr3.jar Dsbdr3
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Dsbdr4.jar Dsbdr4
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Dsbdr5.jar Dsbdr5
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Dsbdr6.jar Dsbdr6
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Ssbdr1.jar Ssbdr1
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Ssbdr2.jar Ssbdr2
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Ssbdr3.jar Ssbdr3
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Ssbdr4.jar Ssbdr4
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Ssbdr5.jar Ssbdr5
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Ssbdr6.jar Ssbdr6

clean:
	/bin/rm -rf *.java *.class *.f2j *.jar
