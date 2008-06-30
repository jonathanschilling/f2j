.SUFFIXES: .f .java

ROOT=../..
include $(ROOT)/make.def

BLAS=$(ROOT)/$(BLAS_DIR)/$(BLAS_JAR)
LAPACK=$(ROOT)/$(LAPACK_DIR)/$(LAPACK_JAR)
ARPACK_UTIL=$(ROOT)/$(ARPACK_UTIL_DIR)/$(ARPACK_UTIL_JAR)
ARPACK=$(ROOT)/$(ARPACK_DIR)/$(ARPACK_JAR)
CP_JARS=$(BLAS):$(LAPACK):$(ARPACK_UTIL):$(ARPACK):$(ROOT)/$(F2J_UTIL_JAR)

F2JFLAGS=-c .:$(ROOT)/$(ERR_OBJ):$(ROOT)/$(BLAS_OBJ):$(ROOT)/$(LAPACK_OBJ):$(ROOT)/$(ARPACK_UTIL_OBJ):$(ROOT)/$(ARPACK_OBJ) $(STATIC)

LIB_DEP=$(BLAS) $(LAPACK) $(ARPACK_UTIL) $(ARPACK)

all: double single

double: Dsdrv1.jar Dsdrv2.jar Dsdrv3.jar Dsdrv4.jar Dsdrv5.jar Dsdrv6.jar
single: Ssdrv1.jar Ssdrv2.jar Ssdrv3.jar Ssdrv4.jar Ssdrv5.jar Ssdrv6.jar

D%.jar: d%.f
	$(F2J) $(F2JFLAGS) $<
	$(JAR) cvf $@ *.class
	/bin/rm -f *.class *.f2j
S%.jar: s%.f
	$(F2J) $(F2JFLAGS) $<
	$(JAR) cvf $@ *.class
	/bin/rm -f *.class *.f2j

runtest: run_double run_single

run_double: double
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Dsdrv1.jar Dsdrv1
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Dsdrv2.jar Dsdrv2
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Dsdrv3.jar Dsdrv3
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Dsdrv4.jar Dsdrv4
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Dsdrv5.jar Dsdrv5
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Dsdrv6.jar Dsdrv6

run_single: single
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Ssdrv1.jar Ssdrv1
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Ssdrv2.jar Ssdrv2
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Ssdrv3.jar Ssdrv3
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Ssdrv4.jar Ssdrv4
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Ssdrv5.jar Ssdrv5
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Ssdrv6.jar Ssdrv6
	
$(BLAS):
	cd $(ROOT)/$(BLAS_DIR); $(MAKE)

$(LAPACK):
	cd $(ROOT)/$(LAPACK_DIR); $(MAKE)

$(ARPACK_UTIL):
	cd $(ROOT)/$(ARPACK_UTIL_DIR); $(MAKE)

$(ARPACK):
	cd $(ROOT)/$(ARPACK_DIR); $(MAKE)

clean:
	/bin/rm -rf *.java *.class *.f2j *.jar
