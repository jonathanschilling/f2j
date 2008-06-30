
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

D%.jar: d%.f
	$(F2J) $(F2JFLAGS) $<
	$(JAR) cvf $@ *.class
	/bin/rm -f *.class *.f2j
S%.jar: s%.f
	$(F2J) $(F2JFLAGS) $<
	$(JAR) cvf $@ *.class
	/bin/rm -f *.class *.f2j

all: Ssvd Dsvd

Ssvd: Ssvd.jar

Dsvd: Dsvd.jar

runtest: Ssvd Dsvd
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Dsvd.jar Dsvd
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Ssvd.jar Ssvd

clean:
	/bin/rm -rf *.java *.class *.jar *.f2j *.jar
