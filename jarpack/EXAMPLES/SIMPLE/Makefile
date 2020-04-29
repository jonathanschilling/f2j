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

all: Dssimp.jar Dnsimp.jar Snsimp.jar Sssimp.jar

D%.jar: d%.f
	$(F2J) $(F2JFLAGS) $<
	$(JAR) cvf $@ *.class
	/bin/rm -f *.class *.f2j
S%.jar: s%.f
	$(F2J) $(F2JFLAGS) $<
	$(JAR) cvf $@ *.class
	/bin/rm -f *.class *.f2j

runtest: all
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Dssimp.jar Dssimp
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Dnsimp.jar Dnsimp
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Snsimp.jar Snsimp
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Sssimp.jar Sssimp
	
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
