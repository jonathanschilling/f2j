
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

all: sndrv dndrv

sndrv: Sndrv1 Sndrv2 Sndrv3 Sndrv4

Sndrv1: Sndrv1.jar
Sndrv2: Sndrv2.jar
Sndrv3: Sndrv3.jar
Sndrv4: Sndrv4.jar

dndrv: Dndrv1 Dndrv2 Dndrv3 Dndrv4

Dndrv1: Dndrv1.jar
Dndrv2: Dndrv2.jar
Dndrv3: Dndrv3.jar
Dndrv4: Dndrv4.jar

runtest: sndrv dndrv
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Dndrv1.jar Dndrv1
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Dndrv2.jar Dndrv2
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Dndrv3.jar Dndrv3
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Dndrv4.jar Dndrv4
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Sndrv1.jar Sndrv1
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Sndrv2.jar Sndrv2
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Sndrv3.jar Sndrv3
	$(JAVA) $(JFLAGS) -cp .:$(CP_JARS):Sndrv4.jar Sndrv4

clean:
	/bin/rm -rf *.java *.class *.jar *.f2j *.jar
