include ../make.def

BLAS_CLASSPATH=.:../$(UTIL_DIR)/$(UTIL_JAR):../$(BLAS_DIR)/$(BLAS_JAR)
SIMPLE_BLAS_CLASSPATH=$(BLAS_CLASSPATH):../$(BLAS_DIR)/$(SIMPLE_BLAS_JAR)
LAPACK_CLASSPATH=$(BLAS_CLASSPATH):../$(LAPACK_DIR)/$(LAPACK_JAR):$(ERR_DIR)/$(ERR_JAR)
SIMPLE_LAPACK_CLASSPATH=$(LAPACK_CLASSPATH):../$(LAPACK_DIR)/$(SIMPLE_LAPACK_JAR)

all: DdotTest.class SimpleDdotTest.class DgesvdTest.class DlaruvTest.class \
     DsygvTest.class SimpleDsygvTest.class

DdotTest.class: DdotTest.java
	$(JAVAC) -classpath $(BLAS_CLASSPATH) DdotTest.java
SimpleDdotTest.class: SimpleDdotTest.java
	$(JAVAC) -classpath $(SIMPLE_BLAS_CLASSPATH) SimpleDdotTest.java
DlaruvTest.class: DlaruvTest.java
	$(JAVAC) -classpath $(LAPACK_CLASSPATH) DlaruvTest.java
DsygvTest.class: DsygvTest.java
	$(JAVAC) -classpath $(LAPACK_CLASSPATH) DsygvTest.java
SimpleDsygvTest.class: SimpleDsygvTest.java
	$(JAVAC) -classpath $(SIMPLE_LAPACK_CLASSPATH) SimpleDsygvTest.java
DgesvdTest.class: DgesvdTest.java
	$(JAVAC) -classpath $(LAPACK_CLASSPATH) DgesvdTest.java
SimpleDgesvdTest.class: SimpleDgesvdTest.java
	$(JAVAC) -classpath $(LAPACK_CLASSPATH) SimpleDgesvdTest.java

clean:
	/bin/rm -f *.class
