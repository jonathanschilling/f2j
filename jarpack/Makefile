include make.def

DIRS=ERR BLAS LAPACK UTIL SRC EXAMPLES
JAR_TEMP=jar_temp

arpack_lib:
	@( \
	for f in $(DIRS); \
	do \
		cd $$f; \
		echo Making in $$f; \
		$(MAKE); \
		cd ..; \
	done );

javadoc: arpack_lib
	mkdir -p doc
	javadoc -author -sourcepath $(BLAS_OBJ):$(LAPACK_OBJ):$(ERR_OBJ):$(ARPACK_UTIL_OBJ):$(ARPACK_OBJ) -d doc -J-mx256000000 $(ARPACK_UTIL_PACKAGE) $(BLAS_PACKAGE) $(LAPACK_PACKAGE) $(ERR_PACKAGE) $(ARPACK_PACKAGE)

combined_jar: arpack_lib
	/bin/rm -rf $(JAR_TEMP)
	mkdir $(JAR_TEMP)
	cd $(JAR_TEMP); jar xvf ../f2jutil.jar
	cd $(JAR_TEMP); jar xvf ../$(ERR_DIR)/$(ERR_JAR)
	cd $(JAR_TEMP); jar xvf ../$(BLAS_DIR)/$(BLAS_JAR)
	cd $(JAR_TEMP); jar xvf ../$(LAPACK_DIR)/$(LAPACK_JAR)
	cd $(JAR_TEMP); jar xvf ../$(ARPACK_UTIL_DIR)/$(ARPACK_UTIL_JAR)
	cd $(JAR_TEMP); jar xvf ../$(ARPACK_DIR)/$(ARPACK_JAR)
	cd $(JAR_TEMP); jar cvf $(COMBINED_JAR) org
	
dist: combined_jar
	/bin/rm -rf $(VERSION)
	mkdir -p $(VERSION)
	/bin/cp dist/README README.orig INSTALL CHANGES f2jutil.jar $(VERSION)
	/bin/cp -r $(EXAMPLES_DIR) $(VERSION)/EXAMPLES
	/bin/cp $(BLAS_DIR)/$(BLAS_JAR) $(LAPACK_DIR)/$(LAPACK_JAR) \
           $(ARPACK_UTIL_DIR)/$(ARPACK_UTIL_JAR) $(ERR_DIR)/$(ERR_JAR) \
           $(ARPACK_DIR)/$(ARPACK_JAR) $(JAR_TEMP)/$(COMBINED_JAR) $(VERSION)
	/bin/rm -rf `find $(VERSION) -name "CVS"`
	/bin/rm -rf $(VERSION)/EXAMPLES/COMPLEX
	/bin/rm -rf `find $(VERSION)/EXAMPLES -name "Makefile"`
	$(ZIP) -r9 $(LIBDIST_ZIP) $(VERSION)
	$(TAR) $(TARFLAGS) - $(VERSION) | $(GZIP) > $(LIBDIST_TGZ)

clean:
	@( \
	for f in $(DIRS); \
	do \
		cd $$f; \
		echo Making clean in $$f; \
		$(MAKE) clean; \
		cd ..; \
	done );
	/bin/rm -rf $(VERSION) $(LIBDIST_ZIP) $(LIBDIST_TGZ) $(JAR_TEMP)
