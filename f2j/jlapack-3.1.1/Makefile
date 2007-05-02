include make.def

default:
	@echo ""
	@echo "JLAPACK (version: $(VERSION))"
	@echo ""
	@echo "The possible targets are as follows:"
	@echo ""
	@echo "Translated from Fortran directly to bytecode:"
	@echo "  lib - BLAS and LAPACK libraries"
	@echo "  testers - BLAS and LAPACK test routines"
	@echo "  alltests - build and run the test routines"
	@echo "  libdist - a zipped distribution of the BLAS and LAPACK libraries"
	@echo "  libdist_tgz - a gzipped tar file of the BLAS and LAPACK libraries"
	@echo "  libdist_strict - a zipped distribution of BLAS/LAPACK containing all strictfp versions"
	@echo "  libdist_strict_tgz - a tarball of BLAS/LAPACK containing all strictfp versions"
	@echo "  testers_dist - a zipped distribution of the BLAS and LAPACK test routines"
	@echo "  testers_dist_tgz - a gzipped tar file of the BLAS and LAPACK test routines"
	@echo "  timers - BLAS and LAPACK timing routines"
	@echo "  timers_dist - a zipped distribution of the BLAS and LAPACK timing routines"
	@echo "  timers_dist_tgz - a gzipped tar file of the BLAS and LAPACK timing routines"
	@echo ""
	@echo "Translated from Fortran to Java source:"
	@echo "  javasrc - BLAS and LAPACK libraries"
	@echo "  testers_javasrc - BLAS and LAPACK test routines"
	@echo "  alltests_javasrc - build and run the test routines"
	@echo "  libdist_javasrc - a zipped distribution of the BLAS and LAPACK libraries"
	@echo "  libdist_tgz_javasrc - a gzipped tar file of the BLAS and LAPACK libraries"
	@echo "  libdist_strict_javasrc - a zipped distribution of BLAS/LAPACK containing all strictfp versions"
	@echo "  libdist_strict_tgz_javasrc - a tarball of BLAS/LAPACK containing all strictfp versions"
	@echo "  testers_dist_javasrc - a zipped distribution of the BLAS and LAPACK test routines"
	@echo "  testers_dist_tgz_javasrc - a gzipped tar file of the BLAS and LAPACK test routines"
	@echo ""
	@echo "Documentation:"
	@echo "  javadoc - documentation in javadoc HTML format"
	@echo "  javadoc_dist - zipped distribution of the documentation"
	@echo "  javadoc_dist_tgz - gzipped tar file of the documentation"
	@echo ""
	@echo "Other:"
	@echo "  clean - remove all jar files, generated code, etc."

lib:
	cd $(SRCDIR); $(MAKE)

all:
	cd $(SRCDIR); $(MAKE) all

testers: lib
	cd $(TESTING_DIR); $(MAKE) testers

matgen:
	cd $(MATGEN_DIR); $(MAKE)

smatgen:
	cd $(SMATGEN_DIR); $(MAKE)

timers: lib matgen smatgen
	cd $(TIMING_DIR); $(MAKE) timers

testers_javasrc: javasrc
	cd $(TESTING_DIR); $(MAKE) -f Makefile_javasrc testers

alltests: testers
	cd $(TESTING_DIR); $(MAKE) runtests

alltests_javasrc: testers_javasrc 
	cd $(TESTING_DIR); $(MAKE) -f Makefile_javasrc runtests

javasrc:
	cd $(SRCDIR); $(MAKE) -f Makefile_javasrc

javadoc: lib
	mkdir -p doc
	javadoc -author -sourcepath $(BLAS_OBJ):$(SBLAS_OBJ):$(LAPACK_OBJ):$(SLAPACK_OBJ):$(ERR_OBJ):$(UTIL_F2J_SRC_DIR) -d doc -J-mx256000000 $(UTIL_PACKAGE) $(BLAS_PACKAGE) $(LAPACK_PACKAGE) $(ERR_PACKAGE)

javadoc_dist: javadoc
	/bin/rm -f $(JAVADOC_DIST_ZIP) $(VERSION)/doc
	mkdir -p $(VERSION)
	cd $(VERSION); ln -s ../doc doc
	$(ZIP) -r9 $(JAVADOC_DIST_ZIP) $(VERSION)/doc

javadoc_dist_tgz: javadoc
	/bin/rm -f $(JAVADOC_DIST_TGZ) $(VERSION)/doc
	mkdir -p $(VERSION)
	cd $(VERSION); ln -s ../doc doc
	$(TAR) $(TARFLAGS) - $(VERSION)/doc | $(GZIP) > $(JAVADOC_DIST_TGZ)

libdist: lib libdist_common
	$(ZIP) -r9 $(LIBDIST_ZIP) $(VERSION)

libdist_tgz: lib libdist_common
	$(TAR) $(TARFLAGS) - $(VERSION) | $(GZIP) > $(LIBDIST_TGZ)

libdist_javasrc: javasrc libdist_common
	$(ZIP) -r9 $(LIBDIST_ZIP) $(VERSION)

libdist_tgz_javasrc: javasrc libdist_common
	$(TAR) $(TARFLAGS) - $(VERSION) | $(GZIP) > $(LIBDIST_TGZ)

libdist_common:
	/bin/rm -rf $(VERSION)
	mkdir -p $(VERSION)/examples
	cp README INSTALL CHANGES $(VERSION)
	cp examples/*.java examples/Makefile examples/README $(VERSION)/examples
	$(MAKE) STRICT_DIR="" libdist_copy_jar_files

libdist_strict:
	$(MAKE) LIB_TARGET=lib libdist_strict_common
	$(ZIP) -r9 $(LIBDIST_STRICT_ZIP) $(VERSION)

libdist_strict_tgz:
	$(MAKE) LIB_TARGET=lib libdist_strict_common
	$(TAR) $(TARFLAGS) - $(VERSION) | $(GZIP) > $(LIBDIST_STRICT_TGZ)

libdist_strict_javasrc:
	$(MAKE) LIB_TARGET=javasrc libdist_strict_common
	$(ZIP) -r9 $(LIBDIST_STRICT_ZIP) $(VERSION)

libdist_strict_tgz_javasrc:
	$(MAKE) LIB_TARGET=javasrc libdist_strict_common
	$(TAR) $(TARFLAGS) - $(VERSION) | $(GZIP) > $(LIBDIST_STRICT_TGZ)

libdist_strict_common:
	$(MAKE) clean
	/bin/rm -rf $(VERSION)
	mkdir -p $(VERSION)/plain
	mkdir -p $(VERSION)/strict_both
	mkdir -p $(VERSION)/strict_fp
	mkdir -p $(VERSION)/strict_math_lib
	$(MAKE) STATIC=-fb $(LIB_TARGET)
	$(MAKE) STRICT_DIR=strict_both libdist_copy_jar_files
	cd src; $(MAKE) clean
	$(MAKE) STATIC=-fm $(LIB_TARGET)
	$(MAKE) STRICT_DIR=strict_math_lib libdist_copy_jar_files
	cd src; $(MAKE) clean
	$(MAKE) STATIC=-fs $(LIB_TARGET)
	$(MAKE) STRICT_DIR=strict_fp libdist_copy_jar_files
	cd src; $(MAKE) clean
	$(MAKE) $(LIB_TARGET)
	$(MAKE) STRICT_DIR=plain libdist_copy_jar_files
	cp README INSTALL CHANGES $(VERSION)
	mkdir -p $(VERSION)/examples
	cp examples/*.java examples/Makefile examples/README $(VERSION)/examples

libdist_copy_jar_files:
	cp $(BLAS_DIR)/$(BLAS_JAR) $(BLAS_DIR)/$(SIMPLE_BLAS_JAR) \
           $(SBLAS_DIR)/$(SBLAS_JAR) $(SBLAS_DIR)/$(SIMPLE_SBLAS_JAR) \
           $(LAPACK_DIR)/$(LAPACK_JAR) $(LAPACK_DIR)/$(SIMPLE_LAPACK_JAR) \
           $(SLAPACK_DIR)/$(SLAPACK_JAR) $(SLAPACK_DIR)/$(SIMPLE_SLAPACK_JAR) \
           $(UTIL_DIR)/$(UTIL_JAR) $(ERR_DIR)/$(ERR_JAR) $(VERSION)/$(STRICT_DIR)

testers_dist: testers testers_dist_common
	$(ZIP) -r9 $(TESTERS_DIST_ZIP) $(VERSION)/testing

testers_dist_tgz: testers testers_dist_common
	$(TAR) $(TARFLAGS) - $(VERSION)/testing | $(GZIP) > $(TESTERS_DIST_TGZ)

testers_dist_javasrc: testers_javasrc testers_dist_common
	$(ZIP) -r9 $(TESTERS_DIST_ZIP) $(VERSION)/testing

testers_dist_tgz_javasrc: testers_javasrc testers_dist_common
	$(TAR) $(TARFLAGS) - $(VERSION)/testing | $(GZIP) > $(TESTERS_DIST_TGZ)

testers_dist_common:
	/bin/rm -rf $(VERSION)/testing
	mkdir -p $(VERSION)/testing
	-/bin/cp $(BLAS2TEST_DIR)/*.in $(BLAS3TEST_DIR)/*.in $(SBLAS2TEST_DIR)/*.in \
                 $(SBLAS3TEST_DIR)/*.in $(LINTEST_DIR)/*.in $(SLINTEST_DIR)/*.in \
                 $(EIGTEST_DIR)/*.in $(SEIGTEST_DIR)/*.in $(VERSION)/testing
	-/bin/cp $(DISTDIR_TESTING)/*.sh $(DISTDIR_TESTING)/*.bat \
                 $(DISTDIR_TESTING)/README $(VERSION)/testing
	-ln -s ../../$(BLAS1TEST_DIR)/$(BLAS1TEST_JAR) $(VERSION)/testing/$(BLAS1TEST_JAR)
	-ln -s ../../$(BLAS2TEST_DIR)/$(BLAS2TEST_JAR) $(VERSION)/testing/$(BLAS2TEST_JAR)
	-ln -s ../../$(BLAS3TEST_DIR)/$(BLAS3TEST_JAR) $(VERSION)/testing/$(BLAS3TEST_JAR)
	-ln -s ../../$(SBLAS1TEST_DIR)/$(SBLAS1TEST_JAR) $(VERSION)/testing/$(SBLAS1TEST_JAR)
	-ln -s ../../$(SBLAS2TEST_DIR)/$(SBLAS2TEST_JAR) $(VERSION)/testing/$(SBLAS2TEST_JAR)
	-ln -s ../../$(SBLAS3TEST_DIR)/$(SBLAS3TEST_JAR) $(VERSION)/testing/$(SBLAS3TEST_JAR)
	-ln -s ../../$(MATGEN_DIR)/$(MATGEN_JAR) $(VERSION)/testing/$(MATGEN_JAR)
	-ln -s ../../$(SMATGEN_DIR)/$(SMATGEN_JAR) $(VERSION)/testing/$(SMATGEN_JAR)
	-ln -s ../../$(LINTEST_DIR)/$(LINTEST_JAR) $(VERSION)/testing/$(LINTEST_JAR)
	-ln -s ../../$(SLINTEST_DIR)/$(SLINTEST_JAR) $(VERSION)/testing/$(SLINTEST_JAR)
	-ln -s ../../$(EIGTEST_DIR)/$(EIGTEST_JAR) $(VERSION)/testing/$(EIGTEST_JAR)
	-ln -s ../../$(SEIGTEST_DIR)/$(SEIGTEST_JAR) $(VERSION)/testing/$(SEIGTEST_JAR)

timers_dist: timers timers_dist_common
	$(ZIP) -r9 $(TIMERS_DIST_ZIP) $(VERSION)/timing

timers_dist_tgz: timers timers_dist_common
	$(TAR) $(TARFLAGS) - $(VERSION)/timing | $(GZIP) > $(TIMERS_DIST_TGZ)

timers_dist_common:
	/bin/rm -rf $(VERSION)/timing
	mkdir -p $(VERSION)/timing/input_files_large
	-/bin/cp $(LINTIME_DIR)/*.in $(SLINTIME_DIR)/*.in $(EIGTIME_DIR)/*.in \
                 $(SEIGTIME_DIR)/*.in $(VERSION)/timing
	-/bin/cp $(LINTIME_DIR)/input_files_large/*.in \
                 $(SLINTIME_DIR)/input_files_large/*.in \
                 $(EIGTIME_DIR)/input_files_large/*.in \
                 $(SEIGTIME_DIR)/input_files_large/*.in \
                 $(VERSION)/timing/input_files_large
	-/bin/cp $(DISTDIR_TIMING)/*.sh $(DISTDIR_TIMING)/*.bat \
                 $(DISTDIR_TIMING)/README $(VERSION)/timing
	-ln -s ../../$(MATGEN_DIR)/$(MATGEN_JAR) $(VERSION)/timing/$(MATGEN_JAR)
	-ln -s ../../$(SMATGEN_DIR)/$(SMATGEN_JAR) $(VERSION)/timing/$(SMATGEN_JAR)
	-ln -s ../../$(LINTIME_DIR)/$(LINTIME_JAR) $(VERSION)/timing/$(LINTIME_JAR)
	-ln -s ../../$(SLINTIME_DIR)/$(SLINTIME_JAR) $(VERSION)/timing/$(SLINTIME_JAR)
	-ln -s ../../$(EIGTIME_DIR)/$(EIGTIME_JAR) $(VERSION)/timing/$(EIGTIME_JAR)
	-ln -s ../../$(SEIGTIME_DIR)/$(SEIGTIME_JAR) $(VERSION)/timing/$(SEIGTIME_JAR)

examples:
	cd examples; $(MAKE)

clean:
	cd src; $(MAKE) clean
	cd examples; $(MAKE) clean
	/bin/rm -rf doc $(VERSION) $(LIBDIST_ZIP) $(LIBDIST_TGZ) \
		$(TESTERS_DIST_ZIP) $(TESTERS_DIST_TGZ) $(JAVADOC_DIST_ZIP) \
		$(JAVADOC_DIST_TGZ) $(TIMERS_DIST_ZIP) $(TIMERS_DIST_TGZ) \
                $(LIBDIST_STRICT_ZIP) $(LIBDIST_STRICT_TGZ)
