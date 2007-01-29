include make.def

default:
	@echo "The possible targets are as follows:"
	@echo ""
	@echo "Translated from Fortran directly to bytecode:"
	@echo "  lib - BLAS and LAPACK libraries"
	@echo "  testers - BLAS and LAPACK test routines"
	@echo "  alltests - build and run the test routines"
	@echo "  libdist - a zipped distribution of the BLAS and LAPACK libraries"
	@echo "  libdist_tgz - a gzipped tar file of the BLAS and LAPACK libraries"
	@echo "  testers_dist - a zipped distribution of the BLAS and LAPACK test routines"
	@echo "  testers_dist_tgz - a gzipped tar file of the BLAS and LAPACK test routines"
	@echo "  timers_dist - a zipped distribution of the BLAS and LAPACK timing routines"
	@echo "  timers_dist_tgz - a gzipped tar file of the BLAS and LAPACK timing routines"
	@echo ""
	@echo "Translated from Fortran to Java source:"
	@echo "  javasrc - BLAS and LAPACK libraries"
	@echo "  testers_javasrc - BLAS and LAPACK test routines"
	@echo "  alltests_javasrc - build and run the test routines"
	@echo "  libdist_javasrc - a zipped distribution of the BLAS and LAPACK libraries"
	@echo "  libdist_tgz_javasrc - a gzipped tar file of the BLAS and LAPACK libraries"
	@echo "  testers_dist_javasrc - a zipped distribution of the BLAS and LAPACK test routines"
	@echo "  testers_dist_tgz_javasrc - a gzipped tar file of the BLAS and LAPACK test routines"
	@echo ""
	@echo "Documentation:"
	@echo "  javadoc - documentation in javadoc format"
	@echo "  javadoc_dist - zipped distribution of the documentation"
	@echo "  javadoc_dist_tgz - gzipped tar file of the documentation"

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
	mkdir -p $(VERSION)
	-ln -s ../$(BLAS_DIR)/$(BLAS_JAR) $(VERSION)/$(BLAS_JAR)
	-ln -s ../$(BLAS_DIR)/$(SIMPLE_BLAS_JAR) $(VERSION)/$(SIMPLE_BLAS_JAR)
	-ln -s ../$(SBLAS_DIR)/$(SBLAS_JAR) $(VERSION)/$(SBLAS_JAR)
	-ln -s ../$(SBLAS_DIR)/$(SIMPLE_SBLAS_JAR) $(VERSION)/$(SIMPLE_SBLAS_JAR)
	-ln -s ../$(LAPACK_DIR)/$(LAPACK_JAR) $(VERSION)/$(LAPACK_JAR)
	-ln -s ../$(LAPACK_DIR)/$(SIMPLE_LAPACK_JAR) $(VERSION)/$(SIMPLE_LAPACK_JAR)
	-ln -s ../$(SLAPACK_DIR)/$(SLAPACK_JAR) $(VERSION)/$(SLAPACK_JAR)
	-ln -s ../$(SLAPACK_DIR)/$(SIMPLE_SLAPACK_JAR) $(VERSION)/$(SIMPLE_SLAPACK_JAR)
	-ln -s ../$(UTIL_DIR)/$(UTIL_JAR) $(VERSION)/$(UTIL_JAR)
	-ln -s ../$(ERR_DIR)/$(ERR_JAR) $(VERSION)/$(ERR_JAR)
	-ln -s ../README $(VERSION)/README
	-ln -s ../INSTALL $(VERSION)/INSTALL
	-ln -s ../CHANGES $(VERSION)/CHANGES
	mkdir -p $(VERSION)/examples
	cp examples/*.java examples/Makefile examples/README $(VERSION)/examples

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
	-/bin/cp $(BLAS2TEST_INPUT) $(VERSION)/testing
	-/bin/cp $(BLAS3TEST_INPUT) $(VERSION)/testing
	-/bin/cp $(SBLAS2TEST_INPUT) $(VERSION)/testing
	-/bin/cp $(SBLAS3TEST_INPUT) $(VERSION)/testing
	-/bin/cp $(LINTEST_INPUT) $(VERSION)/testing
	-/bin/cp $(SLINTEST_INPUT) $(VERSION)/testing
	-/bin/cp $(EIGTEST_INPUT) $(VERSION)/testing
	-/bin/cp $(SEIGTEST_INPUT) $(VERSION)/testing
	-/bin/cp $(DISTDIR_TESTING)/*.sh $(DISTDIR_TESTING)/*.bat $(DISTDIR_TESTING)/README $(VERSION)/testing
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
	mkdir -p $(VERSION)/timing
	-/bin/cp $(LINTIME_INPUT) $(VERSION)/timing
	-/bin/cp $(SLINTIME_INPUT) $(VERSION)/timing
	-/bin/cp $(EIGTIME_INPUT) $(VERSION)/timing
	-/bin/cp $(SEIGTIME_INPUT) $(VERSION)/timing
	-/bin/cp $(DISTDIR_TIMING)/*.sh $(DISTDIR_TIMING)/*.bat $(DISTDIR_TIMING)/README $(VERSION)/timing
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
		$(JAVADOC_DIST_TGZ)
