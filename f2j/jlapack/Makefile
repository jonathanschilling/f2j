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
	@echo "  doc_dist - zipped distribution of the documentation"
	@echo "  doc_dist_tgz - gzipped tar file of the documentation"

lib:
	cd $(SRCDIR); $(MAKE)

all:
	cd $(SRCDIR); $(MAKE) all

testers: lib
	cd $(TESTING_DIR); $(MAKE) testers

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
	javadoc -sourcepath $(BLAS_OBJ):$(LAPACK_OBJ):$(ERR_OBJ):$(UTIL_DIR) -d doc -J-mx48000000 $(UTIL_PACKAGE) $(BLAS_PACKAGE) $(LAPACK_PACKAGE) $(ERR_PACKAGE)

doc_dist: javadoc
	/bin/rm -f $(DOC_DIST_ZIP) $(VERSION)/doc
	mkdir -p $(VERSION)
	cd $(VERSION); ln -s ../doc doc
	$(ZIP) -r9 $(DOC_DIST_ZIP) $(VERSION)/doc

doc_dist_tgz: javadoc
	/bin/rm -f $(DOC_DIST_TGZ) $(VERSION)/doc
	mkdir -p $(VERSION)
	cd $(VERSION); ln -s ../doc doc
	$(TAR) $(TARFLAGS) - $(VERSION)/doc | $(GZIP) > $(DOC_DIST_TGZ)

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
	-ln -s ../$(LAPACK_DIR)/$(LAPACK_JAR) $(VERSION)/$(LAPACK_JAR)
	-ln -s ../$(LAPACK_DIR)/$(SIMPLE_LAPACK_JAR) $(VERSION)/$(SIMPLE_LAPACK_JAR)
	-ln -s ../$(UTIL_DIR)/$(UTIL_JAR) $(VERSION)/$(UTIL_JAR)
	-ln -s ../$(ERR_DIR)/$(ERR_JAR) $(VERSION)/$(ERR_JAR)
	-ln -s ../README $(VERSION)/README
	-ln -s ../INSTALL $(VERSION)/INSTALL
	-ln -s ../CHANGES $(VERSION)/CHANGES
	-ln -s ../DdotTest.java $(VERSION)/DdotTest.java
	-ln -s ../DlaruvTest.java $(VERSION)/DlaruvTest.java
	-ln -s ../SimpleDdotTest.java $(VERSION)/SimpleDdotTest.java

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
	-/bin/cp $(LINTEST_INPUT) $(VERSION)/testing
	-/bin/cp $(EIGTEST_INPUT) $(VERSION)/testing
	-ln -s ../../$(BLAS1TEST_DIR)/$(BLAS1TEST_JAR) $(VERSION)/testing/$(BLAS1TEST_JAR)
	-ln -s ../../$(BLAS2TEST_DIR)/$(BLAS2TEST_JAR) $(VERSION)/testing/$(BLAS2TEST_JAR)
	-ln -s ../../$(BLAS3TEST_DIR)/$(BLAS3TEST_JAR) $(VERSION)/testing/$(BLAS3TEST_JAR)
	-ln -s ../../$(MATGEN_DIR)/$(MATGEN_JAR) $(VERSION)/testing/$(MATGEN_JAR)
	-ln -s ../../$(LINTEST_DIR)/$(LINTEST_JAR) $(VERSION)/testing/$(LINTEST_JAR)
	-ln -s ../../$(EIGTEST_DIR)/$(EIGTEST_JAR) $(VERSION)/testing/$(EIGTEST_JAR)
	-ln -s ../../$(DISTDIR)/$(BLAS1TEST_SH) $(VERSION)/testing/$(BLAS1TEST_SH)
	-ln -s ../../$(DISTDIR)/$(BLAS2TEST_SH) $(VERSION)/testing/$(BLAS2TEST_SH)
	-ln -s ../../$(DISTDIR)/$(BLAS3TEST_SH) $(VERSION)/testing/$(BLAS3TEST_SH)
	-ln -s ../../$(DISTDIR)/$(LINTEST_SH) $(VERSION)/testing/$(LINTEST_SH)
	-ln -s ../../$(DISTDIR)/$(EIGTEST_SH) $(VERSION)/testing/$(EIGTEST_SH)
	-ln -s ../../$(DISTDIR)/$(BLAS1TEST_BAT) $(VERSION)/testing/$(BLAS1TEST_BAT)
	-ln -s ../../$(DISTDIR)/$(BLAS2TEST_BAT) $(VERSION)/testing/$(BLAS2TEST_BAT)
	-ln -s ../../$(DISTDIR)/$(BLAS3TEST_BAT) $(VERSION)/testing/$(BLAS3TEST_BAT)
	-ln -s ../../$(DISTDIR)/$(LINTEST_BAT) $(VERSION)/testing/$(LINTEST_BAT)
	-ln -s ../../$(DISTDIR)/$(EIGTEST_BAT) $(VERSION)/testing/$(EIGTEST_BAT)
	-ln -s ../../$(DISTDIR)/README $(VERSION)/testing/README

examples:
	cd examples; $(MAKE)

clean:
	cd src; $(MAKE) clean
	cd examples; $(MAKE) clean
	/bin/rm -rf doc $(VERSION) $(LIBDIST_ZIP) $(LIBDIST_TGZ) \
		$(TESTERS_DIST_ZIP) $(TESTERS_DIST_TGZ) $(DOC_DIST_ZIP) \
		$(DOC_DIST_TGZ)
