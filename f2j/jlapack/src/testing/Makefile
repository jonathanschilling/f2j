.PHONY:	blas1 blas2 blas3 runtests clean

ROOT=../..

include $(ROOT)/make.def

testers:	blas1 blas2 blas3 eig lin

blas1:	$(ROOT)/$(BLAS1TEST_IDX)
blas2:	$(ROOT)/$(BLAS2TEST_IDX)
blas3:	$(ROOT)/$(BLAS3TEST_IDX)
eig:	$(ROOT)/$(EIGTEST_IDX)
lin:	$(ROOT)/$(LINTEST_IDX)

$(ROOT)/$(BLAS1TEST_IDX):
	cd blas1;$(MAKE)
$(ROOT)/$(BLAS2TEST_IDX):
	cd blas2;$(MAKE)
$(ROOT)/$(BLAS3TEST_IDX):
	cd blas3;$(MAKE)
$(ROOT)/$(EIGTEST_IDX):
	cd eig;$(MAKE)
$(ROOT)/$(LINTEST_IDX):
	cd lin;$(MAKE)

runtests:	blastest lintest eigtest

blastest:
	cd blas1;$(MAKE) runtest
	cd blas2;$(MAKE) runtest
	cd blas3;$(MAKE) runtest

eigtest:
	cd eig;$(MAKE) runtest

lintest:
	cd lin;$(MAKE) runtest
	
clean:
	cd blas1;$(MAKE) clean
	cd blas2;$(MAKE) clean
	cd blas3;$(MAKE) clean
	cd eig;$(MAKE) clean
	cd lin;$(MAKE) clean
	cd matgen;$(MAKE) clean
