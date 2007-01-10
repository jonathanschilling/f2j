.PHONY:	blas1 blas2 blas3 runtests clean sblas1 sblas2 sblas3 eig lin seig slin

ROOT=../..

include $(ROOT)/make.def

testers:	blas1 blas2 blas3 eig lin sblas1 sblas2 sblas3 seig slin

blas1:	$(ROOT)/$(BLAS1TEST_IDX)
blas2:	$(ROOT)/$(BLAS2TEST_IDX)
blas3:	$(ROOT)/$(BLAS3TEST_IDX)
eig:	$(ROOT)/$(EIGTEST_IDX)
lin:	$(ROOT)/$(LINTEST_IDX)

sblas1:  $(ROOT)/$(SBLAS1TEST_IDX)
sblas2:  $(ROOT)/$(SBLAS2TEST_IDX)
sblas3:  $(ROOT)/$(SBLAS3TEST_IDX)
seig:    $(ROOT)/$(SEIGTEST_IDX)
slin:    $(ROOT)/$(SLINTEST_IDX)


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

$(ROOT)/$(SBLAS1TEST_IDX):
	cd sblas1;$(MAKE)
$(ROOT)/$(SBLAS2TEST_IDX):
	cd sblas2;$(MAKE)
$(ROOT)/$(SBLAS3TEST_IDX):
	cd sblas3;$(MAKE)
$(ROOT)/$(SEIGTEST_IDX):
	cd seig;$(MAKE)
$(ROOT)/$(SLINTEST_IDX):
	cd slin;$(MAKE)

runtests:	blastest lintest eigtest sblastest slintest seigtest

blastest:
	cd blas1;$(MAKE) runtest
	cd blas2;$(MAKE) runtest
	cd blas3;$(MAKE) runtest

sblastest:
	cd sblas1;$(MAKE) runtest
	cd sblas2;$(MAKE) runtest
	cd sblas3;$(MAKE) runtest

eigtest:
	cd eig;$(MAKE) runtest

seigtest:
	cd seig;$(MAKE) runtest

lintest:
	cd lin;$(MAKE) runtest

slintest:
	cd slin;$(MAKE) runtest
	
clean:
	cd blas1;$(MAKE) clean
	cd blas2;$(MAKE) clean
	cd blas3;$(MAKE) clean
	cd sblas1;$(MAKE) clean
	cd sblas2;$(MAKE) clean
	cd sblas3;$(MAKE) clean
	cd eig;$(MAKE) clean
	cd seig;$(MAKE) clean
	cd lin;$(MAKE) clean
	cd slin;$(MAKE) clean
	cd matgen;$(MAKE) clean
	cd smatgen;$(MAKE) clean
