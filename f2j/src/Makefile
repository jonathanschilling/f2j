.PHONY: lib all testers err blas lapack util

ROOT=..
include $(ROOT)/make.def

lib:    util err blas lapack

javasrc:
	$(MAKE) -f Makefile_javasrc

all:    lib testers
	
testers:        err blas lapack
	cd $(ROOT)/$(TESTING_DIR);$(MAKE)

timers:        err blas lapack
	cd $(ROOT)/$(TIMING_DIR);$(MAKE)

err:
	cd $(ROOT)/$(ERR_DIR);$(MAKE)

blas:   util err
	cd $(ROOT)/$(BLAS_DIR);$(MAKE)

lapack: util err
	cd $(ROOT)/$(LAPACK_DIR);$(MAKE)

util:
	cd $(ROOT)/$(UTIL_DIR);$(MAKE)

clean:
	cd $(ROOT)/$(ERR_DIR);$(MAKE) clean
	cd $(ROOT)/$(BLAS_DIR);$(MAKE) clean
	cd $(ROOT)/$(LAPACK_DIR);$(MAKE) clean
	cd $(ROOT)/$(TESTING_DIR);$(MAKE) clean
	cd $(ROOT)/$(TIMING_DIR);$(MAKE) clean
	cd $(ROOT)/$(UTIL_DIR);$(MAKE) clean
