.PHONY:	runtimers eigtimer lintimer clean

ROOT=../..

include $(ROOT)/make.def

timers:	eigtimer lintimer seigtimer slintimer

eigtimer: $(ROOT)/$(EIGTIME_IDX)
seigtimer: $(ROOT)/$(SEIGTIME_IDX)
lintimer: $(ROOT)/$(LINTIME_IDX)
slintimer: $(ROOT)/$(SLINTIME_IDX)

$(ROOT)/$(EIGTIME_IDX):
	cd eig;$(MAKE)
$(ROOT)/$(SEIGTIME_IDX):
	cd seig;$(MAKE)
$(ROOT)/$(LINTIME_IDX):
	cd lin;$(MAKE)
$(ROOT)/$(SLINTIME_IDX):
	cd slin;$(MAKE)

runtimers:
	cd eig;$(MAKE) runtimer
	cd lin;$(MAKE) runtimer
	cd seig;$(MAKE) runtimer
	cd slin;$(MAKE) runtimer

clean:
	cd eig;$(MAKE) clean
	cd lin;$(MAKE) clean
	cd seig;$(MAKE) clean
	cd slin;$(MAKE) clean
