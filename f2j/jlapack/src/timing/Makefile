.PHONY:	runtimers eigtimer lintimer clean

ROOT=../..

include $(ROOT)/make.def

timers:	eigtimer lintimer

eigtimer:	eig/$(OUTDIR)/Eigtime.f2j
lintimer:	lin/$(OUTDIR)/Lintime.f2j

eig/$(OUTDIR)/Eigtime.f2j:
	cd eig;$(MAKE)
lin/$(OUTDIR)/Lintime.f2j:
	cd lin;$(MAKE)

runtimers:
	cd eig;$(MAKE) runtimer
	cd lin;$(MAKE) runtimer

clean:
	cd eig;$(MAKE) clean
	cd lin;$(MAKE) clean
