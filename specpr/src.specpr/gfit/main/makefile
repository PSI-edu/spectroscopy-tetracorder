# @(#) makefile 2.17@(#)

SHELL=/bin/sh

gfit.o:		gfit.r
	$(RF) $(RFLAGS) gfit.r > gfit.f
	$(F77) $(GF2_FFLAGS) -c gfit.f
	rm -f gfit.f

