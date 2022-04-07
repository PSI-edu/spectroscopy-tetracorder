# @(#) makefile 2.17@(#)

SHELL=/bin/sh

OBJ= \
gfplot.o \
plot.o \
ptplot.o \
scale.o \
xplta.o

../plot.o:	$(OBJ)	
	ld $(GF2_LFLAGS) -r -o ../plot.o $(OBJ)

gfplot.o:		gfplot.r
	$(RF) $(RFLAGS) gfplot.r > gfplot.f
	$(F77) $(GF2_FFLAGS) -c gfplot.f
	rm -f gfplot.f

plot.o:		plot.r
	$(RF) $(RFLAGS) plot.r > plot.f
	$(F77) $(GF2_FFLAGS) -c plot.f
	rm -f plot.f

ptplot.o:		ptplot.r
	$(RF) $(RFLAGS) ptplot.r > ptplot.f
	$(F77) $(GF2_FFLAGS) -c ptplot.f
	rm -f ptplot.f

scale.o:		scale.r
	$(RF) $(RFLAGS) scale.r > scale.f
	$(F77) $(GF2_FFLAGS) -c scale.f
	rm -f scale.f

xplta.o:		xplta.r
	$(RF) $(RFLAGS) xplta.r > xplta.f
	$(F77) $(GF2_FFLAGS) -c xplta.f
	rm -f xplta.f

