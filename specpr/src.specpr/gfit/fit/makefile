# @(#) makefile 2.17@(#)

SHELL=/bin/sh

OBJ= \
bas1.o \
f.o \
matinv.o

OBJ2= \
fit.o

../fits.o:	$(OBJ)	
	ld $(GF2_LFLAGS) -r -o ../fits.o $(OBJ)

bas1.o:		bas1.r
	$(RF) $(RFLAGS) bas1.r > bas1.f
	$(F77) $(GF2_FFLAGS) -c bas1.f
	rm -f bas1.f

f.o:		f.r
	$(RF) $(RFLAGS) f.r > f.f
	$(F77) $(GF2_FFLAGS) -c f.f
	rm -f f.f

matinv.o:		matinv.r
	$(RF) $(RFLAGS) matinv.r > matinv.f
	$(F77) $(GF2_FFLAGS) -c matinv.f
	rm -f matinv.f

fit.o:		fit.r
	$(RF) $(RFLAGS) fit.r > fit.f
	$(F77) $(GF2_FLAGS) -c fit.f
	rm -f fit.f
